#lang plait
(require "utilities.rkt")
(require "error.rkt")
(require "io.rkt")
(require "datatypes.rkt")
(require "s-exp-of-state.rkt")
(require (typed-in racket
                   [list->vector : ((Listof 'a) -> (Vectorof 'a))]
                   [vector->list : ((Vectorof 'a) -> (Listof 'a))]
                   [vector-map : (('a -> 'b) (Vectorof 'a) -> (Vectorof 'b))]
                   [remove-duplicates : ((Listof 'a) -> (Listof 'a))]))


(define-type Operator
  (op-prim [name : PrimitiveOp])
  (op-fun [env : Env] [arg* : (Listof Id)] [def* : (Listof (Id * Term))] [body : Term]))


(define (eval check pict-state (e : Program))
  :
  (Listof Obs)

  (local (
          (define (compile [program : Program]): Term
            (let* ([def* (fst program)]
                   [exp* (snd program)]
                   [exp* (map compile-e exp*)]
                   [exp* (map t-show exp*)]
                   [prelude*&result (type-case (Dec ((Listof Term) * Term) ()) (get-last exp*)
                                      [(no _) (values exp* (term-of-c (c-void)))]
                                      [(yes prelude*&result)
                                       prelude*&result])]
                   [prelude* (fst prelude*&result)]
                   [result (snd prelude*&result)])
              (global-letrec-of-def*-1 (fst program) (maybe-begin prelude* result))))
          (define (compile-e [e : Expr]) : Term
            (type-case Expr e
              [(e-con c)
               (term-of-c c)]
              [(e-var x)
               (t-var x)]
              [(e-fun arg* def* prelude* result)
               (compile-fun (none) arg* def* prelude* result)]
              [(e-app fun arg*)
               (t-app (compile-e fun) (map compile-e arg*))]
              [(e-let bind* def* prelude* result)
               (t-let (map compile-bind bind*) (letrec-of-def* def* prelude* result))]
              [(e-let* bind* def* prelude* result)
               (compile-let* (map compile-bind bind*)
                             (letrec-of-def* def* prelude* result))]
              [(e-letrec bind* def* prelude* result)
               (t-letrec (map compile-bind bind*)
                         (letrec-of-def* def* prelude* result))]
              [(e-set! var val)
               (t-set! var (compile-e val))]
              [(e-begin prelude* result)
               (compile-begin prelude* result)]
              [(e-if cnd thn els)
               (t-if (compile-e cnd)
                     (compile-e thn)
                     (compile-e els))]))
          (define (compile-def def) : (Id * Term)
            (type-case Def def
              ((d-var x e)
               (values x (compile-e e)))
              ((d-fun fun arg* def* prelude* result)
               (values fun (compile-fun (some fun) arg* def* prelude* result)))))
          (define (compile-fun name arg* def* prelude* result)
            (t-fun name arg* (map compile-def def*) (compile-begin prelude* result)))
          (define (compile-bind bind)
            (values (fst bind)
                    (compile-e (snd bind))))
          (define (letrec-of-def* def* prelude* result)
            (letrec-of-def*-1 def* (compile-begin prelude* result)))
          (define (global-letrec-of-def*-1 def* body)
            (t-letrec (map compile-def def*)
                      body))
          (define (letrec-of-def*-1 def* body)
            (if (empty? def*)
                body
                (t-letrec (map compile-def def*)
                          body)))
          (define (compile-let* bind* body)
            (ind-List bind*
                      body
                      (λ (IH bind)
                        (t-let (list bind) IH))))
          (define (compile-begin prelude* result)
            (maybe-begin (map compile-e prelude*)
                         (compile-e result)))
          (define (maybe-begin prelude* result)
            (if (empty? prelude*)
                result
                (t-begin prelude* result)))

          (define (truthy? [v : Val])
            (type-case Val v
              [(v-bool b)
               b]
              [else
               #t]))
          (define (term-of-c c) : Term
            (type-case Constant c
              ((c-void)
               (t-quote (v-void)))
              ((c-str it)
               (t-quote (v-str it)))
              ((c-num it)
               (t-quote (v-num it)))
              ((c-bool it)
               (t-quote (v-bool it)))
              ((c-vec it)
               (t-app (t-quote (v-prim (po-ivec))) (map term-of-c it)))
              ((c-list it)
               (t-app (t-quote (v-prim (po-list))) (map term-of-c it)))))


          (define (apply-stack [stack : Stack] v)
            (type-case (Listof Ctx) stack
              (empty
               v)
              ((cons sf0 stack)
               (local ((define-values (env ectx ann) sf0))
                 (apply-k v env ectx stack)))))
          (define (apply-k v env ectx [stack : Stack])
            (type-case (Listof ECFrame) ectx
              [empty
               (apply-stack stack v)]
              ((cons f ectx)
               (type-case ECFrame f
                 ((F-begin e* e)
                  (interp-begin e* e env ectx stack))
                 ((F-app v* e*)
                  (interp-app (append v* (list v)) e* env ectx stack))
                 ((F-show!)
                  (let ((_ (show! v))
                        (v (v-void)))
                    (apply-k v env ectx stack)))
                 ((F-let xv* x xe* body)
                  (interp-let (append xv* (list (pair x v))) xe* body env ectx stack))
                 ((F-letrec-1 x xe* body)
                  (let ((_ (env-set! env x v)))
                    (interp-letrec-1 xe* body env ectx stack)))
                 ((F-fun-call x xe* body)
                  (let ((_ (env-set! env x v)))
                    (interp-fun-call xe* body env ectx stack)))
                 ((F-if thn els)
                  (if (truthy? v)
                      (let ((e thn))
                        (interp e env ectx stack))
                      (let* ((e els))
                        (interp e env ectx stack))))
                 ((F-set! var)
                  (let ((_ (env-set! env var v))
                        (v (v-void)))
                    (apply-k v env ectx stack)))
                 ))))
          (define (display-state [e : Term] [env : Env] ectx [stack : Stack])
            (pict-state (s-exp-of-e e) (s-exp-of-env env) (s-exp-of-ectx ectx) (s-exp-of-stack stack) (s-exp-of-heap the-heap)))
          (define (simple? e)
            (type-case Term e
              ((t-var _) #t)
              ((t-quote _) #t)
              (else #f)))
          (define (interp e [env : Env] ectx stack)
            :
            (Result Val)
            (begin
              (type-case
                  Term
                e
                ((t-quote v) (apply-k v env ectx stack))
                ((t-var x)
                 (begin
                   (display-state e env ectx stack)
                   (let ([v (env-lookup env x)])
                     (apply-k v env ectx stack))))
                ((t-fun name arg* def* body)
                 (let ((v (v-fun name env arg* def* body))) (apply-k v env ectx stack)))
                ((t-app fun arg*) (interp-app (list) (cons fun arg*) env ectx stack))
                ((t-let bind* body) (interp-let (list) bind* body env ectx stack))
                ((t-letrec-1 bind* body) (interp-letrec-1 bind* body env ectx stack))
                ((t-fun-call bind* body) (interp-fun-call bind* body env ectx stack))
                ((t-letrec bind* body)
                 (let ([stack (cons (values env ectx (ca-letrec)) stack)])
                   (let ((ectx (list)))
                     (let ((var* (map var-of-bind bind*)))
                       (let ((env (env-declare env var*)))
                         (let ([e (t-letrec-1 bind* body)])
                           (interp e env ectx stack)))))))
                ((t-set! var val)
                 (let ((e val))
                   (let ((ectx (cons (F-set! var) ectx)))
                     (interp e env ectx stack))))
                ((t-begin prelude* result)
                 (interp-begin prelude* result env ectx stack))
                ((t-if cnd thn els)
                 (let ((e cnd))
                   (let ((ectx (cons (F-if thn els) ectx)))
                     (interp e env ectx stack))))
                ((t-show val)
                 (let ((e val))
                   (let ((ectx (cons (F-show!) ectx)))
                     (interp e env ectx stack)))))))
          (define (interp-app v* e* env ectx stack)
            (type-case
                (Listof Term)
              e*
              (empty
               (type-case
                   (Listof Val)
                 v*
                 (empty (raise (exn-internal 'interpter "")))
                 ((cons fun arg*) (interp-beta fun arg* env ectx stack))))
              ((cons e e*)
               (let ((ectx (cons (F-app v* e*) ectx)))
                 (interp e env ectx stack)))))
          (define (interp-begin prelude* result env ectx stack)
            (type-case
                (Listof Term)
              prelude*
              (empty
               (let ([e result])
                 (interp e env ectx stack)))
              ((cons e prelude*)
               (let ((ectx (cons (F-begin prelude* result) ectx)))
                 (interp e env ectx stack)))))
          (define (interp-let xv* xe* body env ectx stack)
            (type-case
                (Listof (Id * Term))
              xe*
              (empty
               (let ([stack (cons (values env ectx (ca-let)) stack)])
                 (let ((env (env-extend env xv*)))
                   (let ((ectx (list)))
                     (let ((e body))
                       (interp e env ectx stack))))))
              ((cons ⟨x×e⟩ xe*)
               (let ((x (fst ⟨x×e⟩)))
                 (let ((e (snd ⟨x×e⟩)))
                   (let ((ectx (cons (F-let xv* x xe* body) ectx)))
                     (interp e env ectx stack)))))))
          (define (interp-letrec-1 xe* body env ectx stack)
            (type-case
                (Listof (Id * Term))
              xe*
              (empty
               (let ([e body])
                 (interp e env ectx stack)))
              ((cons xe xe*)
               (let ((x (fst xe)))
                 (let ((e (snd xe)))
                   (let ((ectx (cons (F-letrec-1 x xe* body) ectx)))
                     (interp e env ectx stack)))))))
          (define (interp-fun-call xe* body env ectx stack)
            (type-case
                (Listof (Id * Term))
              xe*
              (empty
               (let ([e body])
                 (interp e env ectx stack)))
              ((cons xe xe*)
               (let ((x (fst xe)))
                 (let ((e (snd xe)))
                   (let ((ectx (cons (F-fun-call x xe* body) ectx)))
                     (interp e env ectx stack)))))))
          (define output-buffer (box '()))
          (define (reset-output-buffer!) (set-box! output-buffer '()))
          (define (output! o) (set-box! output-buffer (append (unbox output-buffer) o)))
          (define (show! val) (output! (list (obs-of-val val))))
          (define (obs-of-val v)
            (type-case
                Val
              v
              ((v-str it) (o-con (c-str it)))
              ((v-num it) (o-con (c-num it)))
              ((v-bool it) (o-con (c-bool it)))
              ((v-prim name) (o-fun))
              ((v-void) (o-void))
              ((v-addr it)
               (obs-of-hv (some-v (hash-ref the-heap it))))))
          (define (obs-of-hv hv)
            (type-case HeapValue hv
              ((h-vec vs) (o-vec (vector-map obs-of-val vs)))
              ((h-list vs) (o-list (map obs-of-val vs)))
              ((h-fun env name arg* def* body) (o-fun))
              ((h-env _env _map)
               (raise (exn-internal 'obs-of-val "Impossible.")))))
          (define (as-fun (v : Val))
            (type-case Val v
              ((v-prim name) (op-prim name))
              ((v-addr addr)
               (type-case HeapValue (some-v (hash-ref the-heap addr))
                 ((h-fun env name arg* def* body)
                  (op-fun env arg* def* body))
                 (else
                  (raise (exn-rt "not a function")))))
              (else (raise (exn-rt "not a function")))))
          (define (interp-beta (fun : Val) (arg-v* : (Listof Val)) env ectx [stack : Stack])
            :
            (Result 'Val)
            (begin
              (display-state (t-app (t-quote fun) (map t-quote arg-v*)) env ectx stack)
              (type-case
                  Operator
                (as-fun fun)
                ((op-prim op) (let ((v (delta op arg-v* env ectx stack))) (apply-k v env ectx stack)))
                ((op-fun clos-env arg-x* def* body)
                 (let ([stack (cons (values env ectx (ca-app fun arg-v*)) stack)])
                   (let ([ectx (list)])
                     (let ((env (env-extend/declare clos-env
                                                    (append (map2 pair arg-x* (map some arg-v*))
                                                            (map (lambda (def)
                                                                   (let ([name (fst def)])
                                                                     (values name (none))))
                                                                 def*)))))
                       (let ((e (t-fun-call def* body)))
                         (interp e env ectx stack)))))))))
          (define (delta op v-arg* env ectx stack)
            (type-case
                PrimitiveOp
              op
              ((po-equalp)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1))) (v-bool (equal? v1 v2)))))
              ((po-+)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1)))
                   (let ((v1 (as-num v1))) (let ((v2 (as-num v2))) (v-num (+ v1 v2)))))))
              ((po--)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1)))
                   (let ((v1 (as-num v1))) (let ((v2 (as-num v2))) (v-num (- v1 v2)))))))
              ((po-*)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1)))
                   (let ((v1 (as-num v1))) (let ((v2 (as-num v2))) (v-num (* v1 v2)))))))
              ((po-/)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1)))
                   (let ((v1 (as-num v1))) (let ((v2 (as-num v2))) (v-num (/ v1 v2)))))))
              ((po-pair)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1))) (v-vec (list->vector (list v1 v2))))))
              ((po-pairp)
               (let ((v (list-ref v-arg* 0)))
                 (catch
                  (lambda ()
                    (let ([_ (as-vec v)])
                      (v-bool #t)))
                  (lambda (exn)
                    (v-bool #f)))))
              ((po-left)
               (let ((v (list-ref v-arg* 0)))
                 (let ((v (as-vec v)))
                   (let ((_ (unless (= (vector-length v) 2) (raise (exn-rt "left: not a pair")))))
                     (vector-ref v 0)))))
              ((po-right)
               (let ((v (list-ref v-arg* 0)))
                 (let ((v (as-vec v)))
                   (let ((_ (unless (= (vector-length v) 2) (raise (exn-rt "right: not a pair")))))
                     (vector-ref v 1)))))
              ((po-ivec) (v-vec (list->vector v-arg*)))
              ((po-list) (v-list v-arg*))
              ((po-pause)
               (let* ([_ (display-state
                          (t-quote (v-num 0))
                          env ectx
                          (cons (values env ectx (ca-app (v-prim (po-pause)) (list))) stack))])
                 (v-num 0)))
              ))




          (define (as-num (v : Val))
            :
            Number
            (type-case Val v ((v-num it) it) (else (raise (exn-rt "not a number")))))
          (define (as-vec (v : Val))
            (type-case Val v
              ((v-addr addr)
               (type-case HeapValue (some-v (hash-ref the-heap addr))
                 ((h-vec it) it)
                 (else
                  (raise (exn-rt (format "not a vector ~a" (s-exp-of-v v)))))))
              (else (raise (exn-rt (format "not a vector ~a" (s-exp-of-v v)))))))
          )
    (let ((_ (reset-output-buffer!)))
      (let ((_
             (catch
              (λ ()
                (let* ((e (compile e))
                       (_ (check e))
                       (env base-env)
                       (ectx empty)
                       (stack empty)
                       (_ (interp e env ectx stack)))
                  (void)))
              (λ (exn)
                (type-case
                    Exception
                  exn
                  ((exn-tc msg) (output! (list (o-exn msg))))
                  ((exn-rt msg) (output! (list (o-exn msg))))
                  ((exn-internal where what) (error where what)))))))
        (unbox output-buffer))))
  )