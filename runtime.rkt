#lang plait
(require (opaque-type-in pict
                         [Pict pict?]))
(require (typed-in pict
                   [text : (String -> Pict)]))
(require (typed-in "pict-loop.rkt"
                   [pict-loop : ('state 'terminated? 'forward 'pict-of-state -> Void)]))
(require "utilities.rkt")
(require "error.rkt")
(require (typed-in "show.rkt" [string-of-o : (Obs -> String)]))
(require "io.rkt")
(require "datatypes.rkt")
(require "s-exp-of-state.rkt")
(require (typed-in racket
                   [list->vector : ((Listof 'a) -> (Vectorof 'a))]
                   [vector->list : ((Vectorof 'a) -> (Listof 'a))]
                   [vector-map : (('a -> 'b) (Vectorof 'a) -> (Vectorof 'b))]
                   [remove-duplicates : ((Listof 'a) -> (Listof 'a))]
                   [andmap : (('a 'a -> Boolean) (Listof 'a) (Listof 'a) -> Boolean)]))


(define-type Operator
  (op-prim [name : PrimitiveOp])
  (op-fun [env : Env] [arg* : (Listof Id)] [def* : (Listof (Id * Term))] [body : Term]))


(define-type State
  (interp [e : Term] [env : Env] [ectx : ECtx] [stack : Stack] [pctx : PCtx])
  (to-fun-call [fun : Val] [args* : (Listof Val)] [env : Env] [ectx : ECtx] [stack : Stack] [pctx : PCtx]
               [clos-env : Env] [arg* : (Listof Id)] [def* : (Listof (Id * Term))] [body : Term])
  (return [v : Val] [env : Env] [ectx : ECtx] [stack : Stack] [pctx : PCtx])
  (terminate))

(define (compile [program : Program]): CompiledProgram
  (let* ([def* (fst program)]
         [exp* (snd program)]
         [def* (map compile-def def*)]
         [exp* (map compile-e exp*)])
    (values def* exp*))
  #;
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
     (t-app (t-quote (v-prim (po-mvec))) (map term-of-c it)))
    ((c-list it)
     (t-app (t-quote (v-prim (po-list))) (map term-of-c it)))))


(define (simple? e)
  (type-case Term e
    ((t-var _) #t)
    ((t-quote _) #t)
    (else #f)))


(define (obs-of-val v)
  (type-case
      Val
    v
    ((v-str it) (o-con (c-str it)))
    ((v-num it) (o-con (c-num it)))
    ((v-bool it) (o-con (c-bool it)))
    ((v-prim name) (o-fun))
    ((v-empty) (o-list '()))
    ((v-void) (o-void))
    ((v-addr it)
     (obs-of-hv (some-v (hash-ref the-heap it))))))
(define (obs-of-hv hv)
  (type-case HeapValue hv
    ((h-vec vs) (o-vec (vector-map obs-of-val vs)))
    ((h-cons vs) (o-list (cons (obs-of-val (fst vs))
                               (o-list-it (obs-of-val (snd vs))))))
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

(define (eval tracing? [check : (((Listof (Id * Term)) * (Listof Term)) -> Void)] pict-of-state (e : Program))
  :
  Void

  (local ((define (apply-pctx v [pctx : PCtx])
            (let* ((env (snd pctx))
                   (ctx (fst pctx)))
              (type-case ProgramContext ctx
                [(P-def x bind* exp*)
                 (let ((_ (env-set! env x v)))
                   (do-interp-program-def* bind* exp* env))]
                [(P-exp exp*)
                 (let ([_ (output! (obs-of-val v))])
                   (do-interp-program-exp* exp* env))])))
          (define (apply-stack v stack pctx)
            (type-case (Listof Ctx) stack
              (empty
               (apply-pctx v pctx))
              ((cons sf0 stack)
               (local ((define-values (env ectx ann) sf0))
                 (return v env ectx stack pctx)))))
          (define (do-apply-k v env ectx [stack : Stack] pctx)
            : State
            (begin
              (type-case (Listof ECFrame) ectx
                [empty
                 (apply-stack v stack pctx)]
                ((cons f ectx)
                 (type-case ECFrame f
                   ((F-begin e* e)
                    (interp-begin e* e env ectx stack pctx))
                   ((F-app v* e*)
                    (let ([v* (append v* (list v))])
                      (interp-app v* e* env ectx stack pctx)))
                   ((F-let xv* x xe* body)
                    (interp-let (append xv* (list (pair x v))) xe* body env ectx stack pctx))
                   #;
                   ((F-letrec-1 x xe* body)
                    (let ((_ (env-set! env x v)))
                      (interp-letrec-1 xe* body env ectx stack pctx)))
                   #;
                   ((F-fun-call x xe* body)
                    (let ((_ (env-set! env x v)))
                      (interp-fun-call xe* body env ectx stack pctx)))
                   ((F-if thn els)
                    (if (truthy? v)
                        (let ((e thn))
                          (do-interp e env ectx stack pctx))
                        (let ((e els))
                          (do-interp e env ectx stack pctx))))
                   ((F-set! var)
                    (let* ((_ (env-set! env var v))
                           (v (v-void)))
                      (do-apply-k v env ectx stack pctx)))
                   )))))
          (define (do-interp-program [p : CompiledProgram]) : State
            (local ((define-values (bind* exp*) p))
              (let ((var* (map var-of-bind bind*)))
                (let ((env (env-declare base-env var*)))
                  (do-interp-program-def* bind* exp* env)))))
          (define (do-interp-program-def* [bind* : (Listof (Id * Term))] [exp* : (Listof Term)] [env : Env])
            : State
            (type-case (Listof (Id * Term)) bind*
              [empty (do-interp-program-exp* exp* env)]
              [(cons bind bind*)
               (let* ([x (fst bind)]
                      [e (snd bind)])
                 (do-interp e env empty empty (pair (P-def x bind* exp*) env)))]))
          (define (do-interp-program-exp* [exp* : (Listof Term)] [env : Env]): State
            (type-case (Listof Term) exp*
              [empty
               (terminate)]
              [(cons e e*)
               (do-interp e env empty empty (pair (P-exp e*) env))]))
          (define (do-interp [e : Term] [env : Env] ectx stack [pctx : PCtx])
            : State
            (begin
              (type-case
                  Term
                e
                ((t-quote v) (do-apply-k v env ectx stack pctx))
                ((t-var x)
                 (begin
                   (let ([v (env-lookup env x)])
                     (do-apply-k v env ectx stack pctx))))
                ((t-fun name arg* def* body)
                 (let ((v (v-fun name env arg* def* body)))
                   (do-apply-k v env ectx stack pctx)))
                ((t-app fun arg*) (interp-app (list) (cons fun arg*) env ectx stack pctx))
                ((t-let bind* body) (interp-let (list) bind* body env ectx stack pctx))
                #;
                ((t-letrec-1 bind* body) (interp-letrec-1 bind* body env ectx stack pctx))
                #;
                ((t-fun-call bind* body) (interp-fun-call bind* body env ectx stack pctx))
                ((t-letrec bind* body)
                 (let ([stack (cons (values env ectx (ca-letrec)) stack)])
                   (let ((ectx (list)))
                     (let ((var* (map var-of-bind bind*)))
                       (let ((env (env-declare env var*)))
                         (let ([e (t-begin (map (lambda (xe) (t-set! (fst xe) (snd xe))) bind*) body)])
                           (do-interp e env ectx stack pctx)))))))
                ((t-set! var val)
                 (let ((e val))
                   (let ((ectx (cons (F-set! var) ectx)))
                     (do-interp e env ectx stack pctx))))
                ((t-begin prelude* result)
                 (interp-begin prelude* result env ectx stack pctx))
                ((t-if cnd thn els)
                 (let ((e cnd))
                   (let ((ectx (cons (F-if thn els) ectx)))
                     (do-interp e env ectx stack pctx)))))))
          (define (interp-app v* e* env ectx stack pctx)
            (type-case
                (Listof Term)
              e*
              (empty
               (type-case
                   (Listof Val)
                 v*
                 (empty (raise (exn-internal 'interpter "")))
                 ((cons fun arg*) (interp-beta fun arg* env ectx stack pctx))))
              ((cons e e*)
               (let ((ectx (cons (F-app v* e*) ectx)))
                 (do-interp e env ectx stack pctx)))))
          (define (interp-begin prelude* result env ectx stack pctx)
            (type-case
                (Listof Term)
              prelude*
              (empty
               (let ([e result])
                 (do-interp e env ectx stack pctx)))
              ((cons e prelude*)
               (let ((ectx (cons (F-begin prelude* result) ectx)))
                 (do-interp e env ectx stack pctx)))))
          (define (interp-let xv* xe* body env ectx stack pctx)
            (type-case
                (Listof (Id * Term))
              xe*
              (empty
               (let ([stack (cons (values env ectx (ca-let)) stack)])
                 (let ((env (env-extend env xv*)))
                   (let ((ectx (list)))
                     (let ((e body))
                       (do-interp e env ectx stack pctx))))))
              ((cons ⟨x×e⟩ xe*)
               (let ((x (fst ⟨x×e⟩)))
                 (let ((e (snd ⟨x×e⟩)))
                   (let ((ectx (cons (F-let xv* x xe* body) ectx)))
                     (do-interp e env ectx stack pctx)))))))
          #;
          (define (interp-letrec-1 xe* body env ectx stack pctx)
            (type-case
                (Listof (Id * Term))
              xe*
              (empty
               (let ([e body])
                 (do-interp e env ectx stack pctx)))
              ((cons xe xe*)
               (let ((x (fst xe)))
                 (let ((e (snd xe)))
                   (let ((ectx (cons (F-letrec-1 x xe* body) ectx)))
                     (do-interp e env ectx stack pctx)))))))
          #;
          (define (interp-fun-call xe* body env ectx stack pctx)
            (type-case
                (Listof (Id * Term))
              xe*
              (empty
               (let ([e body])
                 (do-interp e env ectx stack pctx)))
              ((cons xe xe*)
               (let ((x (fst xe)))
                 (let ((e (snd xe)))
                   (let ((ectx (cons (F-fun-call x xe* body) ectx)))
                     (do-interp e env ectx stack pctx)))))))
          (define (output! o)
            (unless (o-void? o)
              (displayln (string-of-o o))))
          (define (interp-beta (fun : Val) (arg-v* : (Listof Val)) env ectx stack pctx)
            :
            (Result 'Val)
            (begin
              (type-case
                  Operator
                (as-fun fun)
                ((op-prim op)
                 (let ((v (delta op arg-v* env ectx stack pctx)))
                   (do-apply-k v env ectx stack pctx)))
                ((op-fun clos-env arg-x* def* body)
                 (to-fun-call fun arg-v* env ectx stack pctx clos-env arg-x* def* body)))))
          (define (do-fun-call fun arg-v* env ectx stack pctx clos-env arg-x* def* body)
            (let ([stack (cons (values env ectx (ca-app fun arg-v*)) stack)])
              (let ([ectx (list)])
                (let ((env (env-extend/declare clos-env
                                               (append (map2 pair arg-x* (map some arg-v*))
                                                       (map (lambda (def)
                                                              (let ([name (fst def)])
                                                                (values name (none))))
                                                            def*)))))
                  (let ((e (t-init! def* body)))
                    (interp e env ectx stack pctx))))))
          (define (t-init! bind* body)
            (t-begin (map (lambda (xe) (t-set! (fst xe) (snd xe))) bind*) body))
          (define (do-equal? v1 v2)
            (let ([visited (list)])
              (local ((define (do-equal?-helper v1 v2)
                        (or (equal? v1 v2)
                            (member (values v1 v2) visited)
                            (begin
                              (set! visited (cons (values v1 v2) visited))
                              (type-case Val v1
                                [(v-addr v1)
                                 (type-case Val v2
                                   [(v-addr v2)
                                    (let ([v1 (some-v (hash-ref the-heap v1))]
                                          [v2 (some-v (hash-ref the-heap v2))])
                                      (cond
                                        [(and (h-vec? v1) (h-vec? v2))
                                         (andmap
                                          do-equal?-helper
                                          (vector->list (h-vec-it v1))
                                          (vector->list (h-vec-it v2)))]
                                        [(and (h-cons? v1) (h-cons? v2))
                                         (and (do-equal?-helper (fst (h-cons-it v1))
                                                                (fst (h-cons-it v2)))
                                              (do-equal?-helper (snd (h-cons-it v1))
                                                                (snd (h-cons-it v2))))]
                                        [else #f]))]
                                   [else #f])]
                                [else #f])))))
                (do-equal?-helper v1 v2))))
          (define (delta op v-arg* env ectx stack pctx)
            (type-case
                PrimitiveOp
              op
              ((po-not)
               (let ((v (list-ref v-arg* 0)))
                 (let ((v (as-bool v)))
                   (v-bool (not v)))))
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
              ((po-vlen)
               (let ((v (list-ref v-arg* 0)))
                 (let ((v (as-vec v)))
                   (v-num (vector-length v)))))
              ((po-zerop)
               (let ((v1 (list-ref v-arg* 0)))
                 (v-bool (equal? v1 (v-num 0)))))
              ((po-eqp)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1))) (v-bool (equal? v1 v2)))))
              ((po-equalp)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1))) (v-bool (do-equal? v1 v2)))))
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
              ((po-<)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1)))
                   (let ((v1 (as-num v1))) (let ((v2 (as-num v2))) (v-bool (< v1 v2)))))))
              ((po->)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1)))
                   (let ((v1 (as-num v1))) (let ((v2 (as-num v2))) (v-bool (> v1 v2)))))))
              ((po-<=)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1)))
                   (let ((v1 (as-num v1))) (let ((v2 (as-num v2))) (v-bool (<= v1 v2)))))))
              ((po->=)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1)))
                   (let ((v1 (as-num v1))) (let ((v2 (as-num v2))) (v-bool (>= v1 v2)))))))
              ((po-=)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1)))
                   (let ((v1 (as-num v1))) (let ((v2 (as-num v2))) (v-bool (= v1 v2)))))))
              ((po-pairp)
               (let ((v (list-ref v-arg* 0)))
                 (catch
                  (lambda ()
                    (let ([_ (as-vec v)])
                      (v-bool #t)))
                  (lambda (exn)
                    (v-bool #f)))))
              ((po-mpair)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1))) (v-vec (list->vector (list v1 v2))))))
              ((po-set-left!)
               (let ((v (list-ref v-arg* 0)))
                 (let ((v (as-vec v)))
                   (let ((_ (unless (= (vector-length v) 2)
                              (raise (exn-rt "set-left!: not a pair")))))
                     (let ((_ (vector-set! v 0 (list-ref v-arg* 1))))
                       (v-void))))))
              ((po-set-right!)
               (let ((v (list-ref v-arg* 0)))
                 (let ((v (as-vec v)))
                   (let ((_ (unless (= (vector-length v) 2)
                              (raise (exn-rt "set-right!: not a pair")))))
                     (let ((_ (vector-set! v 1 (list-ref v-arg* 1))))
                       (v-void))))))
              ((po-vref)
               (let ((v (list-ref v-arg* 0)))
                 (let ((v (as-vec v)))
                   (vector-ref v (as-num (list-ref v-arg* 1))))))
              ((po-cons)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v2 (list-ref v-arg* 1)))
                   (let ((v2 (as-list v2)))
                     (v-cons (pair v1 v2))))))
              ((po-first)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v1 (as-cons v1)))
                   (fst v1))))
              ((po-rest)
               (let ((v1 (list-ref v-arg* 0)))
                 (let ((v1 (as-cons v1)))
                   (snd v1))))
              ((po-vset!)
               (let ((v (list-ref v-arg* 0)))
                 (let ((v (as-vec v)))
                   (let ((_ (vector-set! v (as-num (list-ref v-arg* 1)) (list-ref v-arg* 2))))
                     (v-void)))))
              ((po-mvec) (v-vec (list->vector v-arg*)))
              ((po-list) (v-list v-arg*))
              ((po-pause) (v-num 0))
              ))
          (define (as-num (v : Val))
            :
            Number
            (type-case Val v ((v-num it) it) (else (raise (exn-rt "not a number")))))
          (define (as-bool (v : Val))
            :
            Boolean
            (type-case Val v ((v-bool it) it) (else (raise (exn-rt "not a boolean")))))
          (define (as-vec (v : Val))
            (type-case Val v
              ((v-addr addr)
               (type-case HeapValue (some-v (hash-ref the-heap addr))
                 ((h-vec it) it)
                 (else
                  (raise (exn-rt (format "not a vector ~a" (s-exp-of-v v)))))))
              (else (raise (exn-rt (format "not a vector ~a" (s-exp-of-v v)))))))
          (define (as-list (v : Val))
            (type-case Val v
              ((v-empty) v)
              ((v-addr addr)
               (type-case HeapValue (some-v (hash-ref the-heap addr))
                 ((h-cons it) v)
                 (else
                  (raise (exn-rt (format "not a list ~a" (s-exp-of-v v)))))))
              (else (raise (exn-rt (format "not a list ~a" (s-exp-of-v v)))))))
          (define (as-cons (v : Val))
            (type-case Val v
              ((v-addr addr)
               (type-case HeapValue (some-v (hash-ref the-heap addr))
                 ((h-cons it) it)
                 (else
                  (raise (exn-rt (format "not a cons ~a" (s-exp-of-v v)))))))
              (else (raise (exn-rt (format "not a cons ~a" (s-exp-of-v v)))))))
          (define (forward [state : State])
            (begin
              (type-case State state
                [(interp e env ectx stack pctx)
                 (do-interp e env ectx stack pctx)]
                [(to-fun-call fun arg-v* env ectx stack pctx clos-env arg-x* def* body)
                 (do-fun-call fun arg-v* env ectx stack pctx clos-env arg-x* def* body)]
                [(return v env ectx stack pctx)
                 (do-apply-k v env ectx stack pctx)]
                [(terminate)
                 (terminate)])))
          (define (trampoline [state : State])
            (when (not (terminate? state))
              (trampoline (forward state))))
          (define (my-pict-of-state state)
            (type-case State state
              [(interp e env ectx stack pctx)
               (pict-of-state "Computing"
                              (s-exp-of-e e)
                              (s-exp-of-env env)
                              (s-exp-of-ectx ectx)
                              (s-exp-of-stack stack)
                              (s-exp-of-pctx pctx)
                              (s-exp-of-heap the-heap))]
              [(to-fun-call fun arg* env ectx stack pctx clos-env arg-x* def* body)
               (pict-of-state "Computing"
                              (s-exp-of-e (t-app (t-quote fun) (map t-quote arg*)))
                              (s-exp-of-env env)
                              (s-exp-of-ectx ectx)
                              (s-exp-of-stack stack)
                              (s-exp-of-pctx pctx)
                              (s-exp-of-heap the-heap))]
              [(return v env ectx stack pctx)
               (pict-of-state "Returning"
                              (s-exp-of-e (t-quote v))
                              (s-exp-of-env env)
                              (s-exp-of-ectx ectx)
                              (s-exp-of-stack stack)
                              (s-exp-of-pctx pctx)
                              (s-exp-of-heap the-heap))]
              [(terminate)
               (text "end of computation.")])
            ))
    (let ([initial-state
           (catch
            (λ ()
              (let* ((e (compile e))
                     (_ (check e)))
                (some (do-interp-program e))))
            (λ (exn)
              (begin
                (type-case
                    Exception
                  exn
                  ((exn-tc msg) (output! (o-exn msg)))
                  ((exn-rt msg) (output! (o-exn msg)))
                  ((exn-internal where what) (error where what)))
                (none))))])
      (type-case (Optionof State) initial-state
        [(none) (void)]
        [(some state)
         (if tracing?
             (pict-loop state terminate? forward my-pict-of-state)
             (trampoline state))]))))