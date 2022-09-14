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
                   [andmap : (('a 'a -> Boolean) (Listof 'a) (Listof 'a) -> Boolean)]
                   [take : ((Listof 'a) Number -> (Listof 'a))]
                   [drop : ((Listof 'a) Number -> (Listof 'a))]
                   [index-of : ((Listof 'a) 'a -> Number)]
                   ))

(define (compile [program : Program]): CompiledProgram
  (let* ([def* (fst program)]
         [exp* (snd program)]
         [def* (map compile-def def*)]
         [exp* (map compile-e exp*)])
    (values def* exp*)))
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
     (t-let (map compile-bind bind*) (block-of-def* def* prelude* result))]
    [(e-let* bind* def* prelude* result)
     (compile-let* (map compile-bind bind*)
                   (block-of-def* def* prelude* result))]
    [(e-letrec bind* def* prelude* result)
     (t-letrec (map compile-bind bind*)
               (block-of-def* def* prelude* result))]
    [(e-set! var val)
     (t-set! var (compile-e val))]
    [(e-begin prelude* result)
     (compile-begin prelude* result)]
    [(e-if cnd thn els)
     (t-if (compile-e cnd)
           (compile-e thn)
           (compile-e els))]
    [(e-cond thn-cnd* els)
     (t-cond (map compile-e&e thn-cnd*)
             (type-case (Optionof '_) els
               [(none) (none)]
               [(some e)
                (some (compile-e e))]))]))
(define (compile-e&e e&e)
  (pair (compile-e (fst e&e))
        (compile-e (snd e&e))))
(define (compile-def def) : (Id * Term)
  (type-case Def def
    ((d-var x e)
     (values x (compile-e e)))
    ((d-fun fun arg* def* prelude* result)
     (values fun (compile-fun (some fun) arg* def* prelude* result)))))
(define (compile-fun name arg* def* prelude* result)
  (t-fun name arg*
         (block
          (map compile-def def*)
          (map compile-e prelude*)
          (compile-e result))))
(define (compile-bind bind)
  (values (fst bind)
          (compile-e (snd bind))))
(define (block-of-def* def* prelude* result)
  (block
   (map compile-def def*)
   (map compile-e prelude*)
   (compile-e result)))
(define (term-of-block b)
  (cond
    [(not (empty? (block-def* b)))
     (t-let (list) b)]
    [(not (empty? (block-exp* b)))
     (t-block (block-exp* b) (block-out b))]
    [else
     (block-out b)]))
(define (compile-let* bind* [body : Block])
  (term-of-block
   (ind-List bind*
             body
             (λ (IH bind)
               (block
                (list)
                (list)
                (t-let (list bind) IH))))))
(define (compile-begin prelude* result)
  (t-begin
   (map compile-e prelude*)
   (compile-e result)))

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
    ((c-char it)
     (t-quote (v-char it)))
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

(define (as-fun the-heap (v : Val))
  (type-case Val v
    ((v-prim name) (op-prim name))
    ((v-addr addr)
     (type-case HeapValue (heap-ref the-heap addr)
       ((h-fun env name arg* body)
        (op-fun env arg* body))
       (else
        (raise (exn-rt "not a function")))))
    (else (raise (exn-rt "not a function")))))

(define-syntax-rule
  (let-args ([(v ...) arg*]) . body)
  (cond
    [(not (= (length '(v ...)) (length arg*)))
     (raise (exn-rt "arity mismatch"))]
    [else
     (let ([v (list-ref arg* (index-of '(v ...) 'v))] ...)
       . body)]))

(define (eval tracing? enable-tco? [check : (((Listof (Id * Term)) * (Listof Term)) -> Void)] pict-of-state (e : Program))
  :
  Void

  (local ((define (apply-stack the-heap v stack)
            : State
            (type-case (Listof Ctx) stack
              (empty
               (raise (exn-internal 'apply-stack "The empty stack should have been caught by P-exp")))
              ((cons sf0 stack)
               (local ((define-values (env ectx ann) sf0))
                 (continue-returned the-heap v env ectx stack)
                 #;
                 (values the-heap (returned v env ectx stack))))))
          (define (continue-returning the-heap v stack)
            (apply-stack the-heap v stack))
          (define (continue-returned the-heap v env ectx stack)
            (do-apply-k the-heap v env ectx stack))
          (define (do-apply-k the-heap v env ectx [stack : Stack])
            : State
            (begin
              (type-case (Listof ECFrame) ectx
                [empty
                 (values the-heap (returning v stack))]
                ((cons f ectx)
                 (type-case ECFrame f
                   ((F-seq is-block e* e)
                    (interp-seq is-block the-heap e* e env ectx stack))
                   ((F-app v* e*)
                    (let ([v* (append v* (list v))])
                      (interp-app the-heap v* e* env ectx stack)))
                   ((F-let xv* x xe* body)
                    (interp-let the-heap (append xv* (list (pair x v))) xe* body env ectx stack))
                   ((F-if thn els)
                    (if (truthy? v)
                        (let ((e thn))
                          (do-interp the-heap e env ectx stack))
                        (let ((e els))
                          (do-interp the-heap e env ectx stack))))
                   ((F-set! var)
                    (values the-heap (setting var v env ectx stack))
                    #;
                    (let ((the-heap (env-set the-heap env var v)))
                      (do-apply-k the-heap (v-void) env ectx stack)))
                   ((P-def x d* e*)
                    (begin
                      (unless (and (empty? ectx) (empty? stack))
                        (raise (exn-internal 'apply-k "The ectx and the stack must be empty.")))
                      (let ([the-heap (env-set the-heap env x v)])
                        (do-interp-program-def* the-heap d* e* env))))
                   ((P-exp v* e*)
                    (begin
                      (unless (and (empty? ectx) (empty? stack))
                        (raise (exn-internal 'apply-k "The ectx and the stack must be empty.")))
                      (let ([o ((obs-of-val the-heap) v)])
                        (begin
                          (output! o)
                          (do-interp-program-exp* the-heap (append v* (list v)) e* env))))))))))
          (define (do-interp-program the-heap [p : CompiledProgram]) : State
            (local ((define-values (bind* exp*) p))
              (let ((var* (map var-of-bind bind*)))
                (let-values (((the-heap env) (env-declare the-heap base-env var*)))
                  (do-interp-program-def* the-heap bind* exp* env)))))
          (define (do-interp-program-def* the-heap [bind* : (Listof (Id * Term))] [exp* : (Listof Term)] [env : Env])
            : State
            (type-case (Listof (Id * Term)) bind*
              [empty (do-interp-program-exp* the-heap (list) exp* env)]
              [(cons bind bind*)
               (let* ([x (fst bind)]
                      [e (snd bind)])
                 (do-interp the-heap e env (list (P-def x bind* exp*)) empty))]))
          (define (do-interp-program-exp* the-heap [v* : (Listof Val)] [e* : (Listof Term)] [env : Env]): State
            (type-case (Listof Term) e*
              [empty
               (values the-heap (terminated v*))]
              [(cons e e*)
               (do-interp the-heap e env (list (P-exp v* e*)) empty)]))
          (define (do-ref the-heap x env ectx stack)
            (type-case (Optionof Val) (env-lookup the-heap env x)
              [(some v) (do-apply-k the-heap v env ectx stack)]
              [else (raise (exn-rt (format "unbound id ~a" x)))]))
          (define (do-interp the-heap [e : Term] [env : Env] ectx stack)
            : State
            (begin
              (type-case
                  Term
                e
                ((t-quote v) (do-apply-k the-heap v env ectx stack))
                ((t-var x)
                 (do-ref the-heap x env ectx stack))
                ((t-fun name arg* body)
                 (let-values (((the-heap v) (v-fun the-heap env name arg* body)))
                   (do-apply-k the-heap v env ectx stack)))
                ((t-app fun arg*) (interp-app the-heap (list) (cons fun arg*) env ectx stack))
                ((t-let bind* body) (interp-let the-heap (list) bind* body env ectx stack))
                ((t-letrec bind* body)
                 (let ([stack (cons (values env ectx (ca-letrec)) stack)])
                   (let ((ectx (list)))
                     (let ((var* (map var-of-bind bind*)))
                       (let-values (((the-heap env) (env-declare the-heap env var*)))
                         (let ([e (t-block
                                   (map (lambda (xe) (t-set! (fst xe) (snd xe))) bind*)
                                   (term-of-block body))])
                           (do-interp the-heap e env ectx stack)))))))
                ((t-set! var val)
                 (let ((e val))
                   (let ((ectx (cons (F-set! var) ectx)))
                     (do-interp the-heap e env ectx stack))))
                ((t-seq is-block prelude* result)
                 (interp-seq is-block the-heap prelude* result env ectx stack))
                ((t-if cnd thn els)
                 (let ((e cnd))
                   (let ((ectx (cons (F-if thn els) ectx)))
                     (do-interp the-heap e env ectx stack))))
                ((t-cond cnd-thn* els)
                 (type-case (Listof '_) cnd-thn*
                   [empty
                    (type-case (Optionof Term) els
                      [(none)
                       (do-apply-k the-heap (v-void) env ectx stack)]
                      [(some e)
                       (do-interp the-heap e env ectx stack)])]
                   [(cons cnd-thn cnd-thn*)
                    (let ([e (t-if (fst cnd-thn)
                                   (snd cnd-thn)
                                   (t-cond cnd-thn* els))])
                      (do-interp the-heap e env ectx stack))])))))
          (define (interp-app the-heap v* e* env ectx stack) : State
            (type-case
                (Listof Term)
              e*
              (empty
               (type-case
                   (Listof Val)
                 v*
                 (empty (raise (exn-internal 'interpter "")))
                 ((cons fun arg*) (interp-beta the-heap fun arg* env ectx stack))))
              ((cons e e*)
               (let ((ectx (cons (F-app v* e*) ectx)))
                 (do-interp the-heap e env ectx stack)))))
          (define (interp-seq is-block the-heap prelude* result env ectx stack) : State
            (type-case
                (Listof Term)
              prelude*
              (empty
               (let ([e result])
                 (do-interp the-heap e env ectx stack)))
              ((cons e prelude*)
               (let ((ectx (cons (F-seq is-block prelude* result) ectx)))
                 (do-interp the-heap e env ectx stack)))))
          (define (interp-let the-heap xv* xe* [body : Block] env ectx stack) : State
            (type-case
                (Listof (Id * Term))
              xe*
              (empty
               (enter-block the-heap env ectx (ca-let) stack env (map snd xv*) (map fst xv*) body))
              ((cons ⟨x×e⟩ xe*)
               (let ((x (fst ⟨x×e⟩)))
                 (let ((e (snd ⟨x×e⟩)))
                   (let ((ectx (cons (F-let xv* x xe* body) ectx)))
                     (do-interp the-heap e env ectx stack)))))))
          (define (output! o)
            (unless (o-void? o)
              (displayln (string-of-o o))))
          (define (interp-beta the-heap (fun : Val) (arg-v* : (Listof Val)) env ectx stack)
            : State
            (begin
              (type-case
                  Operator
                (as-fun the-heap fun)
                ((op-prim op)
                 (delta the-heap op arg-v* env ectx stack))
                ((op-fun clos-env arg-x* body)
                 (values the-heap (calling fun arg-v* env ectx stack clos-env arg-x* body))))))
          (define (continute-setting the-heap x v env ectx stack)
            (let ((the-heap (env-set the-heap env x v)))
              (continute-setted the-heap env ectx stack)))
          (define (do-call the-heap fun arg-v* env ectx stack clos-env arg-x* body) : State
            (enter-block the-heap env ectx (ca-app fun arg-v*) stack clos-env arg-v* arg-x* body))
          (define (enter-block the-heap env ectx ctx-ann stack base-env v* x* body) : State
            ;; tail-call optimization
            (let ([stack (if (and (empty? ectx) enable-tco?)
                             stack
                             (cons (values env ectx ctx-ann) stack))])
              (enter-block-2 the-heap stack base-env v* x* body)))
          (define (enter-block-2 the-heap stack base-env v* x* body) : State
            (let ([def* (block-def* body)])
              (cond
                [(not (= (length x*)
                         (length v*)))
                 (raise (exn-rt "arity mismatch"))]
                [else
                 (let-values (((the-heap env)
                               (env-extend/declare the-heap base-env
                                                   (append (map2 pair x* (map some v*))
                                                           (map (lambda (def)
                                                                  (let ([name (fst def)])
                                                                    (values name (none))))
                                                                def*)))))
                   (let ((e (t-init! body)))
                     (values the-heap (called e env stack))))])))
          (define (t-init! body)
            (t-block
             (append
              (map (lambda (xe) (t-set! (fst xe) (snd xe)))
                   (block-def* body))
              (block-exp* body))
             (block-out body)))
          (define (do-equal? the-heap v1 v2)
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
                                    (let ([v1 (heap-ref the-heap v1)]
                                          [v2 (heap-ref the-heap v2)])
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
          (define (delta the-heap op v-arg* env ectx stack) : State
            (letrec ([return-value
                      (lambda (the-heap v)
                        (do-apply-k the-heap v env ectx stack))]
                     [return-allocation
                      (lambda (heap&v)
                        (local ([define-values (the-heap v) heap&v])
                          (return-value the-heap v)))])
              (type-case
                  PrimitiveOp
                op
                ((po-not)
                 (let-args (((v) v-arg*))
                           (let ((v (as-bool v)))
                             (return-value the-heap (v-bool (not v))))))
                ((po-left)
                 (let-args (((v) v-arg*))
                           (let ((v (as-vec the-heap v)))
                             (let ((_ (unless (= (vector-length v) 2) (raise (exn-rt "left: not a pair")))))
                               (return-value the-heap (vector-ref v 0))))))
                ((po-right)
                 (let-args (((v) v-arg*))
                           (let ((v (as-vec the-heap v)))
                             (let ((_ (unless (= (vector-length v) 2) (raise (exn-rt "right: not a pair")))))
                               (return-value the-heap (vector-ref v 1))))))
                ((po-vec-len)
                 (let-args (((v) v-arg*))
                           (let ((v (as-vec the-heap v)))
                             (return-value the-heap (v-num (vector-length v))))))
                ((po-string-length)
                 (let-args (((v) v-arg*))
                           (let ((v (as-str v)))
                             (return-value the-heap (v-num (string-length v))))))
                ((po-string-append)
                 (let ((v* (map as-str v-arg*)))
                   (return-value the-heap (v-str (foldr string-append "" v*)))))
                ((po-string->list)
                 (let-args (((v) v-arg*))
                           (let ((v (as-str v)))
                             (return-allocation (v-list the-heap (map v-char (string->list v)))))))
                ((po-list->string)
                 (let-args (((v) v-arg*))
                           (let ((v (as-plait-list the-heap v)))
                             (let ((v (map as-char v)))
                               (return-value the-heap (v-str (list->string v)))))))
                ((po-zerop)
                 (let-args (((v1) v-arg*))
                           (return-value the-heap (v-bool (equal? v1 (v-num 0))))))
                ((po-emptyp)
                 (let-args (((v1) v-arg*))
                           (return-value the-heap (v-bool (equal? v1 (v-empty))))))
                ((po-eqp)
                 (let-args (((v1 v2) v-arg*))
                           (return-value the-heap (v-bool (equal? v1 v2)))))
                ((po-equalp)
                 (let-args (((v1 v2) v-arg*))
                           (return-value the-heap (v-bool (do-equal? the-heap v1 v2)))))
                ((po-+)
                 (cond
                   [(= 0 (length v-arg*))
                    (raise (exn-rt "arity mismatch"))]
                   [else
                    (let ([v-arg* (map as-num v-arg*)])
                      (return-value the-heap
                                    (v-num (foldl (lambda (x y) (+ y x))
                                                  (first v-arg*)
                                                  (rest v-arg*)))))]))
                ((po--)
                 (cond
                   [(= 0 (length v-arg*))
                    (raise (exn-rt "arity mismatch"))]
                   [else
                    (let ([v-arg* (map as-num v-arg*)])
                      (return-value the-heap
                                    (v-num (foldl (lambda (x y) (- y x))
                                                  (first v-arg*)
                                                  (rest v-arg*)))))]))
                ((po-*)
                 (cond
                   [(= 0 (length v-arg*))
                    (raise (exn-rt "arity mismatch"))]
                   [else
                    (let ([v-arg* (map as-num v-arg*)])
                      (return-value the-heap
                                    (v-num (foldl (lambda (x y) (* y x))
                                                  (first v-arg*)
                                                  (rest v-arg*)))))]))
                ((po-/)
                 (cond
                   [(= 0 (length v-arg*))
                    (raise (exn-rt "arity mismatch"))]
                   [else
                    (let ([v-arg* (map as-num v-arg*)])
                      (return-value the-heap
                                    (v-num (foldl (lambda (x y)
                                                    (if (zero? x)
                                                        (raise (exn-rt "division-by-zero"))
                                                        (/ y x)))
                                                  (first v-arg*)
                                                  (rest v-arg*)))))]))
                ((po-<)
                 (let-args (((v1 v2) v-arg*))
                           (let ((v1 (as-num v1)))
                             (let ((v2 (as-num v2)))
                               (return-value the-heap (v-bool (< v1 v2)))))))
                ((po->)
                 (let-args (((v1 v2) v-arg*))
                           (let ((v1 (as-num v1)))
                             (let ((v2 (as-num v2)))
                               (return-value the-heap (v-bool (> v1 v2)))))))
                ((po-<=)
                 (let-args (((v1 v2) v-arg*))
                           (let ((v1 (as-num v1)))
                             (let ((v2 (as-num v2)))
                               (return-value the-heap (v-bool (<= v1 v2)))))))
                ((po->=)
                 (let-args (((v1 v2) v-arg*))
                           (let ((v1 (as-num v1)))
                             (let ((v2 (as-num v2)))
                               (return-value the-heap (v-bool (>= v1 v2)))))))
                ((po-=)
                 (let-args (((v1 v2) v-arg*))
                           (let ((v1 (as-num v1)))
                             (let ((v2 (as-num v2)))
                               (return-value the-heap (v-bool (= v1 v2)))))))
                ((po-pairp)
                 (let-args (((v) v-arg*))
                           (catch
                            (lambda ()
                              (let ([v (as-vec the-heap v)])
                                (return-value the-heap (v-bool (= (vector-length v) 2)))))
                            (lambda (exn)
                              (return-value the-heap (v-bool #f))))))
                ((po-consp)
                 (let-args (((v) v-arg*))
                           (catch
                            (lambda ()
                              (let ([v (as-cons the-heap v)])
                                (return-value the-heap (v-bool #t))))
                            (lambda (exn)
                              (return-value the-heap (v-bool #f))))))
                ((po-mpair)
                 (let-args (((v1 v2) v-arg*))
                           (return-allocation (v-vec the-heap (list->vector (list v1 v2))))))
                ((po-set-left!)
                 (let-args (((v e) v-arg*))
                           (vector-set the-heap v 0
                                       e
                                       (lambda (n) (= n 2))
                                       env ectx stack)))
                ((po-set-right!)
                 (let-args (((v e) v-arg*))
                           (vector-set the-heap v 1
                                       e
                                       (lambda (n) (= n 2))
                                       env ectx stack)))
                ((po-vec-ref)
                 (let-args (((v1 v2) v-arg*))
                           (let ((v1 (as-vec the-heap v1)))
                             (let ((v2 (as-num v2)))
                               (let ((_ (unless (< v2 (vector-length v1))
                                          (raise (exn-rt "vec-ref: vector too short.")))))
                                 (return-value the-heap (vector-ref v1 v2)))))))
                ((po-cons)
                 (let-args (((v1 v2) v-arg*))
                           (let ((v2 (as-list the-heap v2)))
                             (return-allocation (v-cons the-heap (pair v1 v2))))))
                ((po-first)
                 (let-args (((v1) v-arg*))
                           (let ((v1 (as-cons the-heap v1)))
                             (return-value the-heap (fst v1)))))
                ((po-rest)
                 (let-args (((v1) v-arg*))
                           (let ((v1 (as-cons the-heap v1)))
                             (return-value the-heap (snd v1)))))
                ((po-vec-set!)
                 (let-args ([(v i e) v-arg*])
                           (let ([i (as-num i)])
                             (vector-set
                              the-heap
                              v
                              i
                              e
                              (lambda (n) (< i n))
                              env ectx stack))))
                ((po-mvec)
                 (return-allocation (v-vec the-heap (list->vector v-arg*))))
                ((po-list)
                 (return-allocation (v-list the-heap v-arg*))))))
          (define (as-num (v : Val))
            :
            Number
            (type-case Val v ((v-num it) it) (else (raise (exn-rt "not a number")))))
          (define (as-str (v : Val))
            :
            String
            (type-case Val v ((v-str it) it) (else (raise (exn-rt "not a string")))))
          (define (as-bool (v : Val))
            :
            Boolean
            (type-case Val v ((v-bool it) it) (else (raise (exn-rt "not a boolean")))))
          (define (as-char (v : Val))
            :
            Char
            (type-case Val v ((v-char it) it) (else (raise (exn-rt "not a char")))))
          (define (functional-vector-set vec i elm)
            (let ((lst (vector->list vec)))
              (let ((pre (take lst i))
                    (pos (drop lst (add1 i))))
                (h-vec (list->vector (append pre (cons elm pos)))))))
          (define (continute-vector-setting the-heap addr it i velm env ectx stack)
            (let ([the-heap (heap-set the-heap addr (functional-vector-set it i velm))])
              (do-apply-k the-heap (v-void) env ectx stack)))
          (define (vector-set [the-heap : Heap] [v : Val] i velm len-valid? env ectx stack) : State
            (type-case Val v
              ((v-addr addr)
               (type-case HeapValue (heap-ref the-heap addr)
                 ((h-vec it)
                  (begin
                    (unless (len-valid? (vector-length it))
                      (raise (exn-rt "the length of the vector is not what I expected.")))
                    (values the-heap (vector-setting addr it i velm env ectx stack))))
                 (else
                  (raise (exn-rt (format "not a vector ~a" ((string-of-o-of-v the-heap) v)))))))
              (else (raise (exn-rt (format "not a vector ~a" ((string-of-o-of-v the-heap) v)))))))
          (define (as-vec the-heap (v : Val))
            (type-case Val v
              ((v-addr addr)
               (type-case HeapValue (heap-ref the-heap addr)
                 ((h-vec it) it)
                 (else
                  (raise (exn-rt (format "not a vector ~a" ((string-of-o-of-v the-heap) v)))))))
              (else (raise (exn-rt (format "not a vector ~a" ((string-of-o-of-v the-heap) v)))))))
          (define (as-list the-heap (v : Val))
            (type-case Val v
              ((v-empty) v)
              ((v-addr addr)
               (type-case HeapValue (heap-ref the-heap addr)
                 ((h-cons it) v)
                 (else
                  (raise (exn-rt (format "not a list ~a" ((string-of-o-of-v the-heap) v)))))))
              (else (raise (exn-rt (format "not a list ~a" ((string-of-o-of-v the-heap) v)))))))
          (define (as-plait-list the-heap (v : Val))
            (type-case Val v
              ((v-empty) (list))
              ((v-addr addr)
               (type-case HeapValue (heap-ref the-heap addr)
                 ((h-cons it)
                  (cons (fst it) (as-plait-list the-heap (snd it))))
                 (else
                  (raise (exn-rt (format "not a list ~a" ((string-of-o-of-v the-heap) v)))))))
              (else (raise (exn-rt (format "not a list ~a" ((string-of-o-of-v the-heap) v)))))))
          (define (as-cons the-heap (v : Val))
            (type-case Val v
              ((v-addr addr)
               (type-case HeapValue (heap-ref the-heap addr)
                 ((h-cons it) it)
                 (else
                  (raise (exn-rt (format "not a cons ~a" ((string-of-o-of-v the-heap) v)))))))
              (else (raise (exn-rt (format "not a cons ~a" ((string-of-o-of-v the-heap) v)))))))
          (define (continute-setted the-heap env ectx stack)
            (do-apply-k the-heap (v-void) env ectx stack))
          (define (forward [state : State])
            (let-values (((the-heap state) state))
              (catch
               (λ ()
                 (type-case OtherState state
                   [(vector-setting addr it i v env ectx stack)
                    (continute-vector-setting the-heap addr it i v env ectx stack)]
                   [(setting x v env ectx stack)
                    (continute-setting the-heap x v env ectx stack)]
                   [(setted env ectx stack)
                    (continute-setted the-heap env ectx stack)]
                   [(calling fun arg-v* env ectx stack clos-env arg-x* body)
                    (do-call the-heap fun arg-v* env ectx stack clos-env arg-x* body)]
                   [(called e env stack)
                    (do-interp the-heap e env (list) stack)]
                   [(returning v stack)
                    (continue-returning the-heap v stack)]
                   [(returned v env ectx stack)
                    (continue-returned the-heap v env ectx stack)]
                   [else
                    (raise (exn-internal 'forward "The program has terminated"))]))
               (λ (exn)
                 (begin
                   (handle-exn exn)
                   (values the-heap (errored)))))))
          (define (final? [s : OtherState])
            (or (terminated? s)
                (errored? s)))
          (define (trampoline [state : State])
            (when (not (final? (snd state)))
              (trampoline (forward state))))
          (define (handle-exn exn)
            (type-case
                Exception
              exn
              ((exn-tc msg) (output! (o-exn msg)))
              ((exn-rt msg) (output! (o-exn msg)))
              ((exn-internal where what) (error where what)))))
    (let ([initial-state
           (catch
            (λ ()
              (let* ((e (compile e))
                     (_ (check e)))
                (some (do-interp-program base-heap e))))
            (λ (exn)
              (begin
                (handle-exn exn)
                (none))))])
      (type-case (Optionof State) initial-state
        [(none) (void)]
        [(some state)
         (if tracing?
             (pict-loop state (lambda (state) (final? (snd state))) forward pict-of-state)
             (catch
              (λ ()
                (trampoline state))
              (λ (exn)
                (begin
                  (handle-exn exn)
                  (void)))))]))))