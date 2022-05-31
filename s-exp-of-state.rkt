#lang plait

(require "utilities.rkt")
(require "datatypes.rkt")
(require (typed-in "string-of-state.rkt" [block : Symbol]))
(require (typed-in racket
                   [number->string : (Number -> String)]
                   [vector-map : (('a -> 'b) (Vectorof 'a) -> (Vectorof 'b))]
                   [sort : ((Listof 'x) ('x 'x -> Boolean) -> (Listof 'x))]))
(require (opaque-type-in racket [Any any/c]))
(require (rename-in (typed-in racket [identity : ('a -> Any)]) [identity inj]))


(define (s-exp-of-state fun-addr?)
 (lambda (state)
  (let-values (((the-heap state) state))
    (local ((define (s-exp-of-stack stack)
              (inj (map s-exp-of-sf stack)))
            (define (s-exp-of-heap heap)
              (inj
               (map
                (lambda (key)
                  (inj (list (s-exp-of-addr key)
                             (s-exp-of-hv (some-v (hash-ref heap key))))))
                (filter ha-user? (hash-keys heap)))))
            (define (s-exp-of-hv hv): Any
              (type-case HeapValue hv
                ((h-env env map)
                 (inj
                  (list (inj 'Environment)
                        (inj (ind-List (hash-keys map)
                                       (list)
                                       (lambda (IH k)
                                         (cons (inj (list (inj k) (s-exp-of-optionof-v (some-v (hash-ref map k)))))
                                               IH))))
                        (s-exp-of-env env))))
                ((h-vec vs)
                 (inj (vector-map s-exp-of-v vs)))
                ((h-cons it)
                 (inj (list (inj 'Cons)
                            (s-exp-of-v (fst it))
                            (s-exp-of-v (snd it)))))
                ((h-fun env name arg* def* body)
                 (inj (list (inj 'Closure)
                            (s-exp-of-env env)
                            (inj (s-exp-of-funname name))
                            (make-fun arg* def* body))))))
            (define (s-exp-of-funname nm)
              (type-case (Optionof Symbol) nm
                [(none) (inj '_)]
                [(some s) (inj s)]))
            (define (s-exp-of-optionof-v ov)
              (type-case (Optionof Val) ov
                [(none)
                 (inj '_)]
                [(some v)
                 (s-exp-of-v v)]))
            (define (s-exp-of-sf sf)
              (local ((define-values (env ectx ann) sf))
                (inj (list (s-exp-of-env env)
                           (s-exp-of-ectx ectx)
                           (s-exp-of-ann ann)))))
            (define (s-exp-of-ann ann)
              (type-case CtxAnn ann
                #;
                ((ca-toplevel)
                 (inj 'the-top-level))
                ((ca-let)
                 (inj 'let))
                ((ca-letrec)
                 (inj 'letrec))
                ((ca-app fun arg*)
                 (inj (cons (s-exp-of-v fun) (map s-exp-of-v arg*))))))
            (define (s-exp-of-env env): Any
              (type-case Env env
                ((none)
                 (inj '_))
                ((some addr)
                 (s-exp-of-addr addr))))
            (define (s-exp-of-ectx ectx)
              (inj (ind-List (reverse (map s-exp-of-f ectx))
                             (inj '□)
                             (lambda (IH x)
                               (x IH)))))
            (define (s-exp-of-xv xv)
              (inj (list (s-exp-of-x (fst xv)) (s-exp-of-v (snd xv)))))
            (define (s-exp-of-f f)
              (lambda ([□ : Any])
                (type-case ECFrame f
                  ((F-begin prelude* result)
                   (inj (append
                         (list (inj 'begin) □)
                         (append
                          (map s-exp-of-e prelude*)
                          (list (s-exp-of-e result))))))
                  ((F-app v* e*)
                   (inj (append
                         (map s-exp-of-v v*)
                         (cons
                          □
                          (map s-exp-of-e e*)))))
                  ((F-let xv* x xe* body)
                   (inj (list (inj 'let)
                              (inj
                               (append
                                (map s-exp-of-xv xv*)
                                (cons (inj (list (s-exp-of-x x) □))
                                      (map s-exp-of-xe xe*))))
                              (s-exp-of-e body))))
                  ((F-if thn els)
                   (inj (list (inj 'if) □ (s-exp-of-e thn) (s-exp-of-e els))))
                  ((F-set! var)
                   (inj (list (inj 'set!) (s-exp-of-x var) □)))
                  ((P-def x d* e*)
                   (inj (append
                         (list (inj block)
                               ((s-exp-of-f (F-set! x)) □))
                         (append
                          (map s-exp-of-set! d*)
                          (map s-exp-of-e e*)))))
                  ((P-exp e*)
                   (inj (append (list (inj block)
                                      □)
                                (map s-exp-of-e e*)))))))
            (define (s-exp-of-prim p)
              (type-case PrimitiveOp p
                [(po-not)
                 (inj 'not)]
                [(po-left)
                 (inj 'left)]
                [(po-right)
                 (inj 'right)]
                [(po-vlen)
                 (inj 'vlen)]
                [(po-eqp)
                 (inj 'equal?)]
                [(po-equalp)
                 (inj 'equal?)]
                [(po-zerop)
                 (inj 'zero?)]
                [(po-+)
                 (inj '+)]
                [(po--)
                 (inj '-)]
                [(po-*)
                 (inj '*)]
                [(po-/)
                 (inj '/)]
                [(po-<)
                 (inj '<)]
                [(po->)
                 (inj '>)]
                [(po-<=)
                 (inj '<=)]
                [(po->=)
                 (inj '>=)]
                [(po-=)
                 (inj '=)]
                [(po-pairp)
                 (inj 'pair?)]
                [(po-mpair)
                 (inj 'mpair)]
                [(po-set-left!)
                 (inj 'set-left!)]
                [(po-set-right!)
                 (inj 'set-right!)]
                [(po-vref)
                 (inj 'vref)]
                [(po-cons)
                 (inj 'cons)]
                [(po-first)
                 (inj 'first)]
                [(po-rest)
                 (inj 'rest)]
                [(po-vset!)
                 (inj 'vset!)]
                [(po-mvec)
                 (inj 'mvec)]
                [(po-list)
                 (inj 'list)]))
            (define (s-exp-of-addr it)
              (type-case HeapAddress it
                [(ha-user it)
                 (let ([printing (format "~a" (inj it))])
                   (type-case HeapValue (some-v (hash-ref the-heap (ha-user it)))
                     ((h-fun env name arg* def* body)
                      (type-case (Optionof Symbol) name
                        ((none)
                         (inj printing))
                        ((some s)
                         (if fun-addr?
                         (let ([printing (string-append printing (format ".~a" (inj s)))])
                           (inj printing))
                           (inj (format "~a" s)))
                     )))
                     (else
                      (inj printing))))]
                [(ha-prim it)
                 (s-exp-of-primitive-address it)]))
            (define (s-exp-of-primitive-address pa)
              (type-case PrimitiveHeapAddress pa
                [(pa-map) (inj 'map)]
                [(pa-filter) (inj 'filter)]
                [(pa-base-env) (inj 'base-env)]
                [(pa-empty) (inj 'empty)]))
            (define (s-exp-of-v v)
              (type-case Val v
                ((v-addr it)
                 (inj (string->symbol (format "@~a" (s-exp-of-addr it)))))
                ((v-prim name)
                 (s-exp-of-prim name))
                ((v-str it)
                 (inj it))
                ((v-num it)
                 (inj it))
                ((v-bool it)
                 (inj it))
                ((v-empty)
                 (inj '()))
                ((v-void)
                 (inj '|#<void>|))))
            (define (s-exp-of-x x) (inj x))
            (define (s-exp-of-def def)
              (local ((define-values (x e) def))
                (inj (list (inj 'defvar)
                           (s-exp-of-x x)
                           (s-exp-of-e e)))))
            (define (s-exp-of-set! def)
              (local ((define-values (x e) def))
                (inj (list (inj 'set!)
                           (s-exp-of-x x)
                           (s-exp-of-e e)))))
            (define (make-fun args def* body)
              (inj
               (list (inj 'lambda)
                     (inj (map s-exp-of-x args))
                     (inj (cons (inj block) (append (map s-exp-of-def def*) (list (s-exp-of-e body))))))))
            (define (s-exp-of-e e)
              (type-case Term e
                [(t-quote v)
                 (s-exp-of-v v)]
                [(t-var x)
                 (s-exp-of-x x)]
                [(t-fun name args def* body)
                 (make-fun args def* body)]
                [(t-app fun arg*)
                 (inj (map s-exp-of-e (cons fun arg*)))]
                [(t-let bind* body)
                 (inj
                  (list (inj 'let)
                        (inj (map s-exp-of-xe bind*))
                        (s-exp-of-e body)))]
                [(t-letrec bind* body)
                 (inj
                  (list (inj 'letrec)
                        (inj (map s-exp-of-xe bind*))
                        (s-exp-of-e body)))]
                [(t-set! x e)
                 (inj
                  (list
                   (inj 'set!)
                   (s-exp-of-x x)
                   (s-exp-of-e e)))]
                [(t-begin e* e)
                 (inj
                  (cons
                   (inj 'begin)
                   (map s-exp-of-e (append e* (list e)))))]
                [(t-if cnd thn els)
                 (inj
                  (list
                   (inj 'if)
                   (s-exp-of-e cnd)
                   (s-exp-of-e thn)
                   (s-exp-of-e els)))]))
            (define (s-exp-of-xe xe)
              (inj (list (s-exp-of-x (fst xe)) (s-exp-of-e (snd xe)))))
            (define (s-exp-of-b xe)
              (inj (list (inj 'set!) (s-exp-of-x (fst xe)) (s-exp-of-e (snd xe))))))
      (type-case OtherState state
        [(to-fun-call fun arg* env ectx stack clos-env arg-x* def* body)
         (inj (list (inj "Computing")
                    (s-exp-of-e (t-app (t-quote fun) (map t-quote arg*)))
                    (s-exp-of-env env)
                    (s-exp-of-ectx ectx)
                    (s-exp-of-stack stack)
                    (s-exp-of-heap the-heap)))]
        [(return v env ectx stack)
         (inj (list (inj "Returning")
                    (s-exp-of-v v)
                    (s-exp-of-env env)
                    (s-exp-of-ectx ectx)
                    (s-exp-of-stack stack)
                    (s-exp-of-heap the-heap)))]
        [(terminate)
         (inj (list (inj "Terminated")
                    (s-exp-of-heap the-heap)))])))))
