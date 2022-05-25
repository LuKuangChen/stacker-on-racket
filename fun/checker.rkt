#lang plait

(require "../utilities.rkt")
(require "../error.rkt")
(require "../datatypes.rkt")

(define-type Type
  (T-val)
  (T-fun))
(define base-Tenv
  (let* ([env (some-v base-env)]
         [env (some-v (hash-ref the-heap env))]
         [env (h-env-map env)]
         [k->T (lambda (k)
                 (type-case Val (some-v (some-v (hash-ref env k)))
                   [(v-prim _) (T-fun)]
                   [else (T-val)]))]
         [k->item (lambda (k) (values k (k->T k)))])
    (hash
      (map k->item (hash-keys env)))))
(define fun-base-Tenv
  (hash-ref* base-Tenv
             (list 'equal?
                   '+
                   '-
                   '*
                   '/
                   'pause)))
(define (check [e : Term]) : Void
  (let* ([_ (tc fun-base-Tenv e)])
    (void)))
(define (shallow-tc [e : Term])
  (type-case Term e
    ((t-fun _name _arg* _def* _body)
     (T-fun))
    (else
     (T-val))))
(define (tc [env : (Hashof Id Type)] [e : Term]) : Type
  (type-case Term e
    ((t-quote it)
     (T-val))
    ((t-var x)
     (type-case (Optionof Type) (hash-ref env x)
       ((none)
        (raise (exn-tc (format "unbound-id ~a" x))))
       ((some T)
        T)))
    ((t-fun name arg* def* body)
     (let* ([env (hash-set* env (map (λ (arg) (values arg (T-val))) arg*))]
            [env (hash-set* env (map (λ (def) (values (fst def) (shallow-tc (snd def)))) def*))]
            [init* (map (λ (def)
                          (let* ([x (fst def)]
                                 [e (snd def)]
                                 [expected (tc env (t-var x))]
                                 [actual (tc env e)])
                            (as-expected expected actual)))
                        def*)]
            [body (tc env body)]
            [body (as-val body)])
       (T-fun)))
    ((t-app fun arg*)
     (let* ([_ (tc env fun)]
            [_ (map (λ (arg) (as-val (tc env arg))) arg*)])
       (T-val)))
    ((t-let bind* body)
     (raise (exn-tc "`let` is not supported by smol/fun.")))
    ((t-letrec bind* body)
     (let* ([⟨x×T⟩* (map (λ (bind)
                           (values (fst bind) (type-of (snd bind))))
                         bind*)]
            [env_new (hash-set* env ⟨x×T⟩*)]
            [_ (map (λ (bind)
                      (tc env_new (snd bind)))
                    bind*)])
       (as-val (tc env_new body))))
    ((t-letrec-1 bind* body)
     (raise (exn-internal 'compile "This is impossible")))
    ((t-fun-call bind* body)
     (raise (exn-internal 'compile "This is impossible")))
    ((t-set! var val)
     (raise (exn-tc "`set!` is not supported by smol/fun.")))
    ((t-begin prelude* result)
     (let* ([_ (map (λ (prelude)
                      (tc env prelude))
                    prelude*)])
       (tc env result)))
    ((t-if cnd thn els)
     (let* ([_ (as-val (tc env cnd))]
            [_ (as-val (tc env thn))]
            [_ (as-val (tc env els))])
       (T-val)))
    ((t-show val)
     (as-val (tc env val)))))
(define (type-of e)
  (type-case Term e
    ((t-fun name arg* def* body)
     (T-fun))
    (else
     (T-val))))
(define (as-val T)
  (type-case Type T
    ((T-val)
     (T-val))
    (else
     (raise (exn-rt "functions are not values")))))
(define (as-expected T1 T2)
  (if (equal? T1 T2)
      T1
      (raise (exn-rt "functions are not values"))))
