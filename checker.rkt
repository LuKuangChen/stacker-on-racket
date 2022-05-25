#lang plait

;; This checker checks well-scopedness. It is used by mut-vars, vectors, and hof.
;; fun has its own checker because it bans first-class functions.

(require "./utilities.rkt")
(require "./error.rkt")
(require "./datatypes.rkt")

(define base-Tenv
  (let* ([env (some-v base-env)]
         [env (some-v (hash-ref the-heap env))]
         [env (h-env-map env)]
         [k->item (lambda (k) (values k (void)))])
    (hash
     (map k->item (hash-keys env)))))
(define (check [e : Term]) : Void
  (let* ([_ (tc base-Tenv e)])
    (void)))
(define (tc [env : (Hashof Id Void)] [e : Term]) : Void
  (type-case Term e
    ((t-quote it) (void))
    ((t-var x)
     (type-case (Optionof Void) (hash-ref env x)
       ((none)
        (raise (exn-tc (format "unbound-id ~a" x))))
       ((some T)
        T)))
    ((t-fun name arg* def* body)
     (let* ([env (hash-set* env (map (λ (arg) (values arg (void))) arg*))]
            [env (hash-set* env (map (λ (def) (values (fst def) (void))) def*))]
            [init* (map (λ (def)
                          (let* ([x (fst def)]
                                 [e (snd def)]
                                 [expected (tc env (t-var x))]
                                 [actual (tc env e)])
                            (void)))
                        def*)]
            [body (tc env body)])
       (void)))
    ((t-app fun arg*)
     (let* ([_ (tc env fun)]
            [_ (map (λ (arg) (tc env arg)) arg*)])
       (void)))
    ((t-let bind* body)
     (let* ([⟨x×T⟩* (map (λ (bind)
                           (values (fst bind) (tc env (snd bind))))
                         bind*)]
            [env_new (hash-set* env ⟨x×T⟩*)])
       (tc env_new body)))
    ((t-letrec bind* body)
     (let* ([⟨x×T⟩* (map (λ (bind)
                           (values (fst bind) (void)))
                         bind*)]
            [env_new (hash-set* env ⟨x×T⟩*)]
            [_ (map (λ (bind)
                      (tc env_new (snd bind)))
                    bind*)])
       (tc env_new body)))
    ((t-letrec-1 bind* body)
     (raise (exn-internal 'compile "This is impossible")))
    ((t-fun-call bind* body)
     (raise (exn-internal 'compile "This is impossible")))
    ((t-set! var val)
     (let* ([_ (tc env val)]
            [_ (tc env (t-var var))])
       (void)))
    ((t-begin prelude* result)
     (let* ([_ (map (λ (prelude)
                      (tc env prelude))
                    prelude*)])
       (tc env result)))
    ((t-if cnd thn els)
     (let* ([_ (tc env cnd)]
            [_ (tc env thn)]
            [_ (tc env els)])
       (void)))
    ((t-show val)
     (let* ([val (tc env val)])
       val))))
