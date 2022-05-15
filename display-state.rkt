#lang plait
(require "utilities.rkt")
(require "datatypes.rkt")
(require (typed-in "pict-state.rkt"
                   [pict-state : (Any Any Any Any -> Void)]))
(require (typed-in racket
                   [number->string : (Number -> String)]
                   [vector-map : (('a -> 'b) (Vectorof 'a) -> (Vectorof 'b))]
                   [sort : ((Listof 'x) ('x 'x -> Boolean) -> (Listof 'x))]))
(require (opaque-type-in racket [Any any/c]))
(require (rename-in (typed-in racket [identity : ('a -> Any)]) [identity inj]))

(define (my-display-state e env ectx stack heap)
  (pict-state (s-exp-of-env env) (s-exp-of-ectx ectx) (s-exp-of-stack stack) (s-exp-of-heap heap)))
(define (s-exp-of-stack stack)
  (inj (map s-exp-of-sf stack)))
(define (my-display-e e)
  (begin
    (display (format "~a" (s-exp-of-e e)))
    (display "\n")))
(define (my-display-stack [stack : Stack])
  (begin
    (map (lambda (sf)
           (begin
             (display (s-exp-of-sf sf))
             (display "\n"))) stack)
    (void)))
(define (my-display-env env)
  (begin
    (display (format "~a" (s-exp-of-env env)))
    (display "\n")))
(define (my-display-ectx ectx)
  (begin
    (display (format "~a" (s-exp-of-ectx ectx)))
    (display "\n")))
(define (my-display-heap heap)
  (map
   (lambda (key)
     (type-case HeapAddress key
       [(ha-user _)
        (begin
          (display (s-exp-of-addr key))
          (display " = ")
          (display (s-exp-of-hv (some-v (hash-ref heap key))))
          (display "\n"))]
       [(ha-prim _) (void)]))
   ;; remove the base environment
   (reverse (hash-keys heap))))
(define (s-exp-of-heap heap)
  (inj
   (map
    (lambda (key)
      (inj (list (s-exp-of-addr key)
                 (s-exp-of-hv (some-v (hash-ref heap key))))))
    (reverse (hash-keys heap)))))
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
    ((h-list vs)
     (inj (map s-exp-of-v vs)))
    ((h-fun env name arg* def* body)
     (inj (list (inj 'closure)
                (s-exp-of-env env)
                (inj (s-exp-of-funname name))
                (inj (map s-exp-of-x arg*))
                (inj (map s-exp-of-def def*))
                (s-exp-of-e body))))))
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
      ((F-show!) □)
      ((F-let xv* x xe* body)
       (inj (list (inj 'let)
                  (inj
                   (append
                    (map s-exp-of-xv xv*)
                    (cons (inj (list (s-exp-of-x x) □))
                          (map s-exp-of-xe xe*))))
                  (s-exp-of-e body))))
      ((F-letrec-1 x xe* body)
       (inj (list (inj 'letrec-1)
                  (inj
                   (cons (inj (list (s-exp-of-x x) □))
                         (map s-exp-of-xe xe*)))
                  (s-exp-of-e body))))
      ((F-fun-call x xe* body)
       (inj (list (inj 'fun-call)
                  (inj
                   (cons (inj (list (s-exp-of-x x) □))
                         (map s-exp-of-xe xe*)))
                  (s-exp-of-e body))))
      ((F-if thn els)
       (inj (list (inj 'if) □ (s-exp-of-e thn) (s-exp-of-e els))))
      ((F-set! var)
       (inj (list (inj 'set!) (s-exp-of-x var) □))))))
(define (s-exp-of-prim p)
  (type-case PrimitiveOp p
    [(po-+)
     (inj '+)]
    [(po--)
     (inj '-)]
    [(po-*)
     (inj '*)]
    [(po-/)
     (inj '/)]
    [(po-pairp)
     (inj 'pair?)]
    [(po-pair)
     (inj 'pair)]
    [(po-left)
     (inj 'left)]
    [(po-right)
     (inj 'right)]
    [(po-ivec)
     (inj 'ivec)]
    [(po-list)
     (inj 'list)]
    [(po-pause)
     (inj 'pause)]
    [(po-equalp)
     (inj 'equal?)]))
(define (s-exp-of-addr it)
  (type-case HeapAddress it
    [(ha-user it)
     (let ([printing (format "@~a" (inj it))])
       (type-case HeapValue (some-v (hash-ref the-heap (ha-user it)))
         ((h-fun env name arg* def* body)
          (type-case (Optionof Symbol) name
            ((none)
             (inj (string->symbol printing)))
            ((some s)
             (let ([printing (string-append printing (format ":~a" (inj s)))])
               (inj (string->symbol printing))))))
         (else
          (inj (string->symbol printing)))))]
    [(ha-prim it)
     (inj (string->symbol (format "@~a" (inj it))))]))
(define (s-exp-of-v v)
  (type-case Val v
    ((v-addr it)
     (s-exp-of-addr it))
    ((v-prim name)
     (s-exp-of-prim name))
    ((v-str it)
     (inj it))
    ((v-num it)
     (inj it))
    ((v-bool it)
     (inj it))
    ((v-void)
     (inj '_))))
(define (s-exp-of-x x) (inj x))
(define (s-exp-of-def def)
  (local ((define-values (x e) def))
    (inj (list (inj 'defvar)
               (s-exp-of-x x)
               (s-exp-of-e e)))))
(define (s-exp-of-e e)
  (type-case Term e
    [(t-quote v)
     ;;;  (inj (list (inj 'quote) (s-exp-of-v v)))]
     (s-exp-of-v v)]
    [(t-var x)
     (s-exp-of-x x)]
    [(t-fun name args def* body)
     (inj
      (list (inj 'lambda)
            (inj (map s-exp-of-x args))
            (inj (map s-exp-of-def def*))
            (s-exp-of-e body)))]
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
    [(t-letrec-1 bind* body)
     (inj
      (list (inj 'letrec-1)
            (inj (map s-exp-of-xe bind*))
            (s-exp-of-e body)))]
    [(t-fun-call bind* body)
     (inj
      (list (inj 'fun-call)
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
       (s-exp-of-e els)))]
    [(t-show e)
     (s-exp-of-e e)]
    ))
(define (s-exp-of-xe xe)
  (inj (list (s-exp-of-x (fst xe)) (s-exp-of-e (snd xe)))))