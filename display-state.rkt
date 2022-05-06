#lang plait
(require "datatypes.rkt")
(require (typed-in "pict-state.rkt"
                   [pict-state : (Any Any Any Any -> Void)]))
(require (typed-in racket
                   [number->string : (Number -> String)]
                   [vector-map : (('a -> 'b) (Vectorof 'a) -> (Vectorof 'b))]
                   [format : (String Any -> String)]
                   [sort : ((Listof 'x) ('x 'x -> Boolean) -> (Listof 'x))]))
(require (opaque-type-in pict [Pict pict?])
         (opaque-type-in racket [Any any/c]))
(require (typed-in pict
                   [text : (String Symbol -> Pict)]
                   [show-pict : (Pict -> Void)]))
(require (rename-in (typed-in racket [identity : ('a -> Any)]) [identity inj]))

(define (my-display-state env ectx stack heap)
  (pict-state (show-env env) (show-ectx ectx) (show-stack stack) (show-heap heap)))
(define (show-stack stack)
  (inj (map show-sf stack)))
(define (my-display-e e)
  (begin
    (display (format "~a" (show-e e)))
    (display "\n")))
(define (my-display-stack [stack : Stack])
  (begin
    (map (lambda (sf)
           (begin
             (display (show-sf sf))
             (display "\n"))) stack)
    (void)))
(define (my-display-env env)
  (begin
    (display (format "~a" (show-env env)))
    (display "\n")))
(define (my-display-ectx ectx)
  (begin
    (display (format "~a" (show-ectx ectx)))
    (display "\n")))
(define (my-display-heap heap)
  (map
   (lambda (key)
     (type-case HeapAddress key
       [(ha-user _)
        (begin
          (display (show-addr key))
          (display " = ")
          (display (show-hv (some-v (hash-ref heap key))))
          (display "\n"))]
       [(ha-prim _) (void)]))
   ;; remove the base environment
   (reverse (hash-keys heap))))
(define (show-heap heap)
  (inj
   (map
    (lambda (key)
      (inj (list (show-addr key)
                 (show-hv (some-v (hash-ref heap key))))))
    (reverse (hash-keys heap)))))
(define (show-hv hv): Any
  (type-case HeapValue hv
    ((h-env env map)
     (inj
      (list (inj 'Environment)
            (inj (ind-List (hash-keys map)
                           (list)
                           (lambda (IH k)
                             (cons (inj (list (inj k) (show-optionof-v (some-v (hash-ref map k)))))
                                   IH))))
            (show-env env))))
    ((h-vec vs)
     (inj (vector-map show-v vs)))
    ((h-list vs)
     (inj (map show-v vs)))
    ((h-fun env name arg* def* body)
     (inj (list (inj 'closure)
                (show-env env)
                (inj (show-funname name))
                (inj (map show-x arg*))
                (inj (map show-def def*))
                (show-e body))))))
(define (show-funname nm)
  (type-case (Optionof Symbol) nm
    [(none) (inj '_)]
    [(some s) (inj s)]))
(define (show-optionof-v ov)
  (type-case (Optionof Val) ov
    [(none)
     (inj '_)]
    [(some v)
     (show-v v)]))
(define (show-sf sf)
  (local ((define-values (env ectx ann) sf))
    (inj (list (show-env env)
               (show-ectx ectx)
               (show-ann ann)))))
(define (show-ann ann)
  (type-case CtxAnn ann
    ((ca-let)
     (inj 'let))
    ((ca-letrec)
     (inj 'letrec))
    ((ca-app fun arg*)
     (inj (cons (show-v fun) (map show-v arg*))))))
(define (show-env env): Any
  (type-case Env env
    ((none)
     (inj '_))
    ((some addr)
     (show-addr addr))))
(define (show-ectx ectx)
  (inj (ind-List (reverse (map show-f ectx))
                 (inj '□)
                 (lambda (IH x)
                   (x IH)))))
(define (show-xv xv)
  (inj (list (show-x (fst xv)) (show-v (snd xv)))))
(define (show-f f)
  (lambda ([□ : Any])
    (type-case ECFrame f
      ((F-begin prelude* result)
       (inj (append
             (list (inj 'begin) □)
             (append
              (map show-e prelude*)
              (list (show-e result))))))
      ((F-app v* e*)
       (inj (append
             (map show-v v*)
             (cons
              □
              (map show-e e*)))))
      ((F-show!) □)
      ((F-let xv* x xe* body)
       (inj (list (inj 'let)
                  (inj
                   (append
                    (map show-xv xv*)
                    (cons (inj (list (show-x x) □))
                          (map show-xe xe*))))
                  (show-e body))))
      ((F-letrec-1 x xe* body)
       (inj (list (inj 'letrec-1)
                  (inj
                   (cons (inj (list (show-x x) □))
                         (map show-xe xe*)))
                  (show-e body))))
      ((F-fun-call x xe* body)
       (inj (list (inj 'fun-call)
                  (inj
                   (cons (inj (list (show-x x) □))
                         (map show-xe xe*)))
                  (show-e body))))
      ((F-if thn els)
       (inj (list (inj 'if) □ (show-e thn) (show-e els))))
      ((F-set! var)
       (inj (list (inj 'set!) (show-x var) □))))))
(define (show-prim p)
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
(define (show-addr it)
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
(define (show-v v)
  (type-case Val v
    ((v-addr it)
     (show-addr it))
    ((v-prim name)
     (show-prim name))
    ((v-str it)
     (inj it))
    ((v-num it)
     (inj it))
    ((v-bool it)
     (inj it))
    ((v-void)
     (inj '_))))
(define (show-x x) (inj x))
(define (show-def def)
  (local ((define-values (x e) def))
    (inj (list (inj 'defvar)
               (show-x x)
               (show-e e)))))
(define (show-e e)
  (type-case Term e
    [(t-quote v)
     ;;;  (inj (list (inj 'quote) (show-v v)))]
     (show-v v)]
    [(t-var x)
     (show-x x)]
    [(t-fun name args def* body)
     (inj
      (list (inj 'lambda)
            (inj (map show-x args))
            (inj (map show-def def*))
            (show-e body)))]
    [(t-app fun arg*)
     (inj (map show-e (cons fun arg*)))]
    [(t-let bind* body)
     (inj
      (list (inj 'let)
            (inj (map show-xe bind*))
            (show-e body)))]
    [(t-letrec bind* body)
     (inj
      (list (inj 'letrec)
            (inj (map show-xe bind*))
            (show-e body)))]
    [(t-letrec-1 bind* body)
     (inj
      (list (inj 'letrec-1)
            (inj (map show-xe bind*))
            (show-e body)))]
    [(t-fun-call bind* body)
     (inj
      (list (inj 'fun-call)
            (inj (map show-xe bind*))
            (show-e body)))]
    [(t-set! x e)
     (inj
      (list
       (inj 'set!)
       (show-x x)
       (show-e e)))]
    [(t-begin e* e)
     (inj
      (cons
       (inj 'begin)
       (map show-e (append e* (list e)))))]
    [(t-if cnd thn els)
     (inj
      (list
       (inj 'if)
       (show-e cnd)
       (show-e thn)
       (show-e els)))]
    [(t-show e)
     (show-e e)]
    ))
(define (show-xe xe)
  (inj (list (show-x (fst xe)) (show-e (snd xe)))))