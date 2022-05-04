#lang plait
(require "datatypes.rkt")
(require (typed-in racket [number->string : (Number -> String)]
                   [format : (String Any -> String)]))
(require (opaque-type-in pict [Pict pict?])
         (opaque-type-in racket [Any any/c]))
(require (typed-in pict
                   [text : (String Symbol -> Pict)]
                   [show-pict : (Pict -> Void)]))
(require (rename-in (typed-in racket [identity : ('a -> Any)]) [identity inj]))

(define (my-display-e e)
  (begin
    (display (format "~a" (show-e e)))
    (display "\n")))
(define (my-display-stack stack)
  (begin
    (display (format "~a" (inj (map show-sf stack))))
    (display "\n")))
(define (my-display-ectx ectx)
  (begin
    (display (format "~a" (show-ectx ectx)))
    (display "\n")))
(define (show-sf sf)
  (let ([env (fst sf)]
        [ectx (snd sf)])
    (inj (list (show-env env)
               (show-ectx ectx)))))
(define (show-env env)
  ;;; TODO
  (inj 'env-x))
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
    [(po-equalp)
     (inj 'equal?)]))
(define (show-v v)
  (type-case Val v
    ((v-addr it)
     (inj (string->symbol (string-append "%" (number->string it)))))
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
(define (show-e e)
  (type-case Term e
    [(t-quote v)
     (show-v v)]
    [(t-var x)
     (show-x x)]
    [(t-fun args body)
     (inj
      (list (inj 'lambda)
            (inj (map show-x args))
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