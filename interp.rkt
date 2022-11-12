#lang racket
(require racket/match)
(provide (all-defined-out))

(define-values
  (pause! resume!)
  (let ([store #f])
    (define (pause! thunk) (set! store thunk))
    (define (resume!)
      (define thunk store)
      (set! store #f)
      (and thunk (thunk)))
    (values pause! resume!)))

(define the-stack (box (list)))
(define the-heap (make-hash (list)))
(define the-cont (box (list)))
(define the-env (box (hash)))
(define (current-stack) (unbox the-stack))
(define (current-heap) the-heap)
(define (current-cont) (unbox the-cont))
(define (current-env) (unbox the-env))

(define (pop-cont!)
  (match (unbox the-cont)
    [`() #f]
    [`(,f ,@cont)
     (set-box! the-cont cont)
     f]))
(define (add-cont! ectx-frame)
  (set-box! the-cont
            (cons ectx-frame (unbox the-cont))))

(define (push-stack! env)
  (set-box! the-stack (cons (list (unbox the-cont) (unbox the-env)) (unbox the-stack)))
  (set-box! the-cont (list))
  (set-box! the-env env))
(define (pop-stack!)
  (match (unbox the-stack)
    [`() #f]
    [`((,cont ,env) ,@stack)
     (set-box! the-stack stack)
     (set-box! the-cont cont)
     (set-box! the-env env)]))

(define (lookup x)
  (hash-ref (unbox the-env) x))

(define (allocate! hv)
  (let ([k (new-addr)])
    (hash-set! the-heap k hv)
    k))

(define (new-addr)
  (let ([old-addrs (hash-keys the-heap)])
    (let loop ()
      (define addr (random 100 1000))
      (if (member addr old-addrs)
          (loop)
          addr))))

(define (interp expr)
  (match expr
    [`(lambda (,x) ,body)
     (continue (allocate! `(clos ,expr ,(current-env))))]
    [`(,rator ,rand)
     (add-cont! `(app-1 ,rand))
     (interp rator)]
    [`,n
     #:when (number? n)
     (continue n)]
    [`,x
     #;when (symbol? x)
     (continue (lookup x))]))

(define (continue v)
  (pause!
   (lambda ()
     (match (pop-cont!)
       [#f
        (return v)]
       [`(app-1 ,rand)
        (define fun v)
        (add-cont! `(app-2 ,fun))
        (interp rand)]
       [`(app-2 ,fun)
        (define arg v)
        (match-define `(clos (lambda (,x) ,body) ,env) (as-fun fun))
        (push-stack! (hash-set env x arg))
        (interp body)]))))

(define (as-fun v)
  (hash-ref the-heap v))

(define (return v)
  (if (pop-stack!)
      (continue v)
      (begin
        (displayln v)
        (displayln (current-heap)))))

(define (all-the-way-to-the-end)
  (and (resume!) (all-the-way-to-the-end)))

;;; (interp '(((lambda (x) (lambda (y) x)) 2) 3))
;;; (interp '((lambda (x) (lambda (y) x)) 2))
;;; (interp '(lambda (x) (lambda (y) x)))
;;; (interp '((lambda (x) x) 2))
;;; (interp '(lambda (x) x))
(all-the-way-to-the-end)
