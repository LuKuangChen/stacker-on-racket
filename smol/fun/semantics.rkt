#lang racket
(provide (rename-out [my-module-begin #%module-begin])
         (rename-out [my-top-interaction #%top-interaction]))

(require "../../s-exp-of-state.rkt")
(require "../../pict-of-state.rkt")
(require "../../parse.rkt")
(require "../../runtime.rkt")
(require "../../string-of-state.rkt")

(define (defvar-lambda-as-deffun s-exp)
  (define (rec s-exp)
    (match s-exp
      [`(defvar ,x (,lambda (,@args) ,@body))
       #:when (memv lambda '(lambda λ))
       `(deffun (,x ,@args) ,@(map rec body))]
      [else
       (if (list? s-exp)
           (map rec s-exp)
           s-exp)]))
  (rec s-exp))
(define (set!-as-def-1 s-exp)
  (define (rec s-exp)
    (match s-exp
      [`(set! ,x (,lambda (,@args) ,@body))
       #:when (memv lambda '(lambda λ))
       `(deffun (,x ,@args) ,@(map rec body))]
      [`(set! ,x ,e)
       `(defvar ,x ,(rec e))]
      [else
       (if (list? s-exp)
           (map rec s-exp)
           s-exp)]))
  (rec s-exp))
(define (begin-as-block s-exp)
  (define (rec s-exp)
    (match s-exp
      [`(begin ,@e)
       `(,block ,@(map rec e))]
      [else
       (if (list? s-exp)
           (map rec s-exp)
           s-exp)]))
  (rec s-exp))
(define preprocess (compose defvar-lambda-as-deffun set!-as-def-1 begin-as-block))
(define (my-pict-of-state state)
  ((pict-of-state #t #t) (preprocess ((s-exp-of-state #f) state))))

(define (run tracing? e)
  (define check void)
  (eval tracing? check my-pict-of-state (parse e)))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ #:no-trace form ...)
     #'(#%module-begin (run #f '(form ...)))]
    [(_ form ...)
     #'(#%module-begin (run #t '(form ...)))]))

(define-syntax (my-top-interaction stx)
  (syntax-case stx ()
    [(_ . form)
     #'(#%top-interaction . (displayln "Please run programs in the editor window."))]))
