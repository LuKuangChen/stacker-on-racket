#lang racket
(provide (rename-out [my-module-begin #%module-begin])
         (rename-out [my-top-interaction #%top-interaction]))

(require "../pict-of-state.rkt")
(require "../s-exp-of-state.rkt")
(require "../parse.rkt")
(require "../show.rkt")
(require "../runtime.rkt")
(require "../string-of-state.rkt")

(define preprocess (compose))
(define (my-pict-of-state state)
  ((pict-of-state #f #f) (preprocess ((s-exp-of-state #t) state))))

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
