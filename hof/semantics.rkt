#lang racket
(provide (rename-out [my-module-begin #%module-begin])
         (rename-out [my-top-interaction #%top-interaction]))

(require "../pict-of-state.rkt")
(require "../checker.rkt")
(require "../parse.rkt")
(require "../show.rkt")
(require "../runtime.rkt")

(define (run tracing? e)
  (parameterize ([hide-closure #f])
    (let* ([o* (eval tracing? check pict-of-state (parse e))]
           [o* (show o*)])
      (for-each displayln o*))))

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
