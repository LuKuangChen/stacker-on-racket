#lang racket
(provide (rename-out [my-module-begin #%module-begin])
         (rename-out [my-top-interaction #%top-interaction]))

(require "../pict-of-state.rkt")
(require "../parse.rkt")
(require "../show.rkt")
(require "../runtime.rkt")
(require "../string-of-state.rkt")

(define preprocess identity)
(define (my-pict-of-state message term env ectx stack pctx heap)
  ((pict-of-state #f) message (preprocess term) env (preprocess ectx) (preprocess stack) (preprocess pctx) heap))

(define (run tracing? e)
  ;; don't check anything, including scope
  ;; TODO: make sure lambda and other banned operators are not there.
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
