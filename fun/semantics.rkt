#lang racket
(provide (rename-out [my-module-begin #%module-begin])
         (rename-out [my-top-interaction #%top-interaction]))

(require "./pict-state.rkt")
(require "./checker.rkt")
(require "../parse.rkt")
(require "../show.rkt")
(require "../runtime.rkt")

(define (run e)
  (let* ([o* (eval check pict-state (parse e))]
         [o* (show o*)])
    (for-each writeln o*)))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     #'(#%module-begin (run '(form ...)))]))

(define-syntax (my-top-interaction stx)
  (syntax-case stx ()
    [(_ . form)
     #'(#%top-interaction . (displayln "Please run programs in the editor window."))]))
