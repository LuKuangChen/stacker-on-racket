#lang racket
(provide (rename-out [my-module-begin #%module-begin]))

(require "./checker.rkt")
(require "../parse.rkt")
(require "../show.rkt")
(require "../runtime.rkt")

(define (run e)
  (let* ([o* (eval check (parse e))]
         [o* (show o*)])
    (for-each writeln o*)))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     #'(#%module-begin (run '(form ...)))]))
