#lang racket
(provide (rename-out [my-module-begin #%module-begin])
         (rename-out [my-top-interaction #%top-interaction]))

(require "../pict-state.rkt")
(require "./checker.rkt")
(require "../parse.rkt")
(require "../show.rkt")
(require "../runtime.rkt")

(define (run tracing? e)
  (let* ([_ (when tracing? (start-trace))]
         [o* (eval tracing? check pict-state (parse e))]
         [o* (show o*)]
         [_ (when tracing?
              (pict-terminated (string-join (map (curry format "~a") o*) "\n")))])
    (for-each displayln o*)))

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
