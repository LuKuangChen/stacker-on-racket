#lang racket
;; input/output

(provide string-of-o)
(require "io.rkt")

(define (string-of-o o)
  (cond
    [(o-exn? o)
     "error"]
    [(o-con? o) (string-of-c (o-con-it o))]
    [(o-vec? o) (format "'#(~a)" (string-join (vector->list (vector-map string-of-o-internal (o-vec-it o))) " "))]
    [(o-list? o) (format "'(~a)" (string-join (map string-of-o-internal (o-list-it o)) " "))]
    [(o-fun? o) "#<procedure>"]
    [(o-void? o) "#<void>"]
    [(o-rec? o) (format "#~a=~a" (o-rec-id o) (string-of-o (o-rec-content o)))]
    [(o-var? o) (format "#~a#" (o-var-id o))]
    [else
     (displayln o)
     (displayln (o-rec? o))
     (error 'show "internal error ~a" o)]))
(define (string-of-o-internal o)
  (cond
    [(o-exn? o)
     (format "error: ~a" (o-exn-it o))]
    [(o-con? o) (string-of-c (o-con-it o))]
    [(o-vec? o) (format "#(~a)" (string-join (vector->list (vector-map string-of-o-internal (o-vec-it o))) " "))]
    [(o-list? o) (format "(~a)" (string-join (map string-of-o-internal (o-list-it o)) " "))]
    [(o-fun? o) "#<procedure>"]
    [(o-void? o) "#<void>"]
    [(o-rec? o) (format "#~a=~a" (o-rec-id o) (string-of-o-internal (o-rec-content o)))]
    [(o-var? o) (format "#~a#" (o-var-id o))]
    [else (error 'show "internal error ~a" o)]))
(define (string-of-c c)
  (define p (open-output-string))
  (write (pre-string-of-c c) p)
  (get-output-string p))
(define (pre-string-of-c c)
  (cond
    [(c-str? c) (c-str-it c)]
    [(c-num? c) (c-num-it c)]
    [(c-bool? c) (c-bool-it c)]
    [else (error 'show "internal error" c)]))
