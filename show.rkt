#lang racket
;; input/output

(provide show)
(require "io.rkt")

(define (show o*)
  (map s-exp-of-o o*))
(define (s-exp-of-o o)
  (cond
    [(o-exn? o) (string->symbol (format "Error" #;(o-exn-it o)))]
    [(o-con? o) (s-exp-of-c (o-con-it o))]
    [(o-vec? o) `(quote ,(vector-map s-exp-of-o (o-vec-it o)))]
    [(o-list? o) `(quote ,(map s-exp-of-o (o-list-it o)))]
    [(o-fun? o) (λ (x) x)]
    [else (error 'show "internal error ~a" o)]))
(define (s-exp-of-c c)
  (cond
    [(c-str? c) (c-str-it c)]
    [(c-num? c) (c-num-it c)]
    [(c-bool? c) (c-bool-it c)]
    [else (error 'show "internal error" c)]))
