#lang racket
;; input/output

(provide show)
(require "io.rkt")

(define (show o*)
  (map s-exp-of-o (filter (not/c o-void?) o*)))
(define (s-exp-of-o o)
  (let ([r (s-exp-of-o-1 o)])
    (if ((or/c list? vector?) r)
        `(quote ,r)
        r)))
(define (s-exp-of-o-1 o)
  (cond
    ;;; [(o-exn? o) (string->symbol (format "Error" #;(o-exn-it o)))]
    [(o-exn? o)
    ;;;  (displayln (o-exn-it o))
     (string->symbol (format "Error" #;(o-exn-it o)))]
    [(o-con? o) (s-exp-of-c (o-con-it o))]
    [(o-vec? o) (vector-map s-exp-of-o-1 (o-vec-it o))]
    [(o-list? o) (map s-exp-of-o-1 (o-list-it o))]
    [(o-fun? o) (λ (x) x)]
    [else (error 'show "internal error ~a" o)]))
(define (s-exp-of-c c)
  (cond
    [(c-str? c) (c-str-it c)]
    [(c-num? c) (c-num-it c)]
    [(c-bool? c) (c-bool-it c)]
    [else (error 'show "internal error" c)]))
