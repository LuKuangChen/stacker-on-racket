#lang racket
;; input/output

(provide show)
(require "io.rkt")

(define (show o*)
  (map string-of-o (filter (not/c o-void?) o*)))
(define (string-of-o o)
  (cond
    ;;; [(o-exn? o) (string->symbol (format "Error" #;(o-exn-it o)))]
    [(o-exn? o)
    ;;;  (displayln (o-exn-it o))
     (format "error: ~a" (o-exn-it o))]
    [(o-con? o) (string-of-c (o-con-it o))]
    [(o-vec? o) (format "#(~a)" (string-join " " (vector->list (vector-map string-of-o (o-vec-it o)))))]
    ;;; [(o-list? o) (map string-of-o (o-list-it o))]
    [(o-fun? o) "#<procedure>"]
    [else (error 'show "internal error ~a" o)]))
(define (string-of-c c)
  (cond
    [(c-str? c) (format "~a" (c-str-it c))]
    [(c-num? c) (format "~a" (c-num-it c))]
    [(c-bool? c) (format "~a" (c-bool-it c))]
    [else (error 'show "internal error" c)]))
