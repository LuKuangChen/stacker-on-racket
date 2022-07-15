#lang racket

(define enable-tco? #t)
(define hide-closure? #f)
(define hide-env-label? #t)
(define hide-fun-addr? #t)
(define defvar-lambda-as-deffun? #f)
(define set!-lambda-as-def? #f)
(define set!-other-as-def? #f)

(include "../general-semantics.txt")
