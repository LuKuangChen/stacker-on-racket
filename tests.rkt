#lang racket
(require "parse.rkt")
(require "show.rkt")
(require "interp-small-step.rkt")
(require rackunit)

(define (run e)
  (show (eval (parse e))))

;; tests from SMoL/fun
;; one uninteresting test is commented out

(test-equal? "arithmetic operators"
             (run '((deffun (f o) (o 1 1))
                    (f +)))
             '(Error))
(test-equal? "0 as condition"
             (run '((if 0 #t #f)))
             '(#t))
(test-equal? "redeclare var using defvar"
             (run '((defvar x 0)
                    (defvar y x)
                    (defvar x 2)
                    x
                    y))
             '(Error))
(test-equal? "expose local defvar"
             (run '((defvar x 42)
                    (deffun (create)
                      (defvar y 42)
                      y)

                    (create)
                    (equal? x y)))
             '(Error))
(test-equal? "pair?"
             (run '((pair? (pair 1 2))
                    (pair? (ivec 1 2))
                    (pair? '#(1 2))
                    (pair? '(1 2))))
             '(#t #t #t #f))
(test-equal? "let* and let"
             (run '((let* ([v 1]
                           [w (+ v 2)]
                           [y (* w w)])
                      (let ([v 3]
                            [y (* v w)])
                        y))))
             '(3))

(test-equal? "defvar and let"
             (run '((defvar x 3)
                    (defvar y (let ([y 6] [x 5]) x))
                    (* x y)))
             '(15))
(test-equal? "fun-id equals to arg-id"
             (run '((deffun (f f) f)
                    (f 5)))
             '(5))
(test-equal? "scoping rule of let"
             (run '((let ([x 4]
                          [y (+ x 10)])
                      y)))
             '(Error))
(test-equal? "the right component of ivec"
             (run '(right (ivec 1 2 3)))
             '(Error))
(test-equal? "identifiers"
             (run '((defvar x 5)

                    (deffun (reassign var_name new_val)
                      (defvar var_name new_val)
                      (pair var_name x))

                    (reassign x 6)
                    x))
             '('#(6 5) 5))
(test-equal? "defvar, deffun, and let"
             (run '((defvar a 1)
                    (deffun (what-is-a) a)
                    (let ([a 2])
                      (ivec
                       (what-is-a)
                       a))))
             '('#(1 2)))
(test-equal? "syntax pitfall"
             (run '((deffun (f a b) a + b)
                    (f 5 10)))
             '(10))
