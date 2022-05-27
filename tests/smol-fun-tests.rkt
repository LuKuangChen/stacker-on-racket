#lang racket
(require "./utilities.rkt")

(test-equivalent '((deffun (fact n)
                     (if (zero? n)
                         1
                         (* n (fact (- n 1)))))
                   (fact 10)))

(test-equivalent '((deffun (odd? n)
                     (if (zero? n)
                         false
                         (even? (- n 1))))

                   (deffun (even? n)
                     (if (zero? n)
                         true
                         (odd? (- n 1))))
                   (odd? 10)
                   (even? 10)))


(test-equivalent '((defvar x 3)
                   (deffun (f x) (+ x x))
                   (f 5)
                   (let ([x 8] [y x]) x)))


(test-equivalent '((defvar l (mpair "a" (mpair "b" (mpair "c" 0))))
                   (deffun (len l)
                     (if (not (pair? l))
                         0
                         (+ 1 (len (right l)))))
                   (equal? l #("a" #("b" #("c" 0))))
                   (eq? l #("a" #("b" #("c" 0))))
                   (len 0)
                   (len l)))

(test-equivalent '((deffun (nested-i x)
                     (deffun (nested-j y)
                       (+ y 1))
                     (nested-j (+ x 1)))

                   (nested-i 5)))


(test-equivalent '((left (mvec 1 2 3))))
(test-equivalent '((right (mvec 1 2 3))))
