#lang racket
(require "./utilities.rkt")

(test-equivalent '((map (λ (x) (* x x)) (list 1 2 4))))

(test-equivalent '((deffun (hof-test-f g)
                     (g 3))
                   (deffun (hof-test-h x)
                     (+ x 1))
                   (hof-test-f hof-test-h)))

(test-equivalent '((deffun (nested-i x)
                     (deffun (nested-j y)
                       (+ y 1))
                     (nested-j (+ x 1)))
                   (nested-i 5)))


(test-equivalent '((mpair (lambda () 0) 0)))

(test-equivalent '((letrec ([fact
                             (lambda (n)
                               (if (zero? n)
                                   1
                                   (* n (fact (- n 1)))))])
                     (fact 5))))