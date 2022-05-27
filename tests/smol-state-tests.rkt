#lang racket
(require "./utilities.rkt")
(test-equivalent '((defvar p (mpair "a" "b"))
                   (left p)
                   (right p)
                   (set-left! p "x")
                   (set-right! p "y")
                   (left p)
                   (right p)
                   (vlen p)
                   (pair? p)
                   (pair? (mvec "g" "h"))))

(test-equivalent '((defvar v 2)
                   v
                   (set! v 3)
                   v))

(test-equivalent '((defvar y (mpair 1 2))
                   (defvar z y)
                   y
                   z
                   (set! y (mpair 3 4))
                   y
                   z))

(test-equivalent '((defvar w (mvec 1 2 3))
                   (vset! w 2 4)
                   w))

(test-equivalent '((deffun (nested-i x)
                     (deffun (nested-j y)
                       (+ y 1))
                     (nested-j (+ x 1)))
                   (nested-i 5)))

(test-equivalent '((set-left! (mvec 1 2 3) 0)))
(test-equivalent '((set-right! (mvec 1 2 3) 0)))
