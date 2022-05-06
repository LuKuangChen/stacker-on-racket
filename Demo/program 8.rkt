#lang smol-step

#|
# Program 8

fun k(i, j):
  i + g(j - 1) + h(j)

fun g(w):
  w+7

fun h(a):
  z = a+1
  a + pause() + z

k(8, 14)
|#

(deffun (k i j)
  (+ i (+ (g (- j 1)) (h j))))

(deffun (g w)
  (+ w 7))

(deffun (h a)
  (defvar z (+ a 1))
  (+ a (+ (pause) z)))

(k 8 14)