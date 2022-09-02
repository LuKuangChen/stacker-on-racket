#lang racket
(require "./utilities.rkt")

(test-expect/smol '((let ([x 2]
      [y (+ x 1)])
   (+ x y))
) "error")
(test-equivalent '((let ([x 2]
      [y (+ x 1)])
   (+ x y))
))
(test-expect/smol '((let ([x 123]
      [getx (lambda () x)])
  (getx))
) "error")
(test-equivalent '((let ([x 123]
      [getx (lambda () x)])
  (getx))
))
(test-expect/smol '((defvar x 3)
(defvar y (let ([x 5]) x))
(* x y)
) "15")
(test-equivalent '((defvar x 3)
(defvar y (let ([x 5]) x))
(* x y)
))
(test-expect/smol '((defvar x 1)
(defvar y 2)

(let ([x (+ y 1)]
      [y (+ x 1)])
  (mvec x y))
) "'#(3 2)")
(test-equivalent '((defvar x 1)
(defvar y 2)

(let ([x (+ y 1)]
      [y (+ x 1)])
  (mvec x y))
))
(test-expect/smol '((defvar x 1)
(defvar y 2)

(letrec ([x (+ y 1)]
         [y (+ x 1)])
  (mvec x y))
) "error")
(test-equivalent '((defvar x 1)
(defvar y 2)

(letrec ([x (+ y 1)]
         [y (+ x 1)])
  (mvec x y))
))
(test-expect/smol '((defvar x 1)
(defvar y 2)

(let* ([x (+ y 1)]
       [y (+ x 1)])
  (mvec x y))
) "'#(3 4)")
(test-equivalent '((defvar x 1)
(defvar y 2)

(let* ([x (+ y 1)]
       [y (+ x 1)])
  (mvec x y))
))
(test-expect/smol '((let (foo 41)
  (+ foo 1))
) "error")
(test-equivalent '((let (foo 41)
  (+ foo 1))
))
(test-expect/smol '((let ([x 2])
  (let ([y 30])
    (+ x y)))
) "32")
(test-equivalent '((let ([x 2])
  (let ([y 30])
    (+ x y)))
))
(test-expect/smol '((let ([a 2]
      [b (* a 3)])
   (+ a b))
) "error")
(test-equivalent '((let ([a 2]
      [b (* a 3)])
   (+ a b))
))
(test-expect/smol '((let ([foo 123]
      [get-foo (lambda () foo)])
  (get-foo))
) "error")
(test-equivalent '((let ([foo 123]
      [get-foo (lambda () foo)])
  (get-foo))
))
(test-expect/smol '((defvar a 30)
(defvar b (let ([a 2]) a))
(* a b)
) "60")
(test-equivalent '((defvar a 30)
(defvar b (let ([a 2]) a))
(* a b)
))
(test-expect/smol '((defvar a 1)
(defvar b 2)

(let ([a (* b 10)]
      [b (* a 10)])
  (mvec a b))
) "'#(20 10)")
(test-equivalent '((defvar a 1)
(defvar b 2)

(let ([a (* b 10)]
      [b (* a 10)])
  (mvec a b))
))
(test-expect/smol '((defvar a 1)
(defvar b 2)

(letrec ([a (* b 10)]
         [b (* a 10)])
  (mvec a b))
) "error")
(test-equivalent '((defvar a 1)
(defvar b 2)

(letrec ([a (* b 10)]
         [b (* a 10)])
  (mvec a b))
))
(test-expect/smol '((defvar a 1)
(defvar b 2)

(let* ([a (* b 10)]
       [b (* a 10)])
  (mvec a b))
) "'#(20 200)")
(test-equivalent '((defvar a 1)
(defvar b 2)

(let* ([a (* b 10)]
       [b (* a 10)])
  (mvec a b))
))
(test-expect/smol '((let (x 2)
  (+ x 1))
) "error")
(test-equivalent '((let (x 2)
  (+ x 1))
))
(test-expect/smol '((let ([a 100])
  (let ([b 1])
    (- a b)))
) "99")
(test-equivalent '((let ([a 100])
  (let ([b 1])
    (- a b)))
))