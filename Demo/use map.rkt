#lang smol-step

(defvar empty #f)

(deffun (map f xs)
  (if (equal? xs empty)
      empty
      (pair (f (left xs))
            (map f (right xs)))))

(deffun (_*2+1 x)
  (if (equal? x 3)
      (pause)
      (+ (* x 2) 1)))

(deffun (count-down n)
  (if (equal? n 0)
      empty
      (pair n (count-down (- n 1)))))

(defvar 5to1 (count-down 5))

(map _*2+1 5to1)