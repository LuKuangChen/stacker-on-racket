#lang plait
(require (rename-in (typed-in racket [format : (String 'a -> String)])
           [format raw-format]))

(define (format template datum)
  (raw-format template datum))

(define (ind-List (x* : (Listof 'a)) (base : 'b) (step : ('b 'a -> 'b)))
  (foldr (λ (x IH) (step IH x)) base x*))


(define (hash-set* base ⟨k×v⟩*)
  (ind-List ⟨k×v⟩*
            base
            (λ (IH ⟨k×v⟩)
              (hash-set IH (fst ⟨k×v⟩) (snd ⟨k×v⟩)))))

(define (hash-ref* h k*)
  (hash-set* (hash (list))
             (map (lambda (k)
                    (values k (some-v (hash-ref h k))))
                  k*)))
