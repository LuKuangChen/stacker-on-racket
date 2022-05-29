#lang plait
#:untyped
(require (rename-in (typed-in racket [format : (String 'a -> String)])
                    [format raw-format]))

(define-syntax let-values
  (syntax-rules ()
    [(let-values (((x ...) e)) body)
     (let ([tmp e])
       (local ((define-values (x ...) tmp))
         body))]))

(define (displayln x)
  (begin
    (display x)
    (display "\n")))

(define (string-of any)
  (format "~a" any))

(define (string-join t s*)
  (ind-List s*
            ""
            (lambda (_IH s)
              (let ([string*-of-s* (map (lambda (s) (string-append t s)) s*)])
                (string-append s
                               (ind-List string*-of-s*
                                         ""
                                         (lambda (IH elm)
                                           (string-append elm IH))))))))

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

(define-type (Dec 'x 'y)
  (yes [it : 'x])
  (no [it : 'y]))
(define (get-last xs)
  (type-case (Listof 'x) (reverse xs)
    (empty
     (no (values)))
    ((cons x xs)
     (yes (values (reverse xs) x)))))