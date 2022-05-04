#lang racket
(provide catch)

(define (catch thunk handler)
  (with-handlers ([(λ (x) #t)
                   handler])
    (thunk)))
