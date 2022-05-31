#lang plait

(require (rename-in 
           (typed-in "./error-racket.rkt"
                     [catch : ((-> 'a) ('error -> 'a) -> 'a)])
            [catch raw-catch]))
(require (rename-in 
           (typed-in racket
                     [raise : ('a -> 'b)])
            [raise raw-raise]))

(define-type Exception
  (exn-tc [msg : String])
  (exn-rt [msg : String])
  (exn-internal [where : Symbol] [what : String]))

(define (raise [e : Exception]) : 'a
  (raw-raise e))
(define (catch [main : (-> 'a)] [handle : (Exception -> 'a)]) : 'a
  (raw-catch main handle))