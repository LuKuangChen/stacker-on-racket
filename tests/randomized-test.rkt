#lang racket
(require "./utilities.rkt")
(require redex)

;; KC: honestly I find the randomized test not very useful because distribution of programs is not
;; very good.

(define-language smol/hof
  (program
   ::= (def ... expr ...))
  (def
    ::= (defvar id expr)
    (deffun (id_fun id_arg ...) def ... expr ... expr))
  (expr
   ::= id
   constant
   (if expr expr expr)
   (lambda (id ...) def ... expr ... expr)
   (λ (id ...) def ... expr ... expr)
   (let ((id expr) ...) def ... expr ... expr)
   (letrec ((id expr) ...) def ... expr ... expr)
   (begin expr ... expr)
   (o1 expr)
   (o2 expr expr)
   (o3 expr expr expr)
   (o* expr ...)
   (expr expr ...))
  (constant
   ::=
   string
   number
   ;;; have to disable this because redex doesn't understand #()
   ;; #(datum ...)
   (quote (datum ...)))
  (datum
   ::=
   string
   number
   ;;; have to disable this because redex doesn't understand #()
   ;;;  #(datum ...)
   (datum ...))
  (o1
   ::=
   left
   right
   vlen)
  (o2
   ::=
   equal?
   +
   -
   *
   mpair
   set-left!
   set-right!
   vec-ref
   cons
   map
   filter)
  (o3
   ::=
   vec-set!
   ;;;  foldl
   ;;;  foldr
   )
  (o*
   ::=
   mvec
   list)
  (id
   ::=
   ;; they creates too many spam tests
   ;;;  empty
   ;;;  o1
   ;;;  o2
   ;;;  o3
   ;;;  o*
   variable-not-otherwise-mentioned))

(define gen (generate-term smol/hof program))
(define (run-test n)
  (for ([i (in-range n)])
    (let ([program (generate-term smol/hof program #:i-th i)])
      (test-equivalent program))))
(run-test 1000)

;;; (define-metafunction smol/hof
;;;   smol-agree-with-smol-step : program -> boolean
;;;   [(smol-agree-with-smol-step program)
;;;    ,(let* ([oe-standard (eval-in-smol (term program))]
;;;            [oe-step (eval-in-smol-step (term program))]
;;;            [r (equal? oe-standard oe-step)])
;;;       (begin
;;;         (when (not r)
;;;           (displayln "! Program")
;;;           (writeln (term program))
;;;           (displayln "!! Output differs (smol vs smol-step)")
;;;           (writeln oe-standard)
;;;           (writeln oe-step))
;;;         r))])

;;; (redex-check smol/hof
;;;              program
;;;              (term (smol-agree-with-smol-step program))
;;;              #:attempts 100)