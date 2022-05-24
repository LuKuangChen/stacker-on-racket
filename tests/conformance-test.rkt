#lang racket
(require racket/sandbox)
(require redex)

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
   #(datum ...)
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
   vref
   cons
   map
   filter)
  (o3
   ::=
   vset!
  ;;;  foldl
  ;;;  foldr
   )
  (o*
   ::=
   mvec
   list)
  (id
   ::=
   empty
   o1
   o2
   o3
   o*
   variable-not-otherwise-mentioned))

(define (eval-in-smol-step program)
  (parameterize ([sandbox-output 'string]
                 [sandbox-eval-limits (list 5 #f)]
                 [sandbox-propagate-exceptions #f])
    (define ev (make-module-evaluator
                `(module m smol-step/hof/semantics
                   #:no-trace
                   ,@program)))
    (normalize (get-output ev))))

(define (eval-in-smol program)
  (define port (open-output-string))
  (parameterize ([sandbox-output port]
                 [sandbox-eval-limits (list 5 #f)]
                 [sandbox-propagate-exceptions #f])
    ;; sandbox-propagate-exceptions fails to catch some errors (e.g. `(defvar equal? equal?)`)
    (with-handlers ([any/c (lambda (e) "error\n")])
      (define ev (make-module-evaluator
                  `(module m smol/hof/semantics
                     ,@program)))
      (normalize (get-output-string port)))))

(define (normalize output)
  ((compose
     (lambda (output)
       (regexp-replace* #rx"#<procedure:[^\n]*>" output "#<procedure>"))
     (lambda (output)
       (regexp-replace* #rx"error:[^\n]*" output "error"))
    )
   output))

(define-metafunction smol/hof
  smol-agree-with-smol-step : program -> boolean
  [(smol-agree-with-smol-step program)
   ,(let* ([oe-standard (eval-in-smol (term program))]
           [oe-step (eval-in-smol-step (term program))]
           [r (equal? oe-standard oe-step)])
      (begin
        (when (not r)
          (displayln "! Program")
          (writeln (term program))
          (displayln "!! Output differs (smol vs smol-step)")
          (writeln oe-standard)
          (writeln oe-step))
        r))])

(redex-check smol/hof
             program
             (term (smol-agree-with-smol-step program))
             #:attempts 100)