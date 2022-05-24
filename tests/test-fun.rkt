#lang racket
(require racket/sandbox)
(require redex)

(define-language smol/fun
  (program
   ::= (def ... expr ...))
  (def
    ::= (defvar id expr)
    (deffun (id_fun id_arg ...) def ... expr ... expr))
  (expr
   ::= id
   constant
   (if expr expr expr)
   (begin expr ... expr)
   (o2 expr expr)
   (expr expr ...))
  (constant
   ::= string
   number
   #(constant ...))
  (o2
   ::= equal?
   +
   -
   *)
  (id
   ::=
   o2
   variable-not-otherwise-mentioned))

(define (eval-in-smol-step program)
  (parameterize ([sandbox-output 'string]
                 [sandbox-eval-limits (list 1000 1000)]
                 [sandbox-propagate-exceptions #f])
    (define ev (make-module-evaluator
                `(module m smol-step/fun/semantics
                   #:no-trace
                   ,@program)))
    (normalize (get-output ev))))

(define (eval-in-smol program)
  (define port (open-output-string))
  (parameterize ([sandbox-output port]
                 [sandbox-eval-limits (list 10 1000)]
                 [sandbox-propagate-exceptions #f])
    ;; sandbox-propagate-exceptions fails to catch some errors (e.g. `(defvar equal? equal?)`)
    (with-handlers ([any/c (lambda (e) "error\n")])
      (define ev (make-module-evaluator
                  `(module m smol/fun/semantics
                     ,@program)))
      (normalize (get-output-string port)))))

(define (normalize output)
  ((compose
     (lambda (output)
       (regexp-replace* #rx"#<procedure:[^\n.]*>" output "#<procedure>"))
     (lambda (output)
       (regexp-replace* #rx"error:[^\n.]*" output "error"))
    )
   output))

(define-metafunction smol/fun
  smol-agree-with-smol-step : program -> boolean
  [(smol-agree-with-smol-step program)
   ,(let* ([oe-standard (eval-in-smol (term program))]
           [oe-step (eval-in-smol-step (term program))]
           [r (equal? oe-standard oe-step)])
      (begin
        (when (not r)
          (displayln "! Program")
          (writeln (term program))
          (displayln "!! Output differs")
          (writeln oe-standard)
          (writeln oe-step))
        r))])

(redex-check smol/fun
             program
             (term (smol-agree-with-smol-step program))
             #:attempts 20)