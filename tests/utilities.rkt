#lang racket
(provide (all-defined-out))
(require racket/sandbox)

(sandbox-path-permissions
 (list*
  ;;; (list 'exists "/lib64")
  ;;; (list 'exists "/usr/lib64")
  ;;; (list 'exists (current-directory))
  ;;; (list 'exists "/System")
  ;;; (list 'exists "/Users")
  (list 'exists (current-directory))
  (sandbox-path-permissions)
  ))
(define (eval-in-step program)
  (parameterize ([sandbox-output 'string]
                 [sandbox-eval-limits (list 10 #f)]
                 [sandbox-propagate-exceptions #f])
    (define ev (make-module-evaluator
                `(module m cs19-step/semantics
                   #:no-trace
                   ,@program)))
    (normalize (get-output ev))))

(define (eval-in-htdp2 program)
  (define port (open-output-string))
  (parameterize ([sandbox-output port]
                 [sandbox-eval-limits (list 10 #f)]
                 [sandbox-propagate-exceptions #f])
    ;; sandbox-propagate-exceptions fails to catch some errors (e.g. `(defvar equal? equal?)`)
    (with-handlers ([any/c (lambda (e) "error")])
      (define ev (make-module-evaluator
                  `(module m lang/htdp-intermediate-lambda
                     ,@program)))
      (normalize (get-output-string port)))))

(define (test-equivalent program)
  (with-handlers ([any/c (lambda (e)
                           (displayln ";; Program")
                           (writeln program)
                           (displayln ";; exception")
                           (displayln e)
                           (newline))])
    (let* ([oe-standard (eval-in-htdp2 program)]
           [oe-step (eval-in-step program)]
           [r (equal? oe-standard oe-step)])
      (when (not r)
        (displayln ";; Program")
        (writeln program)
        (displayln "-----------------------------")
        (displayln ";; htdp-beginner")
        (writeln oe-standard)
        (displayln "-----------------------------")
        (displayln ";; htdp-beginner-step")
        (writeln oe-step)
        (newline)))))

(define (test-expect/htdp-beginner program expect)
  (with-handlers ([any/c (lambda (e)
                           (displayln ";; exception")
                           (displayln e)
                           (newline))])
    (let* ([result (eval-in-htdp2 program)]
           [expect (normalize-expect expect)]
           [r (equal? result expect)])
      (when (not r)
        (begin
          (displayln ";; Program")
          (writeln program)
          (displayln "-----------------------------")
          (displayln ";; htdp-beginner actual")
          (writeln result)
          (displayln ";; expected")
          (writeln expect)
          (newline))))))

(define (test-expect/htdp-beginner-step program expect)
  (with-handlers ([any/c (lambda (e)
                           (displayln ";; exception")
                           (displayln e)
                           (newline))])
    (let* ([result (eval-in-step program)]
           [r (equal? result (normalize-expect expect))])
      (when (not r)
        (begin
          (displayln ";; Program")
          (writeln program)
          (displayln "-----------------------------")
          (displayln ";; htdp-beginner-step actual")
          (writeln result)
          (displayln ";; expected")
          (writeln expect)
          (newline))))))

(define (normalize output)
  ((compose
    (lambda (output)
      (regexp-replace* #rx" $" output ""))
    (lambda (output)
      (regexp-replace* #rx"\n" output " "))
    (lambda (output)
      (regexp-replace* #rx"#<procedure:[^\n]*>" output "#<procedure>"))
    (lambda (output)
      (regexp-replace* #rx"error:[^\n]*" output "error"))
    )
   output))

(define (normalize-expect expect)
  (string-append expect ""))