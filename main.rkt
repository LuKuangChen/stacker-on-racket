#lang racket/base

(module reader racket
  (require syntax/strip-context)

  (provide (rename-out [my-read read]
                       [my-read-syntax read-syntax]))

  (require "parse.rkt")
  (require "show.rkt")
  (require "interp-small-step.rkt")

  (define (run e)
    (define results (show (eval (parse e))))
    (for ([x results])
      (writeln x)))

  (define (my-read in)
    (syntax->datum
     (my-read-syntax #f in)))

  (define (my-read-syntax src in)
    (with-syntax ([program (let loop ([xs empty])
                             (let ([x (read-syntax 'whatever in)])
                               (if (eof-object? x)
                                   (reverse xs)
                                   (loop (cons x xs)))))])
      (strip-context
       #`(module anything racket
           ('#,run 'program))))))

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here



(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

#;
(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
   #:program "my-program"
   #:once-each
   [("-n" "--name") name "Who to say hello to" (set-box! who name)]
   #:args ()
   (printf "hello ~a~n" (unbox who))))
