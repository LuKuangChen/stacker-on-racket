#lang info
(define collection "stacker")
(define deps '("base" "plait" "pict-lib" "pprint" "gui-lib" "draw-lib" "redex-gui-lib" "sandbox-lib" "testing-util-lib" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/stacker.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(lukc))
(define license '(Apache-2.0 OR MIT))
