#lang info
(define collection "beeswax")
(define deps '("at-exp-lib"
               "pollen"
               "sugar"
               "base"))
(define build-deps '("scribble-doc"
                     "scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/beeswax.scrbl" (multi-page))))
(define raco-commands '(("beeswax" (submod beeswax/private/command raco) "issue Beeswax command" #f)))
(define pkg-desc "A Pollen-friendly template language")
(define version "0.1")
(define pkg-authors '(joel))
