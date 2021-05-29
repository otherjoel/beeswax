#lang info
(define collection "beeswax")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/beeswax.scrbl" (multi-page))))
(define raco-commands '(("beeswax" (submod beeswax/private/command raco) "issue Beeswax command" #f)))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(joel))
