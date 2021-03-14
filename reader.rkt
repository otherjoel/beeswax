#lang racket/base

(require scribble/reader pollen/setup)

(provide (rename-out [*read-syntax read-syntax]))

(define read-pollen-syntax
  (make-at-reader #:command-char (setup:command-char)
                  #:syntax? #t
                  #:inside? #t))

(define (*read-syntax name inport)
  (define exprs (read-pollen-syntax name inport))
  (datum->syntax
   #f
   `(module template-render beeswax/expander
        ,@exprs)))