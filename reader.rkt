#lang racket/base

(require scribble/reader)

(provide (rename-out [*read-syntax read-syntax]))

(define read-lozenge-syntax
  (make-at-reader #:command-char #\◊
                  #:syntax? #t
                  #:inside? #t))

(define (*read-syntax name inport)
  (define exprs (read-lozenge-syntax name inport))
  (datum->syntax
   #f
   `(module beeswax-template racket/base
      (module template-render beeswax/expander
        ,@exprs)
      (require 'template-render)
      (provide (all-from-out 'template-render)))))