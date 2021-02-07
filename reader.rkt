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
   `(module wax-wrapper racket/base
      (module wax-renderer beeswax/expander
        ,@exprs)
      (require 'wax-renderer)
      (provide (all-from-out 'wax-renderer)))))