#lang racket/base

(module setup racket/base
  (provide (all-defined-out))
  (define external-renderer '(beeswax/for-pollen external-renderer)))

(provide root)

(define (root . elems)
  `(test-root ,@elems))