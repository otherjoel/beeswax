#lang racket/base

(module reader racket/base
  (provide read-syntax get-info)
  (require "reader.rkt"))

;; For Scribble purposes. Must match the value in constants.rkt
(provide apply-template)
(define apply-template 'dummy)
