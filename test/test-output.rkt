#lang racket/base

(require (for-syntax racket/base))
(require rackunit
         racket/string
         "data/test.html.pm"
         "data/template.html.rkt")

(define output (apply-template doc metas 'test.html))

(check-true (bytes? output))

(define lines (string-split (bytes->string/utf-8 output) "\n"))

(define-syntax (check-line stx)
  (syntax-case stx ()
    [(_ n expect msg) (syntax/loc stx (check-equal? (list-ref lines n) expect msg))]))

(check-line 1 "<test-root><strong>Good morning.</strong></test-root>" "doc")
(check-line 2 "True!" "when/splice[#t]")
(check-line 3 "Nothing:" "when/splice[#f]")
(check-line 4 "test.html" "value of here")
(check-line 5 "(pagetree-root first.html example.html third.html)" "(current-pagetree)")
