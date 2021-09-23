#lang racket/base

(require (for-syntax racket/base))
(require pollen/setup
         rackunit
         racket/list
         racket/string
         "data/test.html.pm"
         "data/template.html.rkt")


(define output
  (parameterize ([current-project-root (build-path (current-directory) "data")])
    (apply-template doc metas 'example.html)))

(check-true (bytes? output))

(define lines (string-split (bytes->string/utf-8 output) "\n"))

(define-syntax (check-line stx)
  (syntax-case stx ()
    [(_ n expect msg) (syntax/loc stx (check-equal? (list-ref lines n) expect msg))]))

(check-line 1 "<test-root><strong>Good morning.</strong></test-root>" "doc")
(check-line 2 "True!" "when/splice[#t]")
(check-line 3 "Nothing:" "when/splice[#f]")
(check-line 4 "example.html" "value of here")
(check-line 5 "'(pagetree-root first.html example.html third.html)" "(current-pagetree)")

;; Line 6
(test-case
 "Check (current-metas)"
 (define should-be-metas (cadr (read (open-input-string (list-ref lines 6)))))
 (check-equal? (hash-ref should-be-metas 'title) "The Muse in the Machine"))

;; Have to filter out the last part of the absolute path since it will be
;; different on every computer
(test-case
 "Check (current-project-root) required from pollen/setup"
 (define p (take-right (explode-path (string->path (list-ref lines 7))) 3))
 (check-equal? (map path->string p) '("beeswax" "test" "data")))

(check-line 8 "The Muse in the Machine" "title in metas")
(check-line 9 "first.html" "(previous here)")
(check-line 10 "third.html" "(next here)")
