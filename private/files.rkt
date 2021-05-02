#lang racket/base

(require racket/path
         sugar/file)

;; Functions for mapping sources to templates and output files when using
;; Beeswax CLI tools to render sources

(provide ->output
         default-template
         find-upwards-from
         template-from-metas
         find-default-template)

(define (->output source fallback-ext)
  (let* ([chopped-last-ext (remove-ext source)]
         [pen-ext (get-ext chopped-last-ext)]
         [base (remove-ext chopped-last-ext)]
         [target-ext (or (and (equal? pen-ext "poly") fallback-ext)
                         pen-ext
                         fallback-ext)])
    (format "~a.~a" base target-ext)))

(module+ test
  (require rackunit)
  (check-equal? (->output "file.txt.pm" "html")  "file.txt")  ; Penultimate ext determines target
  (check-equal? (->output "file.txt.zz" "html")  "file.txt")  ; Final ext doesn’t matter
  (check-equal? (->output "file.poly.pm" "html") "file.html") ; Poly extension uses fallback
  (check-equal? (->output "file.rkt" "html")     "file.html") ; Only 1 ext treated like poly
  (check-equal? (->output "file.zz" "css")       "file.css")  ; Same rule
  (check-equal? (->output "file.txt" "html")     "file.html")); Hmmmm

;; In the Beeswax scheme, for a template file to be automatically “findable” (i.e., not specified in
;; a source file’s exported `metas` hash table) it must have (at least) two extensions:
;;  → The penultimate extension is the output file format.
;;  → The final extension must be .rkt
(define (default-template target-ext)
  (format "template.~a.rkt" target-ext))

(define (containing-dir path)
  (define-values (dir name dir?) (split-path path))
  dir)

(define (find-upwards-from starting-path file-to-find)
  (let loop ([dir (containing-dir (simple-form-path starting-path))])
    (and dir
         (let ([file (build-path dir file-to-find)])
           (cond
             [(file-exists? file) file]
             [else (loop (containing-dir dir))])))))

; Return the target output format of a source filename, or #f if the source filename does not imply
; a specific output target.
(define (output-ext source)
  (define maybe-out (remove-ext source))
  (and (not (has-ext? maybe-out "poly"))
       (get-ext maybe-out)))

(module+ test
  (check-equal? (output-ext "file.txt.pm")  "txt") ; source implies .txt
  (check-equal? (output-ext "file.txt.zz")  "txt") ; source still implies .txt
  (check-equal? (output-ext "file.txt.rkt") "txt") ; source still implies .txt
  (check-equal? (output-ext "file.poly.pm") #f)    ; poly files could target anything
  (check-equal? (output-ext "file.rkt")     #f)    ; doesn’t imply a target output extension
  (check-equal? (output-ext "file.txt")     #f))   ; probably not a source file

(define (template-from-metas source-path metas target-ext)
  (define template-specified (hash-ref metas 'template #f))
  (define template-name
    (cond [(list? template-specified)
           (findf (λ (t) (eq? (output-ext t) target-ext)) template-specified)]
          [else template-specified]))
  (and template-name (simplify-path (cleanse-path (build-path (containing-dir source-path) template-name)))))

(define (find-default-template source-path target-ext)
  (find-upwards-from source-path (default-template target-ext)))
