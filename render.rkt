#lang racket/base

(require pollen/cache
         pollen/setup
         racket/string
         sugar/file
         "private/constants.rkt"
         "private/files.rkt")

(provide get-template-proc
         render)

(define (err  msg . vals)
  (apply raise-user-error 'beeswax-render msg vals))

(define (beeswax-exn e orig-source)
  (raise
   (exn:fail:contract (string-replace (exn-message e) orig-source "beeswax-renderer")
                      (exn-continuation-marks e))))

(define (get-template-proc template-path)
  (define template-proc
    (with-handlers ([exn:fail:filesystem:missing-module?
                     (λ (e) (beeswax-exn e "standard-module-name-resolver"))]
                    [exn:fail:contract?
                     (λ (e) (beeswax-exn e "dynamic-require"))])
      (dynamic-require (make-resolved-module-path template-path) template-proc-provide)))
  (unless (procedure? template-proc)
    (err "The binding ‘~a’ provided by ~a is not a procedure: ~v"
         template-proc-provide
         template-path
         template-proc))
  template-proc)

;; This function is specifically for rendering Pollen sources.
(define (render source-path output-path)
  (define target-ext (get-ext output-path))
  (parameterize ([current-poly-target (string->symbol target-ext)])
    (define doc (cached-doc source-path))
    (define metas (cached-metas source-path))

    (define template-path
      (or (template-from-metas source-path metas target-ext)
          (find-default-template source-path target-ext)
          (err "No template available for ~a targeting ~a" source-path target-ext)))
    (define render-proc (get-template-proc template-path))
    (render-proc doc metas (string->symbol (format "~a" output-path)))))
