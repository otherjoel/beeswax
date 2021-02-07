#lang racket/base

(require (for-syntax racket/base)
         racket/list
         (prefix-in doclang: pollen/private/external/doclang-raw))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [beeswax-module-begin #%module-begin]))

(define (strip-leading-newlines lst)
  (dropf lst (λ (ln) (member ln (list "\n" "")))))

(define-syntax (beeswax-module-begin stx)
  (syntax-case stx ()
    [(_ . EXPRS)
     (with-syntax ([ALL-DEFINED-OUT (datum->syntax #'EXPRS '(all-defined-out))]
                   [DOC (datum->syntax #'EXPRS 'doc)]
                   [METAS (datum->syntax #'EXPRS 'metas)])
       #'(doclang:#%module-begin
          render   ; name of exported function
          car      ; use only the first value out of the expressions (the lambda)
          (provide ALL-DEFINED-OUT render)
          (lambda (DOC METAS) (beeswax-concat-bytes (list . EXPRS)))))]))

(define (->string->bytes x)
  (cond
    [(bytes? x) x]
    [(string? x) (string->bytes/utf-8 x)]
    [(or (null? x) (void? x)) #""]
    [(or (symbol? x) (number? x) (path? x) (char? x)) (->string->bytes (format "~a" x))]
    [else (->string->bytes (format "~v" x))]))

(define (beeswax-concat-bytes xs)
  (apply bytes-append (map ->string->bytes (strip-leading-newlines xs))))