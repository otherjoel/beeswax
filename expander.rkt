#lang racket/base

(require (for-syntax racket/base)
         racket/list
         (prefix-in doclang: pollen/private/external/doclang-raw))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [beeswax-module-begin #%module-begin]))

(define (strip-leading-newlines doc)
  (dropf doc (λ (ln) (member ln (list "\n" "")))))

(define-syntax (beeswax-module-begin stx)
  (syntax-case stx ()
    [(_ . EXPRS)
     (with-syntax ([ALL-DEFINED-OUT (datum->syntax #'EXPRS '(all-defined-out))])
       #'(doclang:#%module-begin
          render
          (λ (xs)
            (lambda (doc metas filename)
              (apply bytes-append (map ->string->bytes (strip-leading-newlines xs)))))
          (provide ALL-DEFINED-OUT
                   render)
          (begin . EXPRS)))]))

(define (->string->bytes x)
  (cond
    [(bytes? x) x]
    [(string? x) (string->bytes/utf-8 x)]
    [(or (null? x) (void? x)) #""]
    [(or (symbol? x) (number? x) (path? x) (char? x)) (->string->bytes (format "~a" x))]
    [else (->string->bytes (format "~v" x))]))

(define (beeswax-concat-bytes xs)
  (apply bytes-append (map ->string->bytes xs)))