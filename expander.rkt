#lang racket/base

(require (for-syntax racket/base)
         racket/contract/base
         racket/list
         (prefix-in doclang: "private/doclang-raw.rkt"))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [beeswax-module-begin #%module-begin]))

(define (strip-leading-newlines lst)
  (dropf lst (λ (ln) (member ln (list "\n" "")))))

;; Split requires and provides etc. out of a list of expressions
(define-for-syntax (toplevel-forms-splitter lst)
  (let loop ([body lst]
             [toplevelstuff '()]
             [normalstuff '()])
    (syntax-case body ()
      [() (list (reverse toplevelstuff) (reverse normalstuff))]
      [((id rest ...) . body2)
       (and (identifier? #'id)
            (ormap (lambda (kw) (free-identifier=? #'id kw))
                   (syntax->list #'(require
                                     provide
                                     define
                                     define-values
                                     define-syntaxes
                                     begin-for-syntax
                                     module
                                     module*
                                     #%require
                                     #%provide))))
       (loop #'body2 (cons #'(id rest ...) toplevelstuff) normalstuff)]
      [(body1 . body2)
       (loop #'body2 toplevelstuff (cons #'body1 normalstuff))])))

(define-syntax (beeswax-module-begin stx)
  (syntax-case stx ()
    [(_ . EXPRS)
     (with-syntax ([((TOPLEVEL ...) (BODY ...)) (toplevel-forms-splitter #'EXPRS)]
                   [DOC (datum->syntax #'EXPRS 'doc)]
                   [METAS (datum->syntax #'EXPRS 'metas)])
       #'(doclang:#%module-begin
          render   ; name of exported identifier
          car      ; Exported identifier will bind to only the first value (the lambda)
          (provide (contract-out (render (-> any/c hash? bytes?))))
          TOPLEVEL ...
          (lambda (DOC METAS) (beeswax-concat-bytes (list . (BODY ...))))))]))

(define (->string->bytes x)
  (cond
    [(bytes? x) x]
    [(string? x) (string->bytes/utf-8 x)]
    [(or (null? x) (void? x)) #""]
    [(or (symbol? x) (number? x) (path? x) (char? x)) (->string->bytes (format "~a" x))]
    [else (->string->bytes (format "~v" x))]))

(define (beeswax-concat-bytes xs)
  (apply bytes-append (map ->string->bytes (strip-leading-newlines xs))))