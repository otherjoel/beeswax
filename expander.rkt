#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/list)

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [beeswax-module-begin #%module-begin]))

(define (strip-leading-whitespace lst)
  (dropf lst (λ (ln) (or (member ln (list "" (void))) (regexp-match #px"^[\\s]+$" ln)))))

;; Split top-level stuff (requires, provides etc.) and defines out of a list of expressions
(define-for-syntax (forms-splitter lst)
  (let loop ([body lst]
             [toplevelstuff '()]
             [defines '()]
             [normalstuff '()])
    (syntax-case body ()
      [() (list (reverse toplevelstuff) (reverse defines) (reverse normalstuff))]
      [((id rest ...) . body2)
       (and (identifier? #'id)
            (ormap (lambda (kw) (free-identifier=? #'id kw))
                   (syntax->list #'(require
                                     provide
                                     define-syntaxes
                                     begin-for-syntax
                                     module
                                     module*
                                     #%require
                                     #%provide))))
       (loop #'body2 (cons #'(id rest ...) toplevelstuff) defines normalstuff)]
      [((id rest ...) . body2)
       (and (identifier? #'id)
            (ormap (lambda (kw) (free-identifier=? #'id kw))
                   (syntax->list #'(define define-values))))
       (loop #'body2 toplevelstuff (cons #'(id rest ...) defines) normalstuff)]
      [(body1 . body2)
       (loop #'body2 toplevelstuff defines (cons #'body1 normalstuff))])))

(define-syntax (beeswax-module-begin stx)
  (syntax-case stx ()
    [(_ . EXPRS)
     (with-syntax ([((TOPLEVEL ...) (DEFINES ...) (BODY ...)) (forms-splitter #'EXPRS)]
                   [REQUIRES (datum->syntax #'EXPRS '(require pollen/template pollen/pagetree))]
                   [RENDER (datum->syntax #'EXPRS 'render)]
                   [DOC (datum->syntax #'EXPRS 'doc)]
                   [METAS (datum->syntax #'EXPRS 'metas)]
                   [HERE (datum->syntax #'EXPRS 'here)])
       #'(#%module-begin
          (provide RENDER)
          REQUIRES
          TOPLEVEL ...
          (define/contract (RENDER DOC METAS HERE)
            (-> any/c hash? path-string? bytes?)
            DEFINES ...
            (concat+write/bytes HERE (list . (BODY ...))))))]))

(define (->string->bytes x)
  (cond
    [(bytes? x) x]
    [(string? x) (string->bytes/utf-8 x)]
    [(or (null? x) (void? x)) #""]
    [(or (symbol? x) (number? x) (path? x) (char? x)) (string->bytes/utf-8 (format "~a" x))]
    [else (string->bytes/utf-8 (format "~v" x))]))

(define (concat+write/bytes filename xs)
  (with-output-to-file filename
    (lambda ()
      (define bits (apply bytes-append (map ->string->bytes (strip-leading-whitespace xs))))
      (write-bytes bits)
      bits)
    #:exists 'replace))