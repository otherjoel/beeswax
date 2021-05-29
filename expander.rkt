#lang racket/base

(require (for-syntax pollen/setup
                     racket/base
                     racket/match
                     "private/constants.rkt")
         pollen/pagetree
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
                   (syntax->list #'(define define-values match-define))))
       (loop #'body2 toplevelstuff (cons #'(id rest ...) defines) normalstuff)]
      [(body1 . body2)
       (loop #'body2 toplevelstuff defines (cons #'body1 normalstuff))])))

(define-for-syntax (pollen-requires)
  (define pollen-rkt
    (match (find-nearest-default-directory-require (current-project-root))
      [(? path? p) `((file ,(path->string p)))]
      [_ '()]))
  `(require pollen/core pollen/template pollen/pagetree ,@pollen-rkt))

(define-syntax (beeswax-module-begin stx)
  (syntax-case stx ()
    [(_ . EXPRS)
     (with-syntax ([((TOPLEVEL ...) (DEFINES ...) (BODY ...)) (forms-splitter #'EXPRS)]
                   [REQUIRES (datum->syntax #'EXPRS (pollen-requires))]
                   [EXPORT-FUNC (datum->syntax #'EXPRS template-proc-provide)]
                   [DOC (datum->syntax #'EXPRS 'doc)]
                   [METAS (datum->syntax #'EXPRS 'metas)]
                   [HERE (datum->syntax #'EXPRS 'here)])
       #'(#%module-begin
          (provide EXPORT-FUNC)
          REQUIRES
          TOPLEVEL ...
          (define/contract (EXPORT-FUNC DOC METAS HERE)
            (-> any/c hash? pagenode? bytes?)
            DEFINES ...
            (apply bytes-append
                   (map ->bytes (strip-leading-whitespace (list . (BODY ...))))))))]))

(define (->bytes x)
  (cond
    [(bytes? x) x]
    [(string? x) (string->bytes/utf-8 x)]
    [(or (null? x) (void? x)) #""]
    [(or (symbol? x) (number? x) (path? x) (char? x)) (string->bytes/utf-8 (format "~a" x))]
    [else (string->bytes/utf-8 (format "~v" x))]))
