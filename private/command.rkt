#lang racket/base

(module+ raco
  (define subcommand (with-handlers
                       ([exn:fail? (λ (exn) #f)])
                       (vector-ref (current-command-line-arguments) 0)))
  (dispatch subcommand))

(define (dispatch subcommand-name)
  (case subcommand-name
    [(#f "help") (raco-beeswax-help)]
    [("render") (raco-beeswax-render)]
    [else (raco-beeswax-help)]))

(define (raco-beeswax-render)
  #f)

(define (raco-beeswax-help)
  (displayln "Beeswax commands:
help               show this message
render path ...    render one or more paths (source or output name, but no folders)"))
