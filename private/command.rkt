#lang racket/base

(require pollen/setup
         racket/cmdline
         racket/logging
         racket/match
         racket/vector
         "../render.rkt"
         "files.rkt")

(module+ raco
  (define subcommand (with-handlers
                       ([exn:fail? (λ (exn) #f)])
                       (vector-ref (current-command-line-arguments) 0)))
  (dispatch subcommand))

(define-logger beeswax)

(define (dispatch subcommand-name)
  (with-logging-to-port
    (current-error-port)
    (λ ()
      (case subcommand-name
        [("render") (raco-beeswax-render)]
        [else (raco-beeswax-help)]))
    #:logger beeswax-logger 'info 'beeswax))

; raco beeswax render -t ext source-file ...
;  -t ext  sets output file extension
;  A pagetree in the source will be converted to a list of source files using Pollen’s get-source

(define (raco-beeswax-render)
  (define target-ext (make-parameter (symbol->string (current-poly-target))))
  (define args
    (command-line #:program "raco beeswax render"
                  #:argv (vector-drop (current-command-line-arguments) 1)
                  #:once-any
                  [("-t" "--target") target-arg "Fallback render target for poly sources"
                                      (target-ext target-arg)]
                  #:args rest-args
                  rest-args))
  (for ([source (in-list args)])
    (define outpath (->output source (target-ext)))
        (match-define-values
          ((cons render-result _) _ ms _)
          (time-apply render (list source outpath)))
        (log-beeswax-info "rendered /~a → /~a (~ams)" source outpath ms)))

(define (raco-beeswax-help)
  (displayln "Beeswax commands:
help                     show this message
render source-file ...   render one or more sources (no folders)"))
