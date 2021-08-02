#lang racket/base

(require racket/file
         racket/logging
         racket/runtime-path
         racket/string
         raco/all-tools
         scribble/core
         scribble/example
         scribble/html-properties
         scribble/latex-properties
         scribble/manual)

(provide sandbox
         sample-file
         sandbox-raco
         ensure-pollen-rkt
         terminal
         :>
         rem)

(define-runtime-path sample-proj-dir "sample-proj/")
(define (sample-file filename)
  (build-path sample-proj-dir filename))

(define sandbox
  (parameterize ([current-directory sample-proj-dir])
    (make-base-eval #:lang 'racket/base)))

(define (ensure-pollen-rkt state)
  (case state
    [(absent)
     (delete-directory/files (sample-file "pollen.rkt") #:must-exist? #f)]
    [(present)
     (with-handlers ([exn:fail:filesystem? (λ () (void))])
       (copy-file (sample-file "pollen.rkt.orig") (sample-file "pollen.rkt")))]))

;; Call a raco command with args as if it had been run from the sample-proj directory.
;; For `raco pollen` or `raco beeswax` commands, returns a string containing the command
;; output; for all other commands, returns ""
(define (sandbox-raco command args)
  (define cmd-mod (cadr (hash-ref (all-tools) command)))
  (define raco-events '())
  (with-intercepted-logging
   (λ (evt) (set! raco-events (cons (vector-ref evt 1) raco-events)))
   (λ () (parameterize ([current-command-line-arguments args]
                        [current-error-port (open-output-string)] ; keep output off the console
                        [current-namespace (make-base-namespace)]
                        [current-directory sample-proj-dir])
           (dynamic-require cmd-mod #f)))
   'info 'pollen 'info 'beeswax)
  (string-join (reverse raco-events) "\n"))

(define-runtime-path aux-css "additional.css")
(define-runtime-path aux-tex "additional.tex")

(define (terminal . args)
  (compound-paragraph (style "terminal" (list (color-property (list #x66 #x33 #x99))
                                              (css-style-addition aux-css)
                                              (alt-tag "div")
                                              (tex-addition aux-tex)))
                      (list (apply verbatim args))))

;; Simulate a command-line prompt
(define (:> . args)
  (element (style "prompt" (list (color-property (list #x66 #x66 #x66))))
           (apply exec (cons "> " args))))

;; Simulate a bash-style comment
(define (rem . args)
  (apply racketcommentfont (cons "# " args)))

