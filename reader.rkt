#lang racket/base

(require pollen/setup
         racket/class
         scribble/reader)

(provide get-info (rename-out [*read-syntax read-syntax]))

(define read-pollen-syntax
  (make-at-reader #:command-char (setup:command-char)
                  #:syntax? #t
                  #:inside? #t))

(define (*read-syntax name inport)
  (define exprs (read-pollen-syntax name inport))
  (datum->syntax
   #f
   `(module template-render beeswax/expander
      ,@exprs)))

;; My get-info is largely lifted from pollen/private/reader-base.rkt
(define (get-info port src-mod src-line src-col src-pos)
  ;; DrRacket caches source file information per session,
  ;; so we can do the same to avoid multiple searches for the command char.
  (define command-char-cache (make-hash))
  (λ (key default)
    (case key
      ;; only do source-path searching if we have one of these two keys
      [(color-lexer drracket:toolbar-buttons)
       (define maybe-source-path
         (with-handlers ([exn:fail? (λ (exn) #false)])
           ;; Robert Findler does not endorse `get-filename` here,
           ;; because it's sneaky and may not always work.
           ;; OTOH Scribble relies on it, so IMO it's highly unlikely to change.
           (send (object-name port) get-filename)))
       (define my-command-char
         (hash-ref! command-char-cache maybe-source-path (λ () (setup:command-char maybe-source-path))))
       (case key
         [(color-lexer)
          (define maybe-lexer
            (dynamic-require 'syntax-color/scribble-lexer 'make-scribble-inside-lexer (λ () #false)))
          (cond
            [(procedure? maybe-lexer) (maybe-lexer #:command-char my-command-char)]
            [else default])]
         [(drracket:toolbar-buttons) default])] ;; TODO
      [(drracket:indentation)
       (λ (text pos)
         (define line-idx (send text position-line pos))
         (define line-start-pos (send text line-start-position line-idx))
         (define line-end-pos (send text line-end-position line-idx))
         (define first-vis-pos
           (or
            (for/first ([pos (in-range line-start-pos line-end-pos)]
                        #:unless (char-blank? (send text get-character pos)))
              pos)
            line-start-pos))
         (- first-vis-pos line-start-pos))]
      [else default])))
