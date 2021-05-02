#lang racket/base

(require (prefix-in pollen: pollen/render)
         pollen/setup
         racket/match
         racket/path
         "render.rkt")

; This is a renderer specifically for use by Pollen
(provide external-renderer)

(define (external-renderer source-path orig-template-path output-path)
  (case (path-get-extension source-path)
    [(#".pm" #".pmd")
     (match-define-values
      ((cons render-result _) _ ms _)
      (time-apply render (list source-path output-path)))
     (log-info "beeswax rendered /~a (~ams)"
               (find-relative-path (current-project-root) output-path) ms)
     render-result]
    [else (pollen:render source-path orig-template-path)]))
