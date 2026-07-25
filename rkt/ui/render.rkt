#lang racket

;;; Phase 7 — Terminal Grid ASCII/ANSI Visualizer

(require "../world/state.rkt"
         "../world/artifacts.rkt")

(provide render-world-ascii
         display-world-ascii)

;; Render world state as a formatted ASCII grid string
(define (render-world-ascii w)
  (define p (world-params w))
  (define gs (params-grid-size p))
  (define ts (world-step-count w))
  (define living-count (hash-count (world-agents w)))
  (define food-count (hash-count (world-food w)))
  (define artifact-count (hash-count (world-artifacts w)))

  (define lines
    (list (format "┌──────────────────────────────────────────┐")
          (format "│ TerraLingua Step: ~a | Beings: ~a | Food: ~a | Arts: ~a │"
                  (~a ts #:width 5 #:align 'right)
                  (~a living-count #:width 3 #:align 'right)
                  (~a food-count #:width 4 #:align 'right)
                  (~a artifact-count #:width 3 #:align 'right))
          (format "├──────────────────────────────────────────┤")))

  (define grid-lines
    (for/list ([x (in-range gs)])
      (define row
        (for/list ([y (in-range gs)])
          (define cell (pos x y))
          (cond
            [(hash-has-key? (world-pos->agent w) cell)
             (define ag (world-agent w (hash-ref (world-pos->agent w) cell)))
             (define name (agent-name ag))
             (if (string=? name "") "B" (substring name 0 1))]
            [(hash-has-key? (world-food w) cell) "*"]
            [(pair? (artifacts-at w cell)) "A"]
            [else "."])))
      (string-append "│ " (string-join row " ") " │")))

  (define footer (list (format "└──────────────────────────────────────────┘")))
  (string-join (append lines grid-lines footer) "\n"))

;; Display rendered grid directly to current-output-port
(define (display-world-ascii w)
  (displayln (render-world-ascii w)))
