#lang racket

;;; Phase 7 Unit & Integration Tests — Terminal Visualizer

(require rackunit
         "../world/state.rkt"
         "../world/artifacts.rkt"
         "../ui/render.rkt")

(define p (params 4 2 100 50 100 10.0 0.05 1.0 1 #f #f #t 'single #t 50 #t 0 #f #t #t #f))
(define w0 (make-world p))
(define a0 (agent 'being0 "Aelion" (pos 1 1) 50 100 #f '() '() '() ""))
(define art0 (make-text-artifact "Scroll" "Magic" (list 'at (pos 2 2)) 'being0 -1 0))
(define w1 (world-put-artifact (world-add-agent w0 a0) art0))

(define ascii-str (render-world-ascii w1))
(check-true (string? ascii-str) "render-world-ascii returns string")
(check-true (string-contains? ascii-str "TerraLingua Step:") "header contains title")
(check-true (string-contains? ascii-str "A") "agent initial A displayed")
(check-true (string-contains? ascii-str "A") "artifact symbol A displayed")
