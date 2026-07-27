#lang racket

;;; Phase 10 Unit & Integration Tests — Parallel Worker Engine

(require rackunit
         "../world/state.rkt"
         "../world/obs.rkt"
         "../agent/prompt.rkt"
         "../runner/spawn.rkt"
         "../runner/parallel.rkt")

(define prg (make-pseudo-random-generator))
(random-seed 42)
(define p (params 6 2 100 50 100 10.0 0.05 1.0 1 #f #f #t 'single #t 50 #t 0 #f #t #t #f))
(define-values (w dec-states) (spawn-initial-agents p (hasheq 'init-agents 4) prg))

(define-values (actions mems obss)
  (query-agents-parallel
   w (world-agents w) dec-states #f #:max-workers 4))
(check-equal? (hash-count actions) 4 "all 4 agents returned action decisions")
(check-equal? (action-name (hash-ref actions 'being0)) "move" "returned move action struct")
(check-equal? (hash-count mems) 0 "no memory returned when transport is #f")
(check-equal? (hash-count obss) 4 "all observations are returned")
(for ([tag (in-list (sort (hash-keys (world-agents w)) symbol<?))])
  (check-equal? (hash-ref obss tag)
                (observation->string (observe-agent w tag))
                "returned observation comes from the pre-step world"))
