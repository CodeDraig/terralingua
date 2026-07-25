#lang racket

;;; Phase 3 — population management: initial roster, reproduction wiring
;;; (genome mutation), respawn-to-min-agents.
;;; NOTE (spec §10 change #9): respawned agents use the CONFIGURED genome and
;;; motivation — the Python respawn path silently used defaults (bug).

(require "../world/state.rkt"
         "../world/food.rkt"
         "../genome/main.rkt")

(provide (struct-out agent-decision-state)
         spawn-initial-agents
         respawn-to-minimum)

(struct agent-decision-state (tag genome motivation history memory max-history) #:prefab)

;; Helper to find an unoccupied cell on the grid. Only checks for
;; other agents (not food or artifacts) — agents can spawn on top of
;; food, which they'll consume on their first step, or on artifacts,
;; which is harmless. This matches the original (pre-fix-7) behavior
;; and avoids the chicken-and-egg with food seeding.
;;
;; Strategy: sample randomly first (fast for sparse worlds), then if
;; 500 attempts don't find one, deterministically scan the grid. If
;; even the scan fails, return #f rather than an occupied cell — the
;; caller can decide whether to abort or retry on a later step.
(define (find-free-cell w prg)
  (define gs (params-grid-size (world-params w)))
  (define (random-cell) (pos (random gs prg) (random gs prg)))
  (define (free? p) (not (hash-has-key? (world-pos->agent w) p)))
  (let loop ([attempts 0])
    (cond
      [(> attempts 500) (scan-free-cell w free? random-cell gs)]
      [else
       (define p (random-cell))
       (if (free? p) p (loop (add1 attempts)))])))

;; Fallback: scan the entire grid in randomized order. Returns #f if
;; the grid is fully occupied (caller should treat that as an error).
(define (scan-free-cell w free? random-cell gs)
  (let loop ([remaining (* gs gs)])
    (cond
      [(zero? remaining) #f]
      [else
       (define p (random-cell))
       (if (free? p) p (loop (sub1 remaining)))])))

(define (spawn-initial-agents p config prg)
  (define init-count (or (hash-ref config 'init-agents #f) (hash-ref config "init-agents" 4)))
  (define prefix (or (hash-ref config 'agents-name-prefix #f) (hash-ref config "agents-name-prefix" "Being")))
  (define genome-kind (or (hash-ref config 'genome #f) (hash-ref config "genome" 'ocean5)))
  (define motivation (or (hash-ref config 'exogenous-motivation #f) (hash-ref config "exogenous-motivation" 'base)))
  (define max-hist (or (hash-ref config 'max-history #f) (hash-ref config "max-history" 5)))

;; Build a fully-initialized world (food already seeded) and place
;; agents. The food is seeded first, so an init-food of ~grid^2 may
;; leave no fully-empty cells; in that case agents spawn on top of
;; food and consume it on the first step.
  (define-values (w* dec-states)
    (for/fold ([w (make-world-with-food p prg)] [states (hash)])
              ([i (in-range init-count)])
      (define tag (string->symbol (format "being~a" i)))
      (define name (format "~a~a" prefix i))
      (define cell (find-free-cell w prg))
      (when (not cell)
        (error 'spawn-initial-agents
               "no free cell for agent ~a; grid-size too small for init-agents"
               i))
      (define ag (agent tag name cell (params-init-energy p) (params-lifespan p) #f '() (list cell) '() ""))
      (define g (random-genome (if (string? genome-kind) (string->symbol genome-kind) genome-kind) prg))
      (define dec (agent-decision-state tag g motivation '() "" max-hist))
      (values (world-add-agent w ag)
              (hash-set states tag dec))))
  (values w* dec-states))

(define (respawn-to-minimum w decision-states config prg)
  (define p (world-params w))
  (define min-agents (or (hash-ref config 'min-agents #f) (hash-ref config "min-agents" 0)))
  (define current-count (hash-count (world-agents w)))
  (define needed (- min-agents current-count))

  (if (<= needed 0)
      (values w decision-states '())
      (let* ([prefix (or (hash-ref config 'agents-name-prefix #f) (hash-ref config "agents-name-prefix" "Being"))]
             [genome-kind (or (hash-ref config 'genome #f) (hash-ref config "genome" 'ocean5))]
             [motivation (or (hash-ref config 'exogenous-motivation #f) (hash-ref config "exogenous-motivation" 'base))]
             [max-hist (or (hash-ref config 'max-history #f) (hash-ref config "max-history" 5))])
        (for/fold ([w w] [states decision-states] [evts '()])
                  ([i (in-range needed)])
          (define next-idx (+ (hash-count (world-agents w)) (length (world-dead-names w))))
          (define tag (string->symbol (format "being~a" next-idx)))
          (define requested-name (format "~a~a" prefix next-idx))
          (define name (unique-name w requested-name))
          (define cell (or (find-free-cell w prg)
                          (error 'respawn-to-minimum
                                 "no free cell for respawned agent")))
          (define ag (agent tag name cell (params-init-energy p) (params-lifespan p) #f '() (list cell) '() ""))
          (define g (random-genome (if (string? genome-kind) (string->symbol genome-kind) genome-kind) prg))
          (define dec (agent-decision-state tag g motivation '() "" max-hist))
          (values (world-add-agent w ag)
                  (hash-set states tag dec)
                  (cons (evt 'agent-added
                             (hash 'tag tag 'name name 'pos cell 'agent-type 'text))
                        evts))))))
