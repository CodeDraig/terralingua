#lang racket

;;; Phase 3 Unit & Integration Tests — Runner, Checkpoints, EventLog & Resume

(require rackunit
         racket/file
         racket/path
         json
         "../world/state.rkt"
         "../world/step.rkt"
         "../world/actions.rkt"
         "../genome/main.rkt"
         "../eventlog/writer.rkt"
         "../runner/checkpoint.rkt"
         "../runner/spawn.rkt"
         "../runner/loop.rkt")

(define test-dir (make-temporary-file "tl_test_dir_~a" 'directory))

;; ---------------------------------------------------------------------------
;; 1. Event Log Writer Tests
;; ---------------------------------------------------------------------------

(define log-path (build-path test-dir "events.jsonl"))
(define logger (open-event-log log-path))

(log-event! logger 0 (hasheq 'seed 42) 'run-started)
(log-event! logger 1 (evt 'agent-added (hash 'tag 'being0 'name "Aelion" 'pos (pos 1 1))))
(close-event-log logger)

(check-true (file-exists? log-path) "event log file created")
(define lines (file->lines log-path))
(check-equal? (length lines) 2 "logged 2 JSONL lines")

(define entry0 (string->jsexpr (car lines)))
(check-equal? (hash-ref entry0 'v) 1 "version = 1")
(check-equal? (hash-ref entry0 'ts) 0 "ts = 0")
(check-equal? (hash-ref entry0 'type) "run-started" "type = run-started")

;; ---------------------------------------------------------------------------
;; 2. Atomic Checkpoint Save/Load Tests
;; ---------------------------------------------------------------------------

(define ckpt-path (build-path test-dir "checkpoint_latest.rktd"))
(define prg (make-pseudo-random-generator))
(random-seed 42)
(define p (params 8 2 100 50 100 10.0 0.05 1.0 1 #f #f #t 'single #t 50 #t 0 #f #t #t #f))
(define w0 (make-world p))
(define a0 (agent 'being0 "Aelion" (pos 2 2) 50 100 #f '() '() '() ""))
(define w (world-add-agent w0 a0))
(define dec-states (hash 'being0 (agent-decision-state 'being0 (random-genome 'ocean5 prg) 'base '() "" 5)))
(define cfg (hasheq 'exp-name "test" 'max-ts 50))

(save-checkpoint ckpt-path 10 prg w dec-states cfg)
(check-true (file-exists? ckpt-path) "checkpoint file exists")

(define-values (step-res prg-res w-res states-res cfg-res) (load-checkpoint ckpt-path))
(check-equal? step-res 10 "step count restored")
(check-equal? (world-agent w-res 'being0) a0 "world state restored")
(check-equal? (hash-count states-res) 1 "decision states restored")

;; ---------------------------------------------------------------------------
;; 3. Population Spawning & Respawning Tests
;; ---------------------------------------------------------------------------

(define-values (w-init states-init) (spawn-initial-agents p (hasheq 'init-agents 4 'genome 'ocean5 'exogenous-motivation 'base) prg))
(check-equal? (hash-count (world-agents w-init)) 4 "spawns initial agents")

;; Explicit #f config values must survive into the world rather than falling
;; through to their #t defaults.
(define disabled-params
  (build-params-from-config
   (hasheq 'food-mechanism #f 'reproduction-allowed #f
           'artifact-creation #f 'use-inventory #f)))
(check-false (params-food-mechanism? disabled-params))
(check-false (params-reproduction-allowed? disabled-params))
(check-false (params-artifact-creation? disabled-params))
(check-false (params-use-inventory? disabled-params))

;; Kill two agents and respawn to min-agents
(define tags (hash-keys (world-agents w-init)))
(define w-killed (world-remove-agent (world-remove-agent w-init (car tags)) (cadr tags)))
(check-equal? (hash-count (world-agents w-killed)) 2 "2 living agents remain")

(define-values (w-respawned states-respawned resp-evts)
  (respawn-to-minimum w-killed states-init (hasheq 'min-agents 4 'genome 'ocean5 'exogenous-motivation 'base) prg))

(check-equal? (hash-count (world-agents w-respawned)) 4 "respawns back to minimum 4 agents")
(check-equal? (length resp-evts) 2 "emitted 2 agent-added events")

;; ---------------------------------------------------------------------------
;; 4. Simulation Loop & Resume Integration (Phase 3 Gate!)
;; ---------------------------------------------------------------------------

(define loop-out (build-path test-dir "loop_run"))
(define loop-config (hasheq 'max-ts 20 'ckpt-interval 5 'init-agents 4 'min-agents 2 'genome 'ocean5))

;; Custom step selector that moves beings stay
(define (test-selector w tag dec avail prg)
  (action "move" (hash "direction" "stay") ""))

;; Run simulation for 10 steps
(define w-mid
  (run-simulation loop-config
                  #:seed 123
                  #:out-dir loop-out
                  #:custom-selector test-selector
                  #:max-steps-override 10))

(check-equal? (world-step-count w-mid) 10 "ran 10 steps")
(define mid-ckpt (build-path loop-out "checkpoint_latest.rktd"))
(check-true (file-exists? mid-ckpt) "mid checkpoint saved")

;; A 10-step cadence writes exactly after steps 10, 20, ...; zero disables
;; periodic checkpointing while the runner still writes its final checkpoint.
(check-false (checkpoint-due? 9 10))
(check-true (checkpoint-due? 10 10))
(check-false (checkpoint-due? 11 10))
(check-false (checkpoint-due? 10 0))

;; Resume simulation from checkpoint to step 20
(define w-final
  (run-simulation loop-config
                  #:seed 123
                  #:checkpoint-path mid-ckpt
                  #:out-dir loop-out
                  #:custom-selector test-selector
                  #:max-steps-override 20))

(check-equal? (world-step-count w-final) 20 "resumed and ran to step 20")

;; Initial roster records are needed to reconstruct the complete run from
;; events.jsonl, not merely respawns and reproduction.
(define initial-log-out (build-path test-dir "initial_log"))
(void
 (run-simulation (hasheq 'max-ts 0 'init-agents 3 'min-agents 0)
                 #:seed 7
                 #:out-dir initial-log-out
                 #:custom-selector test-selector))
(define initial-records
  (map string->jsexpr (file->lines (build-path initial-log-out "events.jsonl"))))
(define initial-agent-records
  (filter (λ (record) (equal? (hash-ref record 'type "") "agent-added"))
          initial-records))
(check-equal? (length initial-agent-records) 3 "every initial agent is logged")
(for ([record (in-list initial-agent-records)])
  (check-true (string? (hash-ref record 'name #f)))
  (check-true (hash? (hash-ref record 'pos #f))))

;; Step-level movement reaches the JSONL stream used by the dashboard.
(define movement-log-out (build-path test-dir "movement_log"))
(void
 (run-simulation (hasheq 'max-ts 1 'init-agents 1 'min-agents 0)
                 #:seed 11
                 #:out-dir movement-log-out
                 #:custom-selector
                 (λ (w tag dec avail prg)
                   (action "move" (hash "direction" "right") ""))))
(define movement-records
  (map string->jsexpr (file->lines (build-path movement-log-out "events.jsonl"))))
(define move-record
  (findf (λ (record) (equal? (hash-ref record 'type "") "agent-moved"))
         movement-records))
(check-true (hash? move-record) "moving agents emit replayable position events")
(check-true (hash? (hash-ref move-record 'pos #f)))

;; Clean up temporary test directory
(delete-directory/files test-dir)
