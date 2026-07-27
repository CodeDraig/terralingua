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
(define w
  (world-add-agent
   (struct-copy world w0 [step-count 10])
   a0))
(define dec-states (hash 'being0 (agent-decision-state 'being0 (random-genome 'ocean5 prg) 'base '() "" 5)))
(define cfg (hasheq 'exp-name "test" 'max-ts 50))

(save-checkpoint ckpt-path 10 prg w dec-states cfg)
(check-true (file-exists? ckpt-path) "checkpoint file exists")
(define saved-checkpoint (call-with-input-file ckpt-path read))
(check-equal? (checkpoint-version saved-checkpoint) 2
              "new checkpoints use version 2")

(define-values (step-res prg-res w-res states-res cfg-res) (load-checkpoint ckpt-path))
(check-equal? step-res 10 "step count restored")
(check-equal? (world-agent w-res 'being0) a0 "world state restored")
(check-equal? (hash-count states-res) 1 "decision states restored")

;; Version 1 stored living trajectories chronologically. Loading migrates them
;; to newest-first while preserving the chronological consumer view.
(define v1-path (build-path test-dir "checkpoint_v1.rktd"))
(define v1-agent
  (struct-copy agent a0
               [trajectory (list (pos 2 2) (pos 2 3) (pos 2 4))]))
(define v1-world
  (world-add-agent
   (struct-copy world (make-world p) [step-count 10])
   v1-agent))
(define v1-datum
  (checkpoint 1 10 (pseudo-random-generator->vector prg)
              v1-world dec-states cfg))
(call-with-output-file v1-path
  (λ (out) (write v1-datum out) (newline out))
  #:exists 'truncate)
(define-values (_v1-step _v1-prg migrated-world _v1-states _v1-cfg)
  (load-checkpoint v1-path))
(define migrated-agent (world-agent migrated-world 'being0))
(check-equal? (agent-trajectory migrated-agent)
              (list (pos 2 4) (pos 2 3) (pos 2 2)))
(check-equal? (agent-trajectory-chronological migrated-agent)
              (list (pos 2 2) (pos 2 3) (pos 2 4)))

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
(define-values (_mid-step _mid-prg _mid-world mid-states mid-cfg)
  (load-checkpoint mid-ckpt))
(check-equal? (hash-ref mid-cfg 'max-ts) 10
              "explicit max-step override is persisted")
(define mid-history
  (agent-decision-state-history
   (hash-ref mid-states (car (sort (hash-keys mid-states) symbol<?)))))
(check-true (string-prefix? (car mid-history) "[step 10]")
            "history records the completed step number")

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
(define-values (_final-step _final-prg _final-world _final-states final-cfg)
  (load-checkpoint mid-ckpt))
(check-equal? (hash-ref final-cfg 'max-ts) 20
              "resume override remains durable")

;; Incoming simulation config is ignored on resume unless represented by an
;; explicit override keyword.
(define resume-policy-out (build-path test-dir "resume_policy"))
(define resume-policy-start
  (run-simulation (hasheq 'max-ts 1 'init-agents 1 'min-agents 0)
                  #:seed 31
                  #:out-dir resume-policy-out
                  #:custom-selector test-selector))
(define resume-policy-ckpt
  (build-path resume-policy-out "checkpoint_latest.rktd"))
(define resume-policy-result
  (run-simulation (hasheq 'max-ts 3 'init-agents 1 'min-agents 0)
                  #:checkpoint-path resume-policy-ckpt
                  #:out-dir resume-policy-out
                  #:custom-selector test-selector))
(check-equal? (world-step-count resume-policy-start) 1)
(check-equal? (world-step-count resume-policy-result) 1
              "checkpoint config is authoritative")

;; Invalid resume input fails before the output directory or log is created.
(define malformed-ckpt (build-path test-dir "malformed.rktd"))
(call-with-output-file malformed-ckpt
  (λ (out) (displayln "not-a-checkpoint" out))
  #:exists 'truncate)
(define malformed-payload-ckpt
  (build-path test-dir "malformed-payload.rktd"))
(call-with-output-file malformed-payload-ckpt
  (λ (out)
    (write
     (checkpoint 2 0
                 (pseudo-random-generator->vector prg)
                 "not-a-world"
                 (hash)
                 (hasheq 'max-ts 1))
     out)
    (newline out))
  #:exists 'truncate)
(for ([bad-path (in-list
                 (list malformed-ckpt
                       malformed-payload-ckpt
                       (build-path test-dir "missing.rktd")))]
      [name (in-list
             '("malformed-out" "malformed-payload-out" "missing-out"))])
  (define bad-out (build-path test-dir name))
  (check-exn
   exn:fail?
   (λ ()
     (run-simulation (hasheq 'max-ts 1 'init-agents 1)
                     #:checkpoint-path bad-path
                     #:out-dir bad-out
                     #:custom-selector test-selector)))
  (check-false (directory-exists? bad-out)
               "failed checkpoint load creates no output directory"))

;; Ctrl+C after one completed step restores the committed world, decision
;; state, and PRG rather than the initial or partially consumed state.
(define break-control-out (build-path test-dir "break_control"))
(define (random-stay-selector w tag dec avail prg)
  (random 100 prg)
  (action "move" (hash "direction" "stay") ""))
(void
 (run-simulation (hasheq 'max-ts 1 'init-agents 1 'min-agents 0)
                 #:seed 41
                 #:out-dir break-control-out
                 #:custom-selector random-stay-selector))
(define-values (_control-step control-prg _control-world
                             _control-states _control-cfg)
  (load-checkpoint
   (build-path break-control-out "checkpoint_latest.rktd")))

(define break-out (build-path test-dir "break_run"))
(define (breaking-selector w tag dec avail prg)
  (random 100 prg)
  (when (= (world-step-count w) 1)
    (break-thread (current-thread)))
  (action "move" (hash "direction" "stay") ""))
(define break-world
  (run-simulation (hasheq 'max-ts 3 'init-agents 1 'min-agents 0)
                  #:seed 41
                  #:out-dir break-out
                  #:custom-selector breaking-selector))
(define-values (break-step break-prg break-saved-world
                           break-states _break-cfg)
  (load-checkpoint (build-path break-out "checkpoint_latest.rktd")))
(check-equal? (world-step-count break-world) 1)
(check-equal? break-step 1)
(check-equal? (world-step-count break-saved-world) 1)
(check-equal? (pseudo-random-generator->vector break-prg)
              (pseudo-random-generator->vector control-prg)
              "partially consumed PRG state is not checkpointed")
(define break-history
  (agent-decision-state-history
   (hash-ref break-states (car (hash-keys break-states)))))
(check-true (string-prefix? (car break-history) "[step 1]"))
(define break-records
  (map string->jsexpr (file->lines (build-path break-out "events.jsonl"))))
(check-false
 (findf (λ (record) (equal? (hash-ref record 'type "") "end-run"))
        break-records)
 "interrupted run does not counterfeit a normal end-run")

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
