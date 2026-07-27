#lang racket

;;; Phase 4 Unit & Integration Tests — #lang terralingua & 8 Paper Config Ports

(require rackunit
         racket/file
         racket/path
         "../world/state.rkt"
         "../runner/loop.rkt")

(define test-dir (make-temporary-file "tl_phase4_dir_~a" 'directory))

(define (get-module-dir)
  (define rmp (module-path-index-resolve (syntax-source-module #'here)))
  (define p (resolved-module-path-name rmp))
  (if (path? p) (path-only p) (current-directory)))

;; ---------------------------------------------------------------------------
;; 1. Verify All 8 Paper Experiment Configurations Load Cleanly
;; ---------------------------------------------------------------------------

(define config-files
  '("core.rkt"
    "abundant.rkt"
    "creative.rkt"
    "artifact_cost.rkt"
    "inert_artifacts.rkt"
    "long_memory.rkt"
    "no_motivation.rkt"
    "no_personality.rkt"))

(for ([f (in-list config-files)])
  (define p (simplify-path (build-path (get-module-dir) "../configs" f)))
  (check-true (file-exists? p) (format "Config file ~a exists at ~a" f p))
  (define cfg (dynamic-require p 'config))
  (check-true (hash? cfg) (format "Config ~a loaded as hash" f))
  (check-true (hash-has-key? cfg 'exp-name) (format "Config ~a has exp-name" f)))

;; ---------------------------------------------------------------------------
;; 2. Verify Specific Parameter Fields in Ported Configurations
;; ---------------------------------------------------------------------------

(define (get-cfg name)
  (define p (simplify-path (build-path (get-module-dir) "../configs" name)))
  (dynamic-require p 'config))

(define core-cfg (get-cfg "core.rkt"))
(check-equal? (hash-ref core-cfg 'exp-name) "core_run")
(check-equal? (hash-ref core-cfg 'exogenous-motivation) 'base)
(check-equal? (hash-ref core-cfg 'genome) 'ocean5)
(check-equal? (hash-ref core-cfg 'max-parallel-workers) 8)

(define creative-cfg (get-cfg "creative.rkt"))
(check-equal? (hash-ref creative-cfg 'exogenous-motivation) 'creative)

(define cost-cfg (get-cfg "artifact_cost.rkt"))
(check-equal? (hash-ref cost-cfg 'artifact-creation-cost) 10)

(define inert-cfg (get-cfg "inert_artifacts.rkt"))
(check-true (hash-ref inert-cfg 'inert-artifacts))

(define memory-cfg (get-cfg "long_memory.rkt"))
(check-equal? (hash-ref memory-cfg 'max-history) 20)

(define no-mot-cfg (get-cfg "no_motivation.rkt"))
(check-equal? (hash-ref no-mot-cfg 'exogenous-motivation) 'none)

(define no-pers-cfg (get-cfg "no_personality.rkt"))
(check-equal? (hash-ref no-pers-cfg 'genome) 'notraits)

;; The DSL must retain all runner-facing controls, including explicit #f
;; values and an endpoint selected by a config rather than the environment.
(define controls-config-path (build-path test-dir "controls.rkt"))
(call-with-output-file controls-config-path
  (λ (out)
    (displayln "#lang terralingua" out)
    (displayln "(experiment #:name \"controls\" #:openai-base-url \"http://example.test/v1\" #:food-mechanism #f #:reproduction-allowed #f #:artifact-creation #f #:use-inventory #f)" out))
  #:exists 'truncate)
(define controls-cfg (dynamic-require controls-config-path 'config))
(check-equal? (hash-ref controls-cfg 'openai-base-url) "http://example.test/v1")
(for ([key (in-list '(food-mechanism reproduction-allowed artifact-creation use-inventory))])
  (check-true (hash-has-key? controls-cfg key) (format "DSL emits ~a" key))
  (check-false (hash-ref controls-cfg key) (format "DSL preserves ~a = #f" key)))
(define controls-params (build-params-from-config controls-cfg))
(check-false (params-food-mechanism? controls-params))
(check-false (params-reproduction-allowed? controls-params))
(check-false (params-artifact-creation? controls-params))
(check-false (params-use-inventory? controls-params))

;; ---------------------------------------------------------------------------
;; 3. Cost-Capped Live Simulation Smoke Run (Phase 4 Gate!)
;; ---------------------------------------------------------------------------

(define smoke-out (build-path test-dir "smoke_run"))
(define (mock-selector w tag dec avail prg)
  (action "move" (hash "direction" "stay") ""))

(define smoke-w
  (run-simulation core-cfg
                  #:seed 999
                  #:out-dir smoke-out
                  #:custom-selector mock-selector
                  #:max-steps-override 5))

(check-equal? (world-step-count smoke-w) 5 "Smoke simulation ran 5 timesteps")
(check-true (file-exists? (build-path smoke-out "events.jsonl")) "Events JSONL log generated")
(check-true (file-exists? (build-path smoke-out "checkpoint_latest.rktd")) "Final checkpoint generated")

;; Clean up temporary test directory
(delete-directory/files test-dir)
