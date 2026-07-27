#lang racket

;;; Phase 3 — the simulation runner.
;;; Per step: fan out agent decisions (threads + parallelism) -> collect actions ->
;;; world/step -> append events to JSONL log -> checkpoint every ckpt-interval.
;;; Ctrl+C (exn:break) checkpoints the last fully committed step and exits.

(require racket/file
         racket/path
         "../world/state.rkt"
         "../world/step.rkt"
         "../world/obs.rkt"
         "../world/actions.rkt"
         (only-in "../agent/prompt.rkt" observation->string)
         "../agent/memory.rkt"
         "../eventlog/writer.rkt"
         "../ui/render.rkt"
         "checkpoint.rkt"
         "parallel.rkt"
         "spawn.rkt")

(provide run-simulation
         build-params-from-config
         checkpoint-due?)

;; Hash configuration accepts symbols (the DSL) or strings (JSON callers).
;; `hash-ref` defaults cannot distinguish a missing key from an explicit #f,
;; so use membership checks for all feature flags and optional values.
(define (config-ref cfg key default)
  (cond
    [(hash-has-key? cfg key) (hash-ref cfg key)]
    [(hash-has-key? cfg (symbol->string key))
     (hash-ref cfg (symbol->string key))]
    [else default]))

;; Build world params struct from config hash
(define (build-params-from-config cfg)
  (params
   (config-ref cfg 'grid-size 10)
   (config-ref cfg 'vision-radius 2)
   (config-ref cfg 'agent-lifespan 100)
   (config-ref cfg 'init-agent-energy 50)
   (config-ref cfg 'init-food 100)
   (config-ref cfg 'max-food-value 10.0)
   (config-ref cfg 'food-decay-rate 0.05)
   (config-ref cfg 'food-decay-amount 1.0)
   (config-ref cfg 'food-spawn-rate 1)
   (config-ref cfg 'food-zones #f)
   (config-ref cfg 'static-food #f)
   (config-ref cfg 'food-mechanism #t)
   (let ([df (config-ref cfg 'dead-agent-food 'single)])
     (if (string? df) (string->symbol df) df))
   (config-ref cfg 'reproduction-allowed #t)
   (config-ref cfg 'reproduction-cost 50)
   (config-ref cfg 'artifact-creation #t)
   (config-ref cfg 'artifact-creation-cost 0)
   (config-ref cfg 'inert-artifacts #f)
   (config-ref cfg 'use-inventory #t)
   (config-ref cfg 'use-internal-memory #t)
   (config-ref cfg 'use-colors #f)))

(define (checkpoint-due? completed-step interval)
  (and (exact-positive-integer? interval)
       (positive? completed-step)
       (zero? (modulo completed-step interval))))

;; Format the per-step info list (list of (cons key value)) as a string.
(define (format-info-list info)
  (cond
    [(null? info) ""]
    [else
     (string-join
      (map (λ (kv) (format "~a: ~a" (car kv) (cdr kv))) info)
      "; ")]))

;; Append a new history line, capped to max-history.
(define (append-history dec line)
  (define hist (cons line (agent-decision-state-history dec)))
  (define cap (agent-decision-state-max-history dec))
  (if (> (length hist) cap)
      (take hist cap)
      hist))

;; Build a fresh decision state for an agent that just acted and survived.
(define (update-decision-state dec completed-step obs-text info-text
                               act new-mem memory-budget)
  (define line (format "[step ~a] obs: ~a; info: ~a; action: ~a; message: ~a"
                       completed-step
                       obs-text info-text
                       (action-name act) (action-message act)))
  (struct-copy agent-decision-state dec
               [history (append-history dec line)]
               [memory (cap-internal-memory
                        (or new-mem (agent-decision-state-memory dec))
                        memory-budget)]))

(define (clone-prg prg)
  (vector->pseudo-random-generator
   (pseudo-random-generator->vector prg)))

;; Custom selectors share the explicit PRG and therefore remain sequential.
;; The no-transport fallback also uses this path to avoid pointless threads.
(define (query-agents-serial w states custom-selector prg)
  (for/fold ([actions (hash)] [mems (hash)] [obss (hash)])
            ([tag (in-list (sort (hash-keys (world-agents w)) symbol<?))])
    (define dec (hash-ref states tag #f))
    (define avail (available-actions w tag))
    (define obs (observe-agent w tag))
    (define chosen-act
      (if custom-selector
          (custom-selector w tag dec avail prg)
          (action "move" (hash "direction" "stay") "")))
    (values (hash-set actions tag chosen-act)
            mems
            (hash-set obss tag (observation->string obs)))))

;; Main simulation runner entry point
(define (run-simulation config
                        #:seed [seed 0]
                        #:checkpoint-path [resume-ckpt #f]
                        #:out-dir [out-dir "logs"]
                        #:transport [transport #f]
                        #:custom-selector [custom-selector #f]
                        #:max-steps-override [max-steps-override #f]
                        #:ui? [ui? #f])

  (define resuming? (and resume-ckpt #t))

  ;; Validate and restore before opening output resources. Supplying a missing
  ;; checkpoint is an error, never an implicit fresh run.
  (define-values (_step prg initial-world initial-states saved-config)
    (if resuming?
        (load-checkpoint resume-ckpt)
        (let ([fresh-prg (make-pseudo-random-generator)])
          (parameterize ([current-pseudo-random-generator fresh-prg])
            (random-seed seed))
          (define p (build-params-from-config config))
          (define-values (w0 states0)
            (spawn-initial-agents p config fresh-prg))
          (values 0 fresh-prg w0 states0 config))))

  ;; Checkpoint configuration is authoritative on resume. Enumerated explicit
  ;; overrides are applied to it and persisted in every subsequent checkpoint.
  (define cfg
    (if max-steps-override
        (hash-set saved-config 'max-ts max-steps-override)
        saved-config))
  (define max-ts (config-ref cfg 'max-ts 100))
  (define ckpt-interval (config-ref cfg 'ckpt-interval 10))
  (define memory-budget (config-ref cfg 'internal-memory-size 150))
  (define max-workers (config-ref cfg 'max-parallel-workers 8))
  (unless (exact-positive-integer? max-workers)
    (raise-arguments-error 'run-simulation
                           "max-parallel-workers must be a positive integer"
                           "max-parallel-workers" max-workers))

  (define log-dir (if (string? out-dir) (string->path out-dir) out-dir))
  (make-directory* log-dir)
  (define event-log-file (build-path log-dir "events.jsonl"))
  (define default-ckpt-path (build-path log-dir "checkpoint_latest.rktd"))
  (define logger (open-event-log event-log-file))

  (define committed-world (box initial-world))
  (define committed-states (box initial-states))
  (define committed-prg (box (clone-prg prg)))

  (define (save-committed!)
    (define w (unbox committed-world))
    (save-checkpoint default-ckpt-path
                     (world-step-count w)
                     (unbox committed-prg)
                     w
                     (unbox committed-states)
                     cfg))

  (dynamic-wind
    void
    (λ ()
      (with-handlers
        ([exn:break?
          (λ (_e)
            (parameterize-break
             #f
             (printf
              "\n⚠️ Received break signal (Ctrl+C). Checkpointing last completed step...\n")
             (save-committed!))
            (unbox committed-world))])

        ;; Resume appends to the existing log without duplicate bootstrap
        ;; records. A fresh run records its initial roster before stepping.
        (unless resuming?
          (parameterize-break
           #f
           (define p (world-params initial-world))
           (log-event! logger 0
                       (hasheq 'seed seed 'config cfg
                               'grid-size (params-grid-size p)
                               'vision-radius (params-vision-radius p))
                       'run-started)
           (log-event! logger 0
                       (hasheq 'agents
                               (sort (hash-keys (world-agents initial-world))
                                     symbol<?))
                       'env-reset)
           (for ([tag (in-list
                       (sort (hash-keys (world-agents initial-world))
                             symbol<?))])
             (define a (world-agent initial-world tag))
             (log-event! logger 0
                         (evt 'agent-added
                              (hash 'tag tag
                                    'name (agent-name a)
                                    'pos (agent-pos a)
                                    'agent-type 'text))))))

        (let loop ([w initial-world] [states initial-states])
          (define current-ts (world-step-count w))
          (cond
            [(or (>= current-ts max-ts) (hash-empty? (world-agents w)))
             (parameterize-break
              #f
              (log-event!
               logger current-ts
               (hasheq 'reason
                       (if (hash-empty? (world-agents w))
                           "all-died"
                           "max-steps"))
               'end-run)
              (set-box! committed-world w)
              (set-box! committed-states states)
              (set-box! committed-prg (clone-prg prg))
              (save-committed!))
             w]
            [else
             ;; Capture all decision inputs from this pre-step world.
             (define-values (actions new-mems new-obss)
               (cond
                 [custom-selector
                  (query-agents-serial w states custom-selector prg)]
                 [transport
                  (query-agents-parallel
                   w (world-agents w) states transport
                   #:max-workers max-workers)]
                 [else
                  (query-agents-serial w states #f prg)]))

             (define-values (w1 evts infos) (step w actions prg))
             (define-values (w2 states* respawn-evts)
               (respawn-to-minimum w1 states cfg prg))

             ;; Persist per-agent decision state for surviving agents.
             (define completed-step (world-step-count w2))
             (define states**
               (for/fold ([s states*])
                         ([tag (in-list
                                (sort (hash-keys (world-agents w2))
                                      symbol<?))])
                 (define dec (hash-ref s tag #f))
                 (cond
                   [(not dec) s]
                   [(not (hash-has-key? actions tag))
                    ;; Respawned this step; it already has fresh state.
                    s]
                   [else
                    (define act (hash-ref actions tag))
                    (define obs-text (hash-ref new-obss tag ""))
                    (define info-text
                      (format-info-list (hash-ref infos tag '())))
                    (define mem (hash-ref new-mems tag #f))
                    (hash-set
                     s tag
                     (update-decision-state
                      dec completed-step obs-text info-text
                      act mem memory-budget))])))

             ;; Event publication, committed state, PRG snapshot, and periodic
             ;; checkpoint advance under one break-disabled boundary.
             (parameterize-break
              #f
              (for ([e (in-list (append evts respawn-evts))])
                (log-event! logger current-ts e))
              (set-box! committed-world w2)
              (set-box! committed-states states**)
              (set-box! committed-prg (clone-prg prg))
              (when (checkpoint-due? completed-step ckpt-interval)
                (save-committed!)))

             (when ui? (display-world-ascii w2))
             (loop w2 states**)]))))
    (λ () (close-event-log logger))))
