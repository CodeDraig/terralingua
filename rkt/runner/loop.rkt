#lang racket

;;; Phase 3 — the simulation runner.
;;; Per step: fan out agent decisions (threads + parallelism) -> collect actions ->
;;; world/step -> append events to JSONL log -> checkpoint every ckpt-interval.
;;; Ctrl+C (exn:break) finishes current step, checkpoints, exits.

(require racket/file
         racket/path
         json
         "../world/state.rkt"
         "../world/step.rkt"
         "../world/obs.rkt"
         "../world/actions.rkt"
         "../genome/main.rkt"
         "../agent/prompt.rkt"
         "../agent/parse.rkt"
         "../agent/memory.rkt"
         "../llm/transport.rkt"
         "../llm/agent-call.rkt"
         "../llm/retry.rkt"
         "../eventlog/writer.rkt"
         "../ui/render.rkt"
         "checkpoint.rkt"
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

;; Format the obs 'messages hash (name -> last broadcast from co-visible
;; beings) as a "Name: msg; Name2: msg2" string. The prompt template
;; uses this for the "Incoming messages" section.
(define (format-co-visible-messages msgs)
  (cond
    [(or (not msgs) (hash-empty? msgs)) ""]
    [else
     (string-join
      (for/list ([(name msg) (in-hash msgs)])
        (format "~a: ~a" name msg))
      "; ")]))

;; Append a new history line, capped to max-history.
(define (append-history dec line)
  (define hist (cons line (agent-decision-state-history dec)))
  (define cap (agent-decision-state-max-history dec))
  (if (> (length hist) cap)
      (take hist cap)
      hist))

;; Build a fresh decision state for an agent that just acted and survived.
(define (update-decision-state dec obs-text info-text act new-mem memory-budget)
  (define line (format "[step ~a] obs: ~a; info: ~a; action: ~a; message: ~a"
                       (agent-decision-state-tag dec)
                       obs-text info-text
                       (action-name act) (action-message act)))
  (struct-copy agent-decision-state dec
               [history (append-history dec line)]
               [memory (cap-internal-memory
                        (or new-mem (agent-decision-state-memory dec))
                        memory-budget)]))

;; Main simulation runner entry point
(define (run-simulation config
                        #:seed [seed 0]
                        #:checkpoint-path [resume-ckpt #f]
                        #:out-dir [out-dir "logs"]
                        #:transport [transport #f]
                        #:custom-selector [custom-selector #f]
                        #:max-steps-override [max-steps-override #f]
                        #:ui? [ui? #f])

  (define log-dir (if (string? out-dir) (string->path out-dir) out-dir))
  (make-directory* log-dir)
  (define event-log-file (build-path log-dir "events.jsonl"))
  (define default-ckpt-path (build-path log-dir "checkpoint_latest.rktd"))
  (define logger (open-event-log event-log-file))

  ;; The resume path intentionally skips re-emitting run-started/env-reset
  ;; so an appended log doesn't get duplicate bootstrap records.
  (define-values (_step prg w decision-states cfg)
    (if (and resume-ckpt (file-exists? resume-ckpt))
        (load-checkpoint resume-ckpt)
        (let ([prg (make-pseudo-random-generator)])
          ;; Seed the explicit PRG (not current-pseudo-random-generator):
          ;; world/genome/llm layers all thread `prg` explicitly, so
          ;; random-seed outside parameterize would not affect them.
          (parameterize ([current-pseudo-random-generator prg])
            (random-seed seed))
          (define p (build-params-from-config config))
          (define-values (w0 states0) (spawn-initial-agents p config prg))
          (log-event! logger 0 (hasheq 'seed seed 'config config
                                      'grid-size (params-grid-size p)
                                      'vision-radius (params-vision-radius p))
                      'run-started)
          (log-event! logger 0 (hasheq 'agents (hash-keys (world-agents w0))) 'env-reset)
          (for ([tag (in-list (sort (hash-keys (world-agents w0)) symbol<?))])
            (define a (world-agent w0 tag))
            (log-event! logger 0
                        (evt 'agent-added
                             (hash 'tag tag 'name (agent-name a) 'pos (agent-pos a)
                                   'agent-type 'text))))
          (values 0 prg w0 states0 config))))

  (define max-ts (or max-steps-override (hash-ref cfg 'max-ts #f) (hash-ref cfg "max-ts" 100)))
  (define ckpt-interval (or (hash-ref cfg 'ckpt-interval #f) (hash-ref cfg "ckpt-interval" 10)))
  (define memory-budget
    (or (hash-ref cfg 'internal-memory-size #f)
        (hash-ref cfg "internal-memory-size" 150)))

  (define final-world
    (with-handlers ([exn:break?
                     (λ (e)
                       (printf "\n⚠️ Received break signal (Ctrl+C). Checkpointing and exiting...\n")
                       (save-checkpoint default-ckpt-path (world-step-count w) prg w decision-states cfg)
                       (close-event-log logger)
                       w)])

      (let loop ([w w] [states decision-states])
        (define current-ts (world-step-count w))
        (cond
          [(or (>= current-ts max-ts) (hash-empty? (world-agents w)))
           (log-event! logger current-ts (hasheq 'reason (if (hash-empty? (world-agents w)) "all-died" "max-steps")) 'end-run)
           (save-checkpoint default-ckpt-path current-ts prg w states cfg)
           (close-event-log logger)
           w]
          [else
           ;; Select action per living agent. Also capture the per-agent
           ;; observation (used next step for history) and any returned
           ;; internal_memory string.
           (define-values (actions new-mems new-obss)
             (for/fold ([acts-h (hash)] [mems-h (hash)] [obss-h (hash)])
                       ([tag (in-list (sort (hash-keys (world-agents w)) symbol<?))])
               (define ag (world-agent w tag))
               (define dec (hash-ref states tag #f))
               (define avail (available-actions w tag))
               (define obs (observe-agent w tag))

                (define-values (chosen-act new-mem)
                  (cond
                    [custom-selector
                     (values (custom-selector w tag dec avail prg) #f)]
                    [(and transport dec)
                     (let ([sys-prompt
                            (render-system-prompt (agent-name ag) (world-params w)
                                                  (agent-decision-state-motivation dec))])
                       (call-llm-with-fallback
                        transport sys-prompt
                        (λ (hist)
                          (render-user-prompt
                           #:history (string-join hist "\n")
                           #:genome (genome->string (agent-decision-state-genome dec))
                           #:observation (observation->string obs)
                           #:messages (format-co-visible-messages (hash-ref obs 'messages))
                           #:energy (agent-energy ag)
                           #:time (agent-time-left ag)
                           #:memory (agent-decision-state-memory dec)
                           #:available-actions avail))
                        avail
                        #:history (agent-decision-state-history dec)))]
                    [else
                     (values (action "move" (hash "direction" "stay") "") #f)]))

               (values (hash-set acts-h tag chosen-act)
                       (if new-mem (hash-set mems-h tag new-mem) mems-h)
                       (hash-set obss-h tag (observation->string obs)))))

           ;; Step world
           (define-values (w1 evts infos) (step w actions prg))
           (for ([e (in-list evts)])
             (log-event! logger current-ts e))

           ;; Respawn to min-agents
           (define-values (w2 states* respawn-evts) (respawn-to-minimum w1 states cfg prg))
           (for ([e (in-list respawn-evts)])
             (log-event! logger current-ts e))

           ;; Persist per-agent decision state for surviving agents
           ;; (SPEC §5: history + internal_memory).
           (define states**
             (for/fold ([s states*])
                       ([tag (in-list (hash-keys (world-agents w2)))])
               (define dec (hash-ref s tag #f))
               (cond
                 [(not dec) s]
                 [(not (hash-has-key? actions tag))
                  ;; Agent was respawned this step — respawn-to-minimum
                  ;; already set a fresh decision state.
                  s]
                 [else
                  (define act (hash-ref actions tag))
                  (define obs-text (hash-ref new-obss tag ""))
                  (define info-text (format-info-list (hash-ref infos tag '())))
                  (define mem (hash-ref new-mems tag #f))
                  (hash-set s tag
                            (update-decision-state dec obs-text info-text
                                                   act mem memory-budget))])))

           (when ui?
             (display-world-ascii w2))

           ;; Checkpoint interval. w2's step-count is current-ts+1
           ;; (the just-completed step), so the checkpoint's step field
           ;; must match the world's step-count, not the pre-step current-ts.
           (when (checkpoint-due? (world-step-count w2) ckpt-interval)
             (save-checkpoint default-ckpt-path (world-step-count w2) prg w2 states** cfg))

           (loop w2 states**)]))))

  final-world)
