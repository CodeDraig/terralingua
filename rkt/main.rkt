#lang racket

;;; TerraLingua (rkt) — CLI entry.

(require racket/cmdline
         racket/file
         json
         "runner/loop.rkt"
         "runner/checkpoint.rkt"
         "llm/transport.rkt")

(provide main
         make-transport-from-config)

(define config-param (make-parameter #f))
(define seed-param (make-parameter 0))
(define checkpoint-param (make-parameter #f))
(define out-dir-param (make-parameter "logs"))
(define max-ts-param (make-parameter #f))
(define provider-param (make-parameter #f))
(define model-param (make-parameter #f))
(define openai-base-url-param (make-parameter #f))
(define ui-param (make-parameter #f))

(define max-seed (sub1 (expt 2 31)))

(define (parse-seed value)
  (define parsed (string->number value))
  (unless (and (exact-integer? parsed) (<= 0 parsed max-seed))
    (raise-user-error
     'terralingua
     (format "--seed must be an integer from 0 through ~a; received ~s"
             max-seed value)))
  parsed)

(define (config-value config key default)
  (cond [(hash-has-key? config key) (hash-ref config key)]
        [(hash-has-key? config (symbol->string key))
         (hash-ref config (symbol->string key))]
        [else default]))

(define (make-transport-from-config config)
  (define provider-str (config-value config 'provider "openai"))
  (define model-str (config-value config 'model "gpt-4o-mini"))
  (define openai-base-url (config-value config 'openai-base-url #f))
  (case (if (string? provider-str) (string->symbol provider-str) provider-str)
    [(anthropic) (make-anthropic-transport model-str)]
    [else (make-openai-transport model-str #:base-url openai-base-url)]))

(define (main argv)
  ;; `main` is exercised repeatedly by tests; do not leak option values between
  ;; invocations in the same Racket process.
  (config-param #f)
  (seed-param 0)
  (checkpoint-param #f)
  (out-dir-param "logs")
  (max-ts-param #f)
  (provider-param #f)
  (model-param #f)
  (openai-base-url-param #f)
  (ui-param #f)

  (command-line
   #:argv argv
   #:usage-help "terralingua (rkt) — TerraLingua simulation core"
   #:once-each
   [("--config") path "Experiment config file (#lang terralingua)" (config-param path)]
   [("--seed") n "Master RNG seed" (seed-param (parse-seed n))]
   [("--checkpoint") path "Checkpoint path to resume from" (checkpoint-param path)]
   [("--out-dir") path "Output directory for logs and checkpoints" (out-dir-param path)]
   [("--max-ts") n "Maximum timesteps to simulate" (max-ts-param (string->number n))]
   [("--provider") p "LLM provider (openai or anthropic)" (provider-param p)]
   [("--model") m "LLM model name" (model-param m)]
   [("--openai-base-url") url "OpenAI-compatible /v1 API base URL" (openai-base-url-param url)]
   [("--ui") "Enable live ASCII terminal visualization grid" (ui-param #t)]
   #:args ()
   (void))

  (when (and (config-param) (checkpoint-param))
    (raise-user-error
     'terralingua
     "--config and --checkpoint cannot be used together; resume uses checkpoint configuration plus explicit CLI overrides"))

  (define loaded-config
    (cond
      [(checkpoint-param)
       (define-values (_step _prg _world _states checkpoint-config)
         (load-checkpoint (checkpoint-param)))
       checkpoint-config]
      [(config-param)
       (dynamic-require (string->path (config-param)) 'config)]
      [else
       (hasheq 'exp-name "cli-run"
               'max-ts 100
               'provider "openai"
               'model "gpt-4o-mini"
               'init-agents 4
               'min-agents 2
               'ckpt-interval 10
               'max-parallel-workers 8)]))

  (define config
    (for/fold ([cfg loaded-config])
              ([kv (in-list (list (cons 'max-ts (max-ts-param))
                                  (cons 'provider (provider-param))
                                  (cons 'model (model-param))
                                  (cons 'openai-base-url (openai-base-url-param))))]
               #:when (cdr kv))
      (hash-set cfg (car kv) (cdr kv))))

  (define transport (make-transport-from-config config))

  (printf "Starting TerraLingua simulation (seed: ~a, exp: ~a, out-dir: ~a)...\n"
          (seed-param) (hash-ref config 'exp-name "experiment") (out-dir-param))

  (run-simulation config
                  #:seed (seed-param)
                  #:checkpoint-path (checkpoint-param)
                  #:out-dir (out-dir-param)
                  #:transport transport
                  #:max-steps-override (max-ts-param)
                  #:ui? (ui-param)))

(module+ main
  (main (current-command-line-arguments)))
