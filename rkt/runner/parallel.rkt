#lang racket

;;; Phase 10 — Multi-Threaded Parallel Worker Engine
;;;
;;; Queries LLM decisions for multiple agents concurrently using worker
;;; threads and a semaphore. Returns (values actions mems) so the caller
;;; can update per-agent decision state (history + internal_memory) the
;;; same way the serial loop does.

(require racket/async-channel
         "../world/state.rkt"
         "../world/actions.rkt"
         "../world/obs.rkt"
         "../genome/main.rkt"
         "../agent/prompt.rkt"
         "../agent/parse.rkt"
         "../llm/agent-call.rkt"
         "spawn.rkt")

(provide query-agents-parallel)

;; Query decisions for multiple agents concurrently using worker threads and a semaphore.
;; Returns (values (hash tag -> action) (hash tag -> (or #f mem-string))).
(define (query-agents-parallel w agents-hash decision-states transport #:max-workers [max-workers 8])
  (define sema (make-semaphore max-workers))
  (define ch (make-async-channel))

  (define tags (sort (hash-keys agents-hash) symbol<?))
  (define total (length tags))

  (for ([tag (in-list tags)])
    (thread
     (λ ()
       (call-with-semaphore sema
         (λ ()
           (define ag (hash-ref agents-hash tag))
           (define dec (hash-ref decision-states tag #f))
           (define avail (available-actions w tag))
           (define-values (chosen-act new-mem)
             (if (and transport dec)
                 (let* ([sys-prompt (render-system-prompt (agent-name ag) (world-params w) (agent-decision-state-motivation dec))]
                        [obs (observe-agent w tag)]
                        [co-visible-msg (let ([m (hash-ref obs 'messages)])
                                          (if (hash-empty? m)
                                              ""
                                              (string-join
                                               (for/list ([(n t) (in-hash m)])
                                                 (format "~a: ~a" n t))
                                               "; ")))])
                   (call-llm-with-fallback
                    transport sys-prompt
                    (λ (hist)
                      (render-user-prompt
                       #:history (string-join hist "\n")
                       #:genome (genome->string (agent-decision-state-genome dec))
                       #:observation (observation->string obs)
                       #:messages co-visible-msg
                       #:energy (agent-energy ag)
                       #:time (agent-time-left ag)
                       #:memory (agent-decision-state-memory dec)
                       #:available-actions avail))
                    avail
                    #:history (agent-decision-state-history dec)))
                 (values (action "move" (hash "direction" "stay") "") #f)))
           (async-channel-put ch (list tag chosen-act new-mem)))))))

  (for/fold ([acts (hash)] [mems (hash)])
            ([_ (in-range total)])
    (define res (async-channel-get ch))
    (define tag (car res))
    (define act (cadr res))
    (define mem (caddr res))
    (values (hash-set acts tag act)
            (if mem (hash-set mems tag mem) mems))))
