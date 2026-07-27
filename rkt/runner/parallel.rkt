#lang racket

;;; Bounded parallel agent-decision engine.
;;; Every prompt input is prepared from one immutable pre-step world before
;;; worker threads begin. Results are collated by sorted tag.

(require racket/async-channel
         "../world/state.rkt"
         "../world/actions.rkt"
         "../world/obs.rkt"
         "../genome/main.rkt"
         "../agent/prompt.rkt"
         "../llm/agent-call.rkt"
         "spawn.rkt")

(provide query-agents-parallel)

(struct completed-job (tag value error) #:transparent)

(define (format-co-visible-messages msgs)
  (cond
    [(or (not msgs) (hash-empty? msgs)) ""]
    [else
     (string-join
      (for/list ([(name msg) (in-hash msgs)])
        (format "~a: ~a" name msg))
      "; ")]))

;; Run tagged thunks under a worker bound. Each job publishes either a value or
;; its exception, so one failed worker cannot leave the collector waiting
;; forever. The custodian stops outstanding workers if collection is escaped
;; (including Ctrl+C).
(define (run-bounded-jobs jobs max-workers
                          #:worker-created [worker-created void])
  (unless (exact-positive-integer? max-workers)
    (raise-argument-error 'run-bounded-jobs
                          "exact-positive-integer?"
                          max-workers))
  (define sema (make-semaphore max-workers))
  (define ch (make-async-channel))
  (define cust (make-custodian))
  (dynamic-wind
    void
    (λ ()
      (parameterize ([current-custodian cust])
        (for ([job (in-list jobs)])
          (define tag (car job))
          (define thunk (cdr job))
          (define worker
            (thread
             (λ ()
               (call-with-semaphore
                sema
                (λ ()
                  (define result
                    (with-handlers ([exn?
                                     (λ (e) (completed-job tag #f e))])
                      (completed-job tag (thunk) #f)))
                  (async-channel-put ch result))))))
          (worker-created tag worker)))
      (define results
        (sort (for/list ([_ (in-range (length jobs))])
                (async-channel-get ch))
              symbol<?
              #:key completed-job-tag))
      (define failed (findf completed-job-error results))
      (when failed (raise (completed-job-error failed)))
      results)
    (λ () (custodian-shutdown-all cust))))

;; -> (values (hash tag -> action)
;;            (hash tag -> memory-string)
;;            (hash tag -> observation-string))
(define (query-agents-parallel w agents-hash decision-states transport
                               #:max-workers [max-workers 8])
  (define tags (sort (hash-keys agents-hash) symbol<?))

  ;; Prepare all observations, action schemas, and render closures before any
  ;; request starts. The worker thunks contain no reads from evolving state.
  (define jobs
    (for/list ([tag (in-list tags)])
      (define ag (hash-ref agents-hash tag))
      (define dec (hash-ref decision-states tag #f))
      (define avail (available-actions w tag))
      (define obs (observe-agent w tag))
      (define obs-text (observation->string obs))
      (define query
        (λ ()
          (define-values (chosen-act new-mem)
            (if (and transport dec)
                (let ([sys-prompt
                       (render-system-prompt
                        (agent-name ag)
                        (world-params w)
                        (agent-decision-state-motivation dec))])
                  (call-llm-with-fallback
                   transport sys-prompt
                   (λ (hist)
                     (render-user-prompt
                      #:history (string-join hist "\n")
                      #:genome
                      (genome->string (agent-decision-state-genome dec))
                      #:observation obs-text
                      #:messages
                      (format-co-visible-messages (hash-ref obs 'messages))
                      #:energy (agent-energy ag)
                      #:time (agent-time-left ag)
                      #:memory (agent-decision-state-memory dec)
                      #:available-actions avail))
                   avail
                   #:history (agent-decision-state-history dec)))
                (values (action "move" (hash "direction" "stay") "") #f)))
          (list chosen-act new-mem obs-text)))
      (cons tag query)))

  (define results (run-bounded-jobs jobs max-workers))
  (for/fold ([actions (hash)] [mems (hash)] [obss (hash)])
            ([result (in-list results)])
    (define tag (completed-job-tag result))
    (define value (completed-job-value result))
    (define act (car value))
    (define mem (cadr value))
    (define obs-text (caddr value))
    (values (hash-set actions tag act)
            (if mem (hash-set mems tag mem) mems)
            (hash-set obss tag obs-text))))

(module+ test
  (require rackunit)

  (define lock (make-semaphore 1))
  (define active 0)
  (define peak 0)
  (define (tracked-job tag)
    (cons
     tag
     (λ ()
       (call-with-semaphore
        lock
        (λ ()
          (set! active (add1 active))
          (set! peak (max peak active))))
       (sleep 0.02)
       (call-with-semaphore lock (λ () (set! active (sub1 active))))
       tag)))

  (define bounded-results
    (run-bounded-jobs
     (for/list ([tag (in-list '(being3 being1 being2 being0))])
       (tracked-job tag))
     2))
  (check-equal? (map completed-job-tag bounded-results)
                '(being0 being1 being2 being3))
  (check-equal? peak 2 "worker concurrency is bounded")

  (check-exn
   #rx"worker exploded"
   (λ ()
     (run-bounded-jobs
      (list (cons 'being0 (λ () (error 'test "worker exploded")))
            (cons 'being1 (λ () 'ok)))
      2)))

  ;; Escaping the collector must shut down blocked workers rather than leave
  ;; the decision thread or its custodian hanging.
  (define blocked-worker-started (make-semaphore 0))
  (define blocked-worker-created (make-semaphore 0))
  (define blocked-worker-thread (box #f))
  (define blocked-runner
    (thread
     (λ ()
       (with-handlers ([exn:break? void])
         (run-bounded-jobs
          (list
           (cons 'being0
                 (λ ()
                   (semaphore-post blocked-worker-started)
                   (sync never-evt))))
          1
          #:worker-created
          (λ (_tag worker)
            (set-box! blocked-worker-thread worker)
            (semaphore-post blocked-worker-created)))))))
  (void (sync blocked-worker-created))
  (void (sync blocked-worker-started))
  (break-thread blocked-runner)
  (check-equal? (sync/timeout 1 blocked-runner)
                blocked-runner
                "cancelling collection terminates the collector")
  (define worker (unbox blocked-worker-thread))
  (check-equal? (sync/timeout 1 worker)
                worker
                "cancelling collection terminates blocked workers"))
