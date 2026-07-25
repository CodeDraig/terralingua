#lang racket

;;; Scaffold smoke test — proves the harness: world construction, invariant
;;; checker, first-slice step, and (rule #1) state-is-data write/read
;;; round-trip. Run: raco test rkt/

(require rackunit
         "../world/state.rkt"
         "../world/step.rkt")

(define p
  (params 10 ; grid-size
          2 ; vision-radius
          100 ; lifespan
          50 ; init-energy
          100 10.0 0.05 1.0 1 ; food: init max decay-rate decay-amt spawn-rate
          #f #f #t ; zones static? mechanism?
          'single ; dead-agent-food
          #t 50 ; reproduction-allowed? cost
          #t 0 #f ; artifact-creation? cost inert?
          #t #t #f)) ; use-inventory? use-internal-memory? use-colors?

(define w0 (make-world p))
(define a0 (agent 'being0 "Aelion" (pos 2 2) 50 100 #f '() '() '() ""))
(define a1 (agent 'being1 "Mara" (pos 3 3) 50 100 #f '() '() '() ""))
(define w2 (world-add-agent (world-add-agent w0 a0) a1))

(check-equal? (check-invariants w2) '() "fresh world satisfies invariants")
(check-equal? (world-agent w2 'being0) a0 "lookup by tag")
(check-equal? (wrap-pos (pos -1 12) 10) (pos 9 2) "toroidal wrap")

(define-values (w3 events infos) (step w2 (hash) (make-pseudo-random-generator)))

(check-equal? (world-step-count w3) 1 "step counter increments")
(check-equal? (agent-energy (world-agent w3 'being0)) 49 "drain: -1 energy")
(check-equal? (agent-time-left (world-agent w3 'being1)) 99 "drain: -1 time")
(check-equal? events '() "first-slice step emits no events")
(check-equal? (check-invariants w3) '() "world still consistent after step")

;; Rule #1: state is data — write/read round-trips exactly.
(define round-tripped
  (read (open-input-string (with-output-to-string (λ () (write w3))))))
(check-equal? round-tripped w3 "world write/read round-trip")

;; Invariant checker catches corruption.
(define broken
  (struct-copy world w2 [pos->agent (hash-remove (world-pos->agent w2) (pos 2 2))]))
(check-false (null? (check-invariants broken)) "checker flags a broken index")
