#lang racket

;;; Phase 5 Unit, Property & Integration Tests — Macro DSL (define-action, define-event)

(require rackunit
         "../world/state.rkt"
         "../world/actions.rkt"
         "../world/step.rkt"
         "../lang/dsl.rkt")

;; Clear custom action registry before testing
(clear-custom-actions!)

;; ---------------------------------------------------------------------------
;; 1. Test define-event Macro
;; ---------------------------------------------------------------------------

(define-event teleport
  #:fields (tag pos energy-cost))

(define tp-evt (make-teleport-event 'being0 (pos 5 5) 10))
(check-true (event? tp-evt) "event struct created")
(check-equal? (event-type tp-evt) 'teleport "event type is teleport")
(check-equal? (hash-ref (event-data tp-evt) 'tag) 'being0 "field tag stored")
(check-true (teleport-event? tp-evt) "teleport-event? predicate works")

;; ---------------------------------------------------------------------------
;; 2. Test define-action Macro
;; ---------------------------------------------------------------------------

(define-action teleport
  #:name "teleport"
  #:description "Teleport to a random cell on the grid. Costs 10 energy."
  #:params (hash "mode" "Mode of teleportation: [random, home]")
  #:available (λ (w tag) (>= (agent-energy (world-agent w tag)) 10))
  #:handler
  (λ (w tag params prg)
    (define a (world-agent w tag))
    (define gs (params-grid-size (world-params w)))
    (define new-pos (pos (random gs prg) (random gs prg)))
    (define a* (struct-copy agent a [energy (- (agent-energy a) 10)] [pos new-pos]))
    (values (world-move-agent w a*)
            (list (make-teleport-event tag new-pos 10))
            (list (cons "Teleport status" (format "Success to ~a" new-pos)))
            #f)))

(check-equal? (length (get-custom-actions)) 1 "custom action registered")

;; ---------------------------------------------------------------------------
;; 3. Verify Availability and Execution in World
;; ---------------------------------------------------------------------------

(define prg (make-pseudo-random-generator))
(random-seed 123)
(define p (params 10 2 100 50 100 10.0 0.05 1.0 1 #f #f #t 'single #t 50 #t 0 #f #t #t #f))
(define w0 (make-world p))
(define a0 (agent 'being0 "Aelion" (pos 1 1) 50 100 #f '() '() '() ""))
(define w1 (world-add-agent w0 a0))

(define avail (available-actions w1 'being0))
(check-true (hash-has-key? avail "teleport") "teleport action available when energy >= 10")
(check-equal? (hash-ref (hash-ref avail "teleport") 'description)
              "Teleport to a random cell on the grid. Costs 10 energy.")

;; Execute custom teleport action
(define act (action "teleport" (hash "mode" "random") ""))
(define-values (w2 evts infos delta) (execute-action w1 'being0 act prg))

(check-equal? (agent-energy (world-agent w2 'being0)) 40 "energy deducted by 10")
(check-equal? (length evts) 1 "1 teleport event emitted")
(check-true (teleport-event? (car evts)) "emitted teleport event")

;; ---------------------------------------------------------------------------
;; 4. Property Test — Low Energy Prevents Teleportation
;; ---------------------------------------------------------------------------

(define a-low (struct-copy agent a0 [energy 5]))
(define w-low (world-add-agent w0 a-low))
(define avail-low (available-actions w-low 'being0))
(check-false (hash-has-key? avail-low "teleport") "teleport action unavailable when energy < 10")

;; Clean up registry after tests
(clear-custom-actions!)
