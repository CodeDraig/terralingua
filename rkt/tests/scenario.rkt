#lang racket

;;; Golden scenario tests — SPEC §2/§3 semantics, exact arithmetic.
;;; Run: raco test rkt/

(require rackunit
         "../world/state.rkt"
         "../world/step.rkt"
         "../world/actions.rkt"
         "../world/artifacts.rkt")

;; grid 10, vision 2, lifespan 100, init-energy 50, repro-cost 50
(define p
  (params 10 2 100 50
          100 10.0 0.05 1.0 1
          #f #f #t 'single #t 50 #t 0 #f #t #t #f))

(define (mk-agent tag name cell e)
  (agent tag name cell e 100 #f '() (list cell) '() ""))

(define (mk-world agents [food (hash)])
  (define w (struct-copy world (make-world p) [food food]))
  (for/fold ([w w]) ([a (in-list agents)]) (world-add-agent w a)))

(define (act name [params (hash)] [message ""])
  (action name params message))

(define (energy-of w tag) (agent-energy (world-agent w tag)))
(define (pos-of w tag) (agent-pos (world-agent w tag)))
(define (event-types evts) (map event-type evts))

;; ---- move & eat -----------------------------------------------------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 2 2) 50))
                    (hash (pos 2 3) 7))]
       [prg (make-pseudo-random-generator)]
       [actions (hash 'being0 (act "move" (hash "direction" "right")))])
  (define-values (w1 evts infos) (step w actions prg))
  (check-equal? (pos-of w1 'being0) (pos 2 3) "moved onto food cell")
  (check-equal? (energy-of w1 'being0) 56 "ate 7, drained 1: 50+7-1")
  (check-false (hash-has-key? (world-food w1) (pos 2 3)) "food at cell consumed")
  (check-equal? (event-types evts) '(agent-moved) "movement is logged for replay"))

;; ---- blocked move: no penalty, stay (spec §10 change #3) -------------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 2 2) 50)
                          (mk-agent 'being1 "Bbb" (pos 2 3) 50)))]
       [actions (hash 'being0 (act "move" (hash "direction" "right")))])
  (define-values (w1 evts infos) (step w actions (make-pseudo-random-generator)))
  (check-equal? (pos-of w1 'being0) (pos 2 2) "occupied cell: stay")
  (check-equal? (energy-of w1 'being0) 49 "only the drain"))

;; ---- give, across the toroidal seam (spec §10 change #1) -------------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 0 0) 50)
                          (mk-agent 'being1 "Bbb" (pos 0 9) 20)))]
       [actions (hash 'being0 (act "give" (hash "target" "Bbb" "amount" 15)))])
  (define-values (w1 evts infos) (step w actions (make-pseudo-random-generator)))
  (check-equal? (energy-of w1 'being0) 34 "50-15-1")
  (check-equal? (energy-of w1 'being1) 34 "20+15-1")
  (check-equal? (event-types evts) '(gift-energy))
  (check-equal? (hash-ref (event-data (car evts)) 'amount) 15))

;; ---- take ------------------------------------------------------------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 0 0) 50)
                          (mk-agent 'being1 "Bbb" (pos 0 1) 20)))]
       [actions (hash 'being0 (act "take" (hash "target" "Bbb" "amount" 10)))])
  (define-values (w1 evts infos) (step w actions (make-pseudo-random-generator)))
  (check-equal? (energy-of w1 'being0) 59 "50+10-1")
  (check-equal? (energy-of w1 'being1) 9 "20-10-1")
  (check-equal? (event-types evts) '(take-energy)))

;; ---- give to a non-nearby target: info note, no transfer -------------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 0 0) 50)
                          (mk-agent 'being1 "Bbb" (pos 5 5) 20)))]
       [actions (hash 'being0 (act "give" (hash "target" "Bbb" "amount" 10)))])
  (define-values (w1 evts infos) (step w actions (make-pseudo-random-generator)))
  (check-equal? (energy-of w1 'being0) 49 "no transfer, just drain")
  (check-equal? (energy-of w1 'being1) 19)
  (check-true (and (assoc "Action outcome" (hash-ref infos 'being0)) #t)
              "info explains the failure"))

;; ---- reproduction: exact energy arithmetic --------------------------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 5 5) 100)))]
       [actions (hash 'being0 (act "reproduce" (hash "name" "Kid" "energy" 30)))])
  (define-values (w1 evts infos) (step w actions (make-pseudo-random-generator)))
  (check-equal? (energy-of w1 'being0) 19 "100-50(cost)-30(gift)-1(drain)")
  (define child (world-agent w1 'being0_0))
  (check-equal? (agent-name child) "Kid")
  (check-equal? (agent-energy child) 79 "50(init)+30(gift)-1(drain)")
  (check-true (and (member 'being0_0 (agent-spawn (world-agent w1 'being0))) #t))
  (check-equal? (event-types evts) '(agent-added agent-reproduced))
  (check-true (< (chebyshev (pos-of w1 'being0) (agent-pos child) 10) 2)
              "child spawns in the 8-neighborhood"))

;; ---- reproduction: no free space (cost still paid) -------------------------
(let* ([parent (mk-agent 'being0 "Aaa" (pos 1 1) 100)]
       [neighbors (for/list ([i (in-range 8)]
                             [cell (in-list '((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2)))])
                    (mk-agent (string->symbol (format "n~a" i))
                              (format "N~a" i)
                              (pos (car cell) (cadr cell)) 50))]
       [w (mk-world (cons parent neighbors))]
       [actions (hash 'being0 (act "reproduce" (hash "name" "Kid" "energy" 0)))])
  (define-values (w1 evts infos) (step w actions (make-pseudo-random-generator)))
  (check-equal? (hash-count (world-agents w1)) 9 "no child")
  (check-equal? (energy-of w1 'being0) 49 "cost paid despite failure: 100-50-1")
  (check-equal? (event-types evts) '(agent-reproduced))
  (check-false (hash-ref (event-data (car evts)) 'successful)))

;; ---- artifact lifecycle: create -> pickup -> modify -> destroy -------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 2 2) 50)))]
       [prg (make-pseudo-random-generator)])
  ;; create (lifespan 2)
  (define-values (w1 ev1 in1)
    (step w (hash 'being0 (act "create_artifact"
                               (hash "name" "scroll" "type" "text"
                                     "payload" "hello" "lifespan" 2)))
          prg))
  (check-equal? (event-types ev1) '(artifact-added artifact-passive-interaction))
  (check-true (hash-has-key? (world-artifacts w1) "scroll"))
  ;; pickup
  (define-values (w2 ev2 in2)
    (step w1 (hash 'being0 (act "pickup_artifact" (hash "name" "scroll"))) prg))
  (check-equal? (event-types ev2) '(artifact-pickup artifact-passive-interaction))
  (check-equal? (artifact-location (hash-ref (world-artifacts w2) "scroll"))
                (list 'held 'being0))
  (check-true (and (member "scroll" (agent-inventory (world-agent w2 'being0))) #t))
  ;; modify
  (define-values (w3 ev3 in3)
    (step w2 (hash 'being0 (act "modify_artifact_scroll"
                                (hash "payload" "world" "lifespan" 5)))
          prg))
  (define art3 (hash-ref (world-artifacts w3) "scroll"))
  (check-equal? (artifact-payload art3) "world")
  (check-equal? (artifact-version art3) 1)
  (check-equal? (length (artifact-past-versions art3)) 1)
  ;; destroy
  (define-values (w4 ev4 in4)
    (step w3 (hash 'being0 (act "destroy_artifact_scroll" (hash))) prg))
  (check-false (hash-has-key? (world-artifacts w4) "scroll")
               "destroyed artifact expires in the same step's S5")
  (check-true (and (member 'artifact-interaction (event-types ev4))
                   (member 'artifact-removed (event-types ev4))
                   #t)))

;; ---- artifact expiry: not on creation step (spec §10 change #5) ------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 2 2) 50)))]
       [prg (make-pseudo-random-generator)])
  (define-values (w1 ev1 in1)
    (step w (hash 'being0 (act "create_artifact"
                               (hash "name" "ephemeral" "type" "text"
                                     "payload" "x" "lifespan" 1)))
          prg))
  (check-true (hash-has-key? (world-artifacts w1) "ephemeral")
              "lifespan-1 artifact survives its creation step")
  (define-values (w2 ev2 in2) (step w1 (hash) prg))
  (check-false (hash-has-key? (world-artifacts w2) "ephemeral")
               "and expires the next step")
  (check-equal? (event-types ev2) '(artifact-removed)))

;; ---- give_artifact ----------------------------------------------------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 2 2) 50)
                          (mk-agent 'being1 "Bbb" (pos 2 3) 50)))]
       [prg (make-pseudo-random-generator)])
  (define-values (w1 _1 _2)
    (step w (hash 'being0 (act "create_artifact"
                               (hash "name" "gem" "type" "text"
                                     "payload" "x" "lifespan" -1)))
          prg))
  (define-values (w2 _3 _4)
    (step w1 (hash 'being0 (act "pickup_artifact" (hash "name" "gem"))) prg))
  (define-values (w3 ev3 in3)
    (step w2 (hash 'being0 (act "give_artifact"
                                (hash "artifact_name" "gem" "target_agent" "Bbb")))
          prg))
  (check-equal? (event-types ev3) '(give-artifact))
  (check-true (and (member "gem" (agent-inventory (world-agent w3 'being1))) #t))
  (check-false (member "gem" (agent-inventory (world-agent w3 'being0))))
  (check-equal? (check-invariants w3) '()))

;; ---- S1 coercion: unknown action and bad params (spec §10 change #4) --------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 2 2) 50)))]
       [prg (make-pseudo-random-generator)])
  (define-values (w1 ev1 in1)
    (step w (hash 'being0 (act "dance" (hash))) prg))
  (check-equal? (pos-of w1 'being0) (pos 2 2) "unknown action -> stay")
  (check-equal? (energy-of w1 'being0) 49 "no penalty beyond the drain")
  (check-true (and (assoc "Action outcome" (hash-ref in1 'being0)) #t))
  (define-values (w2 ev2 in2)
    (step w (hash 'being0 (act "move" (hash "dir" "up"))) prg))
  (check-equal? (pos-of w2 'being0) (pos 2 2) "wrong param keys -> stay"))

;; ---- death by hunger: food drop, archive, I4 --------------------------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 4 4) 1)))]
       [actions (hash 'being0 (act "move" (hash "direction" "stay")))])
  (define-values (w1 evts infos) (step w actions (make-pseudo-random-generator)))
  (check-equal? (hash-count (world-agents w1)) 0 "dead and gone")
  (check-equal? (event-types evts) '(agent-died))
  (check-equal? (hash-ref (event-data (car evts)) 'reason) 'hunger)
  (check-equal? (hash-ref (world-food w1) (pos 4 4)) 10.0
                "single food drop at death cell (max of 10, energy)")
  (check-equal? (world-dead-names w1) '("Aaa"))
  (check-equal? (hash-ref (world-archived-trajectories w1) 'being0)
                (list (pos 4 4))))

;; ---- death by old age ---------------------------------------------------------
(let* ([a (agent 'being0 "Aaa" (pos 4 4) 100 1 #f '() (list (pos 4 4)) '() "")]
       [w (mk-world (list a))]
       [actions (hash 'being0 (act "move" (hash "direction" "stay")))])
  (define-values (w1 evts infos) (step w actions (make-pseudo-random-generator)))
  (check-equal? (hash-ref (event-data (car evts)) 'reason) 'old-age))

;; ---- availability rules -------------------------------------------------------
(let* ([w (mk-world (list (mk-agent 'being0 "Aaa" (pos 0 0) 30)
                          (mk-agent 'being1 "Bbb" (pos 0 1) 30)))])
  (define avail (available-actions w 'being0))
  (check-true (hash-has-key? avail "move"))
  (check-true (hash-has-key? avail "give") "nearby + food-mechanism")
  (check-true (hash-has-key? avail "take"))
  (check-true (hash-has-key? avail "create_artifact") "affordable (cost 0)")
  (check-false (hash-has-key? avail "reproduce") "30 energy < cost 50")
  (check-false (hash-has-key? avail "pickup_artifact") "nothing at cell")
  (check-false (hash-has-key? avail "set_color") "use-colors? is #f"))
