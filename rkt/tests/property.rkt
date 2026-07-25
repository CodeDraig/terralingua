#lang racket

;;; Property tests — SPEC §6 invariants over random rollouts, determinism,
;;; and state-is-data round-trips. Run: raco test rkt/

(require rackunit
         "../world/state.rkt"
         "../world/step.rkt"
         "../world/obs.rkt")

(define p
  (params 8 ; grid
          2 ; vision
          1000 ; lifespan (avoid old-age deaths)
          30 ; init-energy
          20 10.0 0.1 1.0 2 ; food: init 20, max 10, decay 0.1/1.0, spawn 2
          #f #f #t 'single #t 20 #t 0 #f #t #t #f)) ; repro cost 20

(define (mk-agent i)
  (define cell (pos (random 8) (random 8)))
  (agent (string->symbol (format "being~a" i))
         (format "Being~a" i)
         cell 30 1000 #f '() (list cell) '() ""))

;; Places agents on distinct cells.
(define (mk-world prg)
  (parameterize ([current-pseudo-random-generator prg])
    (let loop ([i 0] [w (make-world p)])
      (if (= i 6)
          w
          (let ([a (mk-agent i)])
            (if (hash-has-key? (world-pos->agent w) (agent-pos a))
                (loop i w)
                (loop (add1 i) (world-add-agent w a))))))))

(define directions '("up" "down" "left" "right" "stay"))

;; A random, mostly-plausible action for the agent in this world.
(define (random-action w tag prg)
  (define a (world-agent w tag))
  (define r (random 100 prg))
  (cond
    [(< r 40)
     (action "move"
             (hash "direction" (list-ref directions (random 5 prg)))
             (if (< (random prg) 0.3) "hello there" ""))]
    [(< r 50)
     (action "create_artifact"
             (hash "name" (format "art~a" (random 10000 prg))
                   "type" "text"
                   "payload" (make-string (random 40 prg) #\x)
                   "lifespan" (number->string (+ 1 (random 5 prg))))
             "")]
    [(< r 60)
     (define at-cell (artifacts-at w (agent-pos a)))
     (if (pair? at-cell)
         (action "pickup_artifact" (hash "name" (car at-cell)) "")
         (action "move" (hash "direction" "stay") ""))]
    [(< r 68)
     (if (pair? (agent-inventory a))
         (action "drop_artifact" (hash "name" (car (agent-inventory a))) "")
         (action "move" (hash "direction" "stay") ""))]
    [(< r 78)
     (action "reproduce"
             (hash "name" (format "Child~a" (random 10000 prg))
                   "energy" (number->string (random 10 prg)))
             "")]
    [(< r 90) ; garbage: exercises S1 coercion
     (action (format "bogus~a" (random 10 prg)) (hash "x" "y") "")]
    [else
     (action "move" (hash "direction" "stay") "")]))

(define (rollout seed steps)
  (define prg (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator prg])
    (random-seed seed))
  (define w0 (mk-world prg))
  (let loop ([w w0] [i 0] [adds 0] [deaths 0] [all-evts '()])
    (if (= i steps)
        (values w adds deaths all-evts)
        (let* ([actions
                (for/hash ([tag (in-hash-keys (world-agents w))])
                  (values tag (random-action w tag prg)))])
          (define-values (w-next evts infos) (step w actions prg))
          (define new-adds (for/sum ([e (in-list evts)]
                                     #:when (eq? (event-type e) 'agent-added))
                             1))
          (define new-deaths (for/sum ([e (in-list evts)]
                                       #:when (eq? (event-type e) 'agent-died))
                               1))
          ;; I1/I2/I5/I6 after every step
          (check-equal? (check-invariants w-next) '()
                        (format "invariants hold at step ~a" i))
          ;; I4: population accounting
          (check-equal? (hash-count (world-agents w-next))
                        (- (+ 6 adds new-adds) (+ deaths new-deaths))
                        (format "I4 population at step ~a" i))
          ;; observations are well-formed for every agent
          (for ([tag (in-hash-keys (world-agents w-next))])
            (check-true (hash? (observe-agent w-next tag))))
          (loop w-next (add1 i) (+ adds new-adds) (+ deaths new-deaths)
                (append all-evts evts))))))

(define-values (w-a adds-a deaths-a evts-a) (rollout 42 40))

;; Determinism: identical seed → identical world (spec §10 change #6).
(define-values (w-b adds-b deaths-b evts-b) (rollout 42 40))
(check-equal? w-b w-a "same seed, same world after 40 steps")
(check-equal? (length evts-b) (length evts-a) "same event stream")

;; Rule #1: write/read round-trip on a mid-run world (has food, artifacts,
;; chat, trajectories, archives).
(define round-tripped
  (read (open-input-string (with-output-to-string (λ () (write w-a))))))
(check-equal? round-tripped w-a "rich world write/read round-trip")

;; Sanity: the rollout actually exercised the mechanics it claims to.
(define evt-kinds (list->set (map event-type evts-a)))
(check-true (set-member? evt-kinds 'artifact-added) "rollout creates artifacts")
(check-true (set-member? evt-kinds 'agent-reproduced) "rollout reproduces")
