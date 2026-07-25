#lang racket

;;; SPEC §2 — the step pipeline.
;;; step : World (Hash Tag -> Action) PRG -> (values World (Listof Event) (Hash Tag -> Info))
;;; Agents act in sorted-tag order (spec §10 change #6). Observations and
;;; available-actions are pure functions (world/obs.rkt, world/actions.rkt),
;;; computed on demand outside step.

(require "state.rkt"
         "actions.rkt"
         "artifacts.rkt"
         "food.rkt")

(provide step)

(define (step w actions prg)
  (define step-n (world-step-count w))
  (define p (world-params w))

  ;; ---- S1–S4: per acting agent -------------------------------------------
  (define-values (w1 events1 infos)
    (for/fold ([w w] [evts '()] [infos (hash)])
              ([tag (in-list (sort (hash-keys actions) symbol<?))])
      (define act (hash-ref actions tag))
      ;; S1+S2: validate & execute
      (define-values (w* ev info move)
        (execute-action w tag act prg))
      ;; S3: broadcast message
      (define msg (action-message act))
      (define a* (struct-copy agent (world-agent w* tag) [last-message (or msg "")]))
      (define w*/chat
        (struct-copy world (world-with-agent w* a*)
                     [chat (if (> (string-length (or msg "")) 0)
                               (hash-update (world-chat w*) step-n
                                            (λ (l) (append l (list (format "~a: ~a"
                                                                           (agent-name a*)
                                                                           msg))))
                                            '())
                               (world-chat w*))]))
      ;; S4: move & eat + passive artifact effects
      (define-values (w** ev2 info2) (move-eat-passive w*/chat tag move))
      (values w**
              (append evts ev ev2)
              (hash-set infos tag (append info info2)))))

  ;; ---- S5: artifact expiry ------------------------------------------------
  (define-values (w2 events2) (artifact-expire-step w1))

  ;; ---- S6: drain ----------------------------------------------------------
  (define w3 (drain w2))

  ;; ---- S7: deaths ---------------------------------------------------------
  (define-values (w4 events3) (process-deaths w3))

  ;; ---- S8: food decay + respawn -------------------------------------------
  (define w5 (if (params-food-mechanism? p)
                 (decay-and-respawn-food w4 prg)
                 w4))

  (define w6 (struct-copy world w5
                          [step-count (add1 step-n)]
                          [food-count-history
                           (cons (food-sum w5) (world-food-count-history w5))]))

  (values w6
          (append events1 events2 events3)
          infos))

;; S4: apply move (no cell sharing — spec §10 change #3), eat, passive effects.
(define (move-eat-passive w tag move)
  (define p (world-params w))
  (define gs (params-grid-size p))
  (define a (world-agent w tag))
  (define wants-move? (and move (not (equal? move (pos 0 0)))))
  (define dest (and wants-move?
                    (wrap-pos (pos-add (agent-pos a) move) gs)))
  (define blocked? (and dest (hash-has-key? (world-pos->agent w) dest)))
  (define new-cell (if (and dest (not blocked?)) dest (agent-pos a)))
  ;; position + trajectory + index (via world-move-agent so the index
  ;; stays in sync — no inline pos->agent patching)
  (define-values (w1 a1)
    (if (and dest (not blocked?))
        (values (world-move-agent w
                                  (struct-copy agent a
                                               [pos new-cell]
                                               [trajectory (append (agent-trajectory a)
                                                                   (list new-cell))]))
                (struct-copy agent a
                             [pos new-cell]
                             [trajectory (append (agent-trajectory a)
                                                 (list new-cell))]))
        (values w a)))
  ;; eat
  (define-values (w2 a2)
    (if (hash-has-key? (world-food w1) new-cell)
        (let ([v (hash-ref (world-food w1) new-cell)])
          (values (struct-copy world w1
                               [food (hash-remove (world-food w1) new-cell)])
                  (struct-copy agent a1 [energy (+ (agent-energy a1) v)])))
        (values w1 a1)))
  (define w3 (world-with-agent w2 a2))
  (define move-events
    (if (and dest (not blocked?))
        (list (evt 'agent-moved
                   (hash 'tag tag 'name (agent-name a)
                         'from (agent-pos a) 'pos new-cell)))
        '()))
  ;; passive artifact effects (cell + inventory), unless inert
  (if (params-inert-artifacts? p)
      (values w3 move-events '())
      (let-values ([(w* evts info) (passive-effects w3 tag new-cell)])
        (values w* (append move-events evts) info))))

(define (passive-effects w tag cell)
  (define step-n (world-step-count w))
  (define a (world-agent w tag))
  (define names
    (append (artifacts-at w cell) (agent-inventory a)))
  (if (null? names)
      (values w '() '())
      (let loop ([w w] [names names] [evts '()] [results '()])
        (if (null? names)
            (values w (reverse evts)
                    (list (cons "Passive interaction result"
                                (string-join (reverse results) "\n"))))
            (let* ([name (car names)]
                   [art (hash-ref (world-artifacts w) name)]
                   [art* (artifact-record-use! art tag step-n)]
                   [result (format "Artifact ~a content: ~a"
                                   name (artifact-payload art*))])
              (loop (world-update-artifact w art*)
                    (cdr names)
                    (cons (evt 'artifact-passive-interaction
                               (hash 'tag tag 'name (agent-name a)
                                     'artifact name 'result result))
                          evts)
                    (cons result results)))))))

;; S6: every agent loses 1 energy and 1 time.
(define (drain w)
  (struct-copy world w
               [agents
                (for/hash ([(tag a) (in-hash (world-agents w))])
                  (values tag
                          (struct-copy agent a
                                       [energy (- (agent-energy a) 1)]
                                       [time-left (- (agent-time-left a) 1)])))]))

;; S7: energy <= 0 or time-left <= 0 → die: drop inventory at death cell,
;; leave food per dead-agent-food mode (area wraps — spec §10 change #11).
(define (process-deaths w)
  (define p (world-params w))
  (define gs (params-grid-size p))
  (define (add-food wc cell v)
    (struct-copy world wc
                 [food (hash-set (world-food wc) cell
                                 (+ v (hash-ref (world-food wc) cell 0)))]))
  (for/fold ([w w] [evts '()])
            ([(tag a) (in-hash (world-agents w))]
             #:when (or (<= (agent-energy a) 0) (<= (agent-time-left a) 0)))
    (define reason (if (<= (agent-time-left a) 0) 'old-age 'hunger))
    (define cell (agent-pos a))
    ;; drop inventory at the death cell
    (define w1
      (for/fold ([w w]) ([name (in-list (agent-inventory a))])
        (define art (hash-ref (world-artifacts w) name))
        (world-update-artifact w
                               (struct-copy artifact art
                                            [location (list 'at cell)]))))
    ;; food left behind
    (define w2
      (case (params-dead-agent-food p)
        [(single)
         (add-food w1 cell (max (params-max-food-value p) (agent-energy a)))]
        [(area)
         (for*/fold ([w w1]) ([dx (in-list '(-1 0 1))] [dy (in-list '(-1 0 1))])
           (add-food w (wrap-pos (pos-add cell (pos dx dy)) gs)
                     (params-max-food-value p)))]
        [else w1]))
    (define w3 (world-remove-agent w2 tag))
    (values w3
            (cons (evt 'agent-died
                       (hash 'tag tag 'name (agent-name a) 'pos cell
                             'reason reason 'energy (agent-energy a)))
                  evts))))
