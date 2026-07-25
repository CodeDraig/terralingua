#lang racket

;;; SPEC §3 — action catalog + availability rules; §2 S1–S3 execution.
;;; Plain registry for Phase 1; Phase 1.5 replaces the hand-written schemas
;;; with a `define-action` macro generating handler + schema + validator.

(require "state.rkt"
         "artifacts.rkt"
         "../lang/dsl.rkt")

(provide available-actions
         execute-action)

;; ---------------------------------------------------------------------------
;; Schemas (prompt-facing: description + param descriptions). Hash string ->
;; (hash 'description string 'params (hash string -> string)).

(define (base-schemas p)
  (define reproduce-desc
    (format "Asexually generate an offspring. It costs ~a energy."
            (params-reproduction-cost p)))
  (define reproduce-params
    (if (params-food-mechanism? p)
        (hash "energy" (format "Integer amount of **additional** energy the parent gifts the child (0 up to <parent_current_energy - ~a>)"
                               (params-reproduction-cost p))
              "name" "Name of the offspring (use **unique** names)")
        (hash "name" "Name of the offspring (use **unique** names)")))
  (hash
   "move" (hash 'description
                "Move of one cell in the specified direction, or stay in the current position"
                'params (hash "direction" "One among [right, left, up, down, stay]."))
   "give" (hash 'description "Transfer some of your energy to another nearby being."
                'params (hash "target" "Name of a being in your field of view to give energy to."
                              "amount" "Integer amount of energy to transfer (1 up to your current energy)."))
   "take" (hash 'description "Steal energy from another nearby being."
                'params (hash "target" "Name of a being in your field of view to steal energy from."
                              "amount" "Integer amount of energy to steal (1 up to target's current energy)."))
   "reproduce" (hash 'description reproduce-desc 'params reproduce-params)
   "create_artifact"
   (hash 'description "Creates a new artifact at the being's location."
         'params (hash "name" "The name of the artifact (use **unique** names)"
                       "type" "Must be exactly 'text'. A text artifact is a persistent physical object that beings can use to create, preserve, and transmit culture."
                       "payload" "Text stored in the artifact and readable by beings that share its cell or possess it."
                       "lifespan" "How many time steps the artifact will last (in number of steps, integer > 0. If -1 the artifact will never disappear)"))
   "pickup_artifact" (hash 'description "Picks up the artifact and puts it in the being's inventory"
                           'params (hash "name" "Name of the artifact to pick up"))
   "drop_artifact" (hash 'description "Drops artifact from the inventory at the being's current position"
                         'params (hash "name" "Name of the artifact to drop"))
   "give_artifact" (hash 'description "Gives an artifact from the inventory to a nearby being"
                         'params (hash "artifact_name" "The name of the artifact"
                                       "target_agent" "Name of a being in your field of view to give the artifact to."))
   "set_color" (hash 'description "Change how you appear to other beings by choosing your color"
                     'params (hash "color" "The color you want to show to other beings"))))

;; Local merge (avoids the racket/hash dependency): entries of b win.
(define (hash-merge a b)
  (for/fold ([h a]) ([(k v) (in-hash b)]) (hash-set h k v)))

(define (artifact-schemas art-name)
  (hash (string-append "destroy_artifact_" art-name)
        (hash 'description (format "Destroys ~a artifact" art-name)
              'params (hash))
        (string-append "modify_artifact_" art-name)
        (hash 'description (format "Modifies the content of ~a artifact" art-name)
              'params (hash "payload" "New content of the artifact"
                            "lifespan" "New lifespan of the artifact"))))

;; SPEC §3 availability rules.
(define (available-actions w tag)
  (define p (world-params w))
  (define a (world-agent w tag))
  (define schemas (base-schemas p))
  (define cell (agent-pos a))
  (define others-nearby
    (filter (λ (t) (not (equal? t tag))) (nearby-tags w cell)))
  (define at-cell (artifacts-at w cell))
  (define held (agent-inventory a))
  (define avail (hash "move" (hash-ref schemas "move")))
  (define (add a* k) (hash-set a* k (hash-ref schemas k)))
  (when (params-use-colors? p)
    (set! avail (add avail "set_color")))
  (when (and (pair? others-nearby) (params-food-mechanism? p))
    (set! avail (add (add avail "give") "take")))
  (when (and (params-artifact-creation? p)
             (>= (agent-energy a) (params-artifact-creation-cost p)))
    (define schema (hash-ref schemas "create_artifact"))
    (when (> (params-artifact-creation-cost p) 0)
      (set! schema
            (hash-set schema 'description
                      (string-append (hash-ref schema 'description)
                                     (format " It costs ~a energy."
                                             (params-artifact-creation-cost p))))))
    (set! avail (hash-set avail "create_artifact" schema)))
  (unless (params-inert-artifacts? p)
    ;; artifact-defined actions for artifacts at this cell
    (for ([art-name (in-list at-cell)])
      (set! avail (hash-merge avail (artifact-schemas art-name))))
    (when (params-use-inventory? p)
      (when (pair? at-cell)
        (set! avail (add avail "pickup_artifact")))
      (when (pair? held)
        (set! avail (add avail "drop_artifact"))
        (for ([art-name (in-list held)])
          (set! avail (hash-merge avail (artifact-schemas art-name))))
        (when (pair? others-nearby)
          (set! avail (add avail "give_artifact"))))))
  (when (and (params-reproduction-allowed? p)
             (>= (agent-energy a) (params-reproduction-cost p)))
    (set! avail (add avail "reproduce")))
  (for ([c-entry (in-list (get-custom-actions))])
    (define c-name (custom-action-entry-name c-entry))
    (define c-avail-proc (custom-action-entry-availability-proc c-entry))
    (when (c-avail-proc w tag)
      (set! avail (hash-set avail c-name
                            (hash 'description (custom-action-entry-description c-entry)
                                  'params (custom-action-entry-params c-entry))))))
  avail)

;; ---------------------------------------------------------------------------
;; Helpers

(define move-deltas
  (hash "stay" (pos 0 0) "up" (pos -1 0) "down" (pos 1 0)
        "left" (pos 0 -1) "right" (pos 0 1)))

(define (coerce-number v [default 0])
  (cond
    [(real? v) v]
    [(string? v) (or (string->number v) default)]
    [else default]))

(define (coerce-integer v [default #f])
  (cond
    [(integer? v) (inexact->exact v)]
    [(string? v) (let ([n (string->number v)])
                   (and n (integer? n) (inexact->exact n)))]
    [else default]))

;; Random free 8-neighbor cell (wraps — spec §10 change #11), or #f.
;; Only rejects cells with other agents; allows food and artifacts
;; (the child consumes any food immediately on its first step).
(define (random-free-neighbor w center prg)
  (define gs (params-grid-size (world-params w)))
  (define cells
    (for*/list ([dx (in-list '(-1 0 1))] [dy (in-list '(-1 0 1))]
                #:unless (and (= dx 0) (= dy 0)))
      (let ([p (wrap-pos (pos-add center (pos dx dy)) gs)])
        (and (not (hash-has-key? (world-pos->agent w) p)) p))))
  (define free-cells (filter values cells))
  (and (pair? free-cells)
       (list-ref free-cells (random (length free-cells) prg))))

(define (dedup-artifact-name w name)
  (let loop ([n name])
    (if (hash-has-key? (world-artifacts w) n)
        (loop (string-append n "_1"))
        n)))

;; ---------------------------------------------------------------------------
;; Handlers: (w tag params prg) -> (values world events info move-delta)
;; info: (listof (cons string string)); move-delta: pos or #f.

(define (handle-move w tag params prg)
  (define delta (hash-ref move-deltas (hash-ref params "direction" "stay") (pos 0 0)))
  (values w '() '() delta))

(define (handle-energy-transfer w tag act-name params prg)
  (define p (world-params w))
  (define a (world-agent w tag))
  (define target-name (hash-ref params "target" ""))
  (define target-tag (hash-ref (world-name->tag w) target-name #f))
  (define nearby?
    (and target-tag
         (<= (chebyshev (agent-pos a) (agent-pos (world-agent w target-tag))
                        (params-grid-size p))
             (params-vision-radius p))))
  (if (not nearby?)
      (values w '()
              (list (cons "Action outcome"
                          (format "Cannot ~a energy to ~a as not nearby"
                                  act-name target-name)))
              #f)
      (let* ([t (world-agent w target-tag)]
             [raw-amount (hash-ref params "amount" 0)]
             [coerced-amount (coerce-number raw-amount)]
             [amount (max coerced-amount 0)]
             [bad-amount? (not (real? raw-amount))]
             [give? (equal? act-name "give")]
             [xfer (if give?
                       (min amount (agent-energy a))
                       (min amount (agent-energy t)))]
             [a* (struct-copy agent a [energy ((if give? - +) (agent-energy a) xfer)])]
             [t* (struct-copy agent t [energy ((if give? + -) (agent-energy t) xfer)])]
             [w* (world-with-agent (world-with-agent w a*) t*)]
             [info
              (if bad-amount?
                  (list (cons "Action outcome"
                              (format "amount=~a is not a number; treating as 0" raw-amount)))
                  '())])
        (values w*
                (list (evt (if give? 'gift-energy 'take-energy)
                           (hash 'tag tag 'name (agent-name a)
                                 'target-tag target-tag 'target-name (agent-name t)
                                 'amount xfer
                                 'final-energy (agent-energy a*)
                                 'target-final-energy (agent-energy t*))))
                info
                #f))))

(define (handle-set-color w tag params prg)
  (define a (world-agent w tag))
  (define color (hash-ref params "color" ""))
  (values (world-with-agent w (struct-copy agent a [color color]))
          (list (evt 'set-color (hash 'tag tag 'name (agent-name a) 'color color)))
          (list (cons "Color selection" (format "Success. Current color ~a" color)))
          #f))

(define (handle-reproduce w tag params prg)
  (define p (world-params w))
  (define a (world-agent w tag))
  (define cost (params-reproduction-cost p))
  (define a1 (struct-copy agent a [energy (- (agent-energy a) cost)]))
  (define w1 (world-with-agent w a1))
  (define spawn-cell (random-free-neighbor w1 (agent-pos a) prg))
  (define (fail reason event-reason)
    (values w1
            (list (evt 'agent-reproduced
                       (hash 'tag tag 'name (agent-name a)
                             'successful #f 'fail-reason event-reason)))
            (list (cons "reproduction"
                        (format "failed: ~a" reason)))
            #f))
  (cond
    [(< (agent-energy a1) 0) (fail "Not enough energy" 'no-energy)]
    [(not spawn-cell) (fail "No free space around" 'no-space)]
    [else
     (define requested (hash-ref params "name" "child"))
     (define child-name (unique-name w1 requested))
     (define idx (length (agent-spawn a)))
     (define child-tag (string->symbol (format "~a_~a" tag idx)))
     (define gift
       (min (max (coerce-number (hash-ref params "energy" 0)) 0)
            (agent-energy a1)))
     (define child
       (agent child-tag child-name spawn-cell
              (+ (params-init-energy p) gift)
              (params-lifespan p) #f '() (list spawn-cell) '() ""))
     (define a2 (struct-copy agent a1
                             [energy (- (agent-energy a1) gift)]
                             [spawn (cons child-tag (agent-spawn a1))]))
     (define w2 (world-add-agent (world-with-agent w1 a2) child))
     (define renamed? (not (equal? child-name requested)))
     (values w2
             (list (evt 'agent-added
                        (hash 'tag child-tag 'name child-name
                              'pos spawn-cell 'agent-type 'text))
                   (evt 'agent-reproduced
                        (hash 'tag tag 'name (agent-name a)
                              'successful #t
                              'child-name child-name 'child-tag child-tag
                              'energy-gifted gift
                              'final-energy (agent-energy a2)
                              'child-energy (agent-energy child))))
             (list (cons "reproduction"
                         (format "successful: ~a(~a)~a"
                                 child-name child-tag
                                 (if renamed?
                                     (format " (renamed; ~a was taken)" requested)
                                     ""))))
             #f)]))

(define (handle-create-artifact w tag params prg)
  (define p (world-params w))
  (define a (world-agent w tag))
  (define cost (params-artifact-creation-cost p))
  (define art-type (hash-ref params "type" "text"))
  (define name (dedup-artifact-name w (hash-ref params "name" (format "~a_artifact" tag))))
  (define payload (hash-ref params "payload" ""))
  (define lifespan (or (coerce-integer (hash-ref params "lifespan" -1)) -1))
  (define valid (validate-payload payload))
  (cond
    [(< (agent-energy a) cost)
     (values w '()
             (list (cons "Artifact creation status"
                         (format "Failed. Agent does not have enough energy. Required: ~a" cost)))
             #f)]
    [(not (equal? art-type "text"))
     (values w '()
             (list (cons "Artifact creation status"
                         (format "Artifact type: ~a is not a valid type. Only artifact valid types are: (text)" art-type)))
             #f)]
    [(string? valid)
     (values w '() (list (cons "Artifact creation status" valid)) #f)]
    [else
     (define art (make-text-artifact name payload (list 'at (agent-pos a))
                                     tag lifespan (world-step-count w)))
     (define w* (world-put-artifact
                 (world-with-agent w (struct-copy agent a [energy (- (agent-energy a) cost)]))
                 art))
     (values w*
              (list (evt 'artifact-added
                         (hash 'name name 'payload payload
                               'pos (agent-pos a) 'creator tag
                               'lifespan (artifact-lifespan art))))
             (list (cons "Artifact creation status"
                         (format "Created artifact ~a of type text at position ~a."
                                 name (agent-pos a))))
             #f)]))

(define (handle-pickup w tag params prg)
  (define a (world-agent w tag))
  (define name (hash-ref params "name" ""))
  (define art (hash-ref (world-artifacts w) name #f))
  (define (fail msg)
    (values w '() (list (cons "Artifact pickup status" msg)) #f))
  (cond
    [(not art) (fail (format "Failed. Artifact ~a does not exist" name))]
    [(not (equal? (artifact-location art) (list 'at (agent-pos a))))
     (fail (format "Failed. No artifact with name ~a at current position" name))]
    [else
     (define art* (struct-copy artifact
                               (artifact-record-use! art tag (world-step-count w))
                               [location (list 'held tag)]))
     (define a* (struct-copy agent a [inventory (cons name (agent-inventory a))]))
     (values (world-update-artifact (world-with-agent w a*) art*)
              (list (evt 'artifact-pickup
                         (hash 'tag tag 'name (agent-name a)
                               'artifact name 'status "Success"
                               'pos (agent-pos a))))
             (list (cons "Artifact pickup status" "Success"))
             #f)]))

(define (handle-drop w tag params prg)
  (define a (world-agent w tag))
  (define name (hash-ref params "name" ""))
  (define art (hash-ref (world-artifacts w) name #f))
  (define (fail msg)
    (values w '() (list (cons "Artifact drop status" msg)) #f))
  (cond
    [(not art) (fail (format "Failed. Artifact ~a does not exist" name))]
    [(not (member name (agent-inventory a)))
     (fail (format "Failed. No artifact with name ~a in inventory" name))]
    [else
     (define art* (struct-copy artifact
                               (artifact-record-use! art tag (world-step-count w))
                               [location (list 'at (agent-pos a))]))
     (define a* (struct-copy agent a
                             [inventory (remove name (agent-inventory a))]))
     (values (world-update-artifact (world-with-agent w a*) art*)
              (list (evt 'artifact-drop
                         (hash 'tag tag 'name (agent-name a)
                               'artifact name 'status "Success"
                               'pos (agent-pos a))))
             (list (cons "Artifact drop status" "Success"))
             #f)]))

(define (handle-give-artifact w tag params prg)
  (define p (world-params w))
  (define a (world-agent w tag))
  (define art-name (hash-ref params "artifact_name" ""))
  (define target-name (hash-ref params "target_agent" ""))
  (define target-tag (hash-ref (world-name->tag w) target-name #f))
  (define (fail msg)
    (values w '()
            (list (cons "Artifact give status" msg))
            #f))
  (cond
    [(not target-tag) (fail (format "Failed. No being with name ~a" target-name))]
    [(> (chebyshev (agent-pos a)
                   (agent-pos (world-agent w target-tag))
                   (params-grid-size p))
        (params-vision-radius p))
     (fail (format "Failed. Target being ~a not nearby." target-name))]
    [(not (member art-name (agent-inventory a)))
     (fail (format "Failed. No artifact with name ~a in inventory" art-name))]
    [else
     (define step-n (world-step-count w))
     (define art (hash-ref (world-artifacts w) art-name))
     (define art* (struct-copy artifact
                               (artifact-record-use!
                                (artifact-record-use! art tag step-n)
                                target-tag step-n)
                               [location (list 'held target-tag)]))
     (define a* (struct-copy agent a
                             [inventory (remove art-name (agent-inventory a))]))
     (define t (world-agent w target-tag))
     (define t* (struct-copy agent t
                             [inventory (cons art-name (agent-inventory t))]))
     (values (world-update-artifact (world-with-agent (world-with-agent w a*) t*) art*)
             (list (evt 'give-artifact
                        (hash 'tag tag 'name (agent-name a)
                              'target-tag target-tag 'target-name target-name
                              'artifact art-name 'status "Success")))
             (list (cons "Artifact give status" "Success"))
             #f)]))

(define (handle-artifact-defined w tag act-name params prg)
  (define a (world-agent w tag))
  (define modify? (string-prefix? act-name "modify_artifact_"))
  (define art-name
    (cond
      [modify? (substring act-name (string-length "modify_artifact_"))]
      [(string-prefix? act-name "destroy_artifact_")
       (substring act-name (string-length "destroy_artifact_"))]
      [else #f]))
  (define art (and art-name (hash-ref (world-artifacts w) art-name #f)))
  (define interactable?
    (and art
         (or (equal? (artifact-location art) (list 'at (agent-pos a)))
             (equal? (artifact-location art) (list 'held tag)))))
  (define (fail msg)
    (values w '() (list (cons "Artifact interaction result" msg)) #f))
  (cond
    [(not art-name)
     (fail (format "No artifact with corresponding action ~a found" act-name))]
    [(not interactable?)
     (fail (format "Artifact ~a is not at your cell or in your inventory" art-name))]
    [modify?
     (define step-n (world-step-count w))
     (define payload (hash-ref params "payload" ""))
     (define lifespan (coerce-integer (hash-ref params "lifespan" "") #f))
     (define-values (art* result)
       (artifact-modify (artifact-record-use! art tag step-n) payload lifespan step-n))
     (values (world-update-artifact w art*)
             (list (evt 'artifact-interaction
                        (hash 'tag tag 'name (agent-name a)
                              'action act-name 'artifact art-name
                              'payload (artifact-payload art*)
                              'result result)))
             (list (cons "Artifact interaction result" result))
             #f)]
    [else ; destroy
     (define art* (artifact-destroy (artifact-record-use! art tag (world-step-count w))))
     (define result (format "Artifact ~a destroyed" art-name))
     (values (world-update-artifact w art*)
             (list (evt 'artifact-interaction
                        (hash 'tag tag 'name (agent-name a)
                              'action act-name 'artifact art-name
                              'result result)))
             (list (cons "Artifact interaction result" result))
             #f)]))

;; ---------------------------------------------------------------------------
;; SPEC §2 S1: validate against available-actions (exact param keys); on
;; mismatch coerce to move/stay with an info note (spec §10 change #4).

(define (execute-action w tag act prg)
  (define avail (available-actions w tag))
  (define name (action-name act))
  (define params (action-params act))
  (define valid?
    (and (hash-has-key? avail name)
         (equal? (list->set (hash-keys params))
                 (list->set (hash-keys
                             (hash-ref (hash-ref avail name) 'params))))))
  (if (not valid?)
      (values w '()
              (list (cons "Action outcome"
                          (format "Unknown action or incorrect params: ~a" name)))
              (pos 0 0))
      (let ([custom-entry (hash-ref (custom-actions-registry) name #f)])
        (cond
          [custom-entry
           ((custom-action-entry-handler-proc custom-entry) w tag params prg)]
        [(equal? name "move") (handle-move w tag params prg)]
        [(or (equal? name "give") (equal? name "take"))
         (handle-energy-transfer w tag name params prg)]
        [(equal? name "set_color") (handle-set-color w tag params prg)]
        [(equal? name "reproduce") (handle-reproduce w tag params prg)]
        [(equal? name "create_artifact") (handle-create-artifact w tag params prg)]
        [(equal? name "pickup_artifact") (handle-pickup w tag params prg)]
        [(equal? name "drop_artifact") (handle-drop w tag params prg)]
        [(equal? name "give_artifact") (handle-give-artifact w tag params prg)]
        [else (handle-artifact-defined w tag name params prg)]))))
