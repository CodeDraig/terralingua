#lang racket

;;; SPEC §1 — World state.
;;; Rule #1: STATE IS DATA. Everything here is #:prefab and free of ports,
;;; procedures, and the PRG (the PRG is threaded by callers and checkpointed
;;; separately — SPEC §7). All updates are functional.

;; Note: make-world builds an empty world (no food). Callers that want
;; a fully-initialized world must call world-seed-food separately —
;; cannot fold the call into make-world without a cyclic import.

(provide (struct-out pos)
         (struct-out params)
         (struct-out agent)
         (struct-out artifact)
         (struct-out world)
         (struct-out event)
         (struct-out action)
         evt
         make-world
         wrap-pos
         pos-add
         chebyshev
         world-agent
         world-add-agent
         world-with-agent
         world-move-agent
         world-remove-agent
         world-put-artifact
         world-update-artifact
         world-remove-artifact
         pos->artifacts-add
         pos->artifacts-remove
         artifacts-at
         nearby-tags
         unique-name
         check-invariants)

;; ---------------------------------------------------------------------------
;; A position on the toroidal grid.
(struct pos (x y) #:prefab)

;; Static experiment parameters (SPEC §8 config surface, env half).
(struct params
  (grid-size
   vision-radius
   lifespan
   init-energy
   ;; food
   init-food max-food-value food-decay-rate food-decay-amount food-spawn-rate
   food-zones static-food? food-mechanism?
   dead-agent-food ; 'single | 'area | 'none
   ;; reproduction
   reproduction-allowed? reproduction-cost
   ;; artifacts
   artifact-creation? artifact-creation-cost inert-artifacts?
   ;; agents
   use-inventory? use-internal-memory? use-colors?)
  #:prefab)

;; World-visible agent state (SPEC §1). Decision state (history, internal
;; memory, prompt) lives in the agent layer — SPEC §5 — NOT here.
(struct agent
  (tag        ; symbol, stable: 'being0, ...
   name       ; string, unique across living and dead (I5)
   pos
   energy
   time-left
   color      ; #f | string
   inventory  ; (listof string) — artifact names, set semantics
   trajectory ; (listof pos), append-only
   spawn      ; (listof symbol) — child tags
   last-message) ; string — this step's broadcast, "" if none
  #:prefab)

;; SPEC §1. Location is a field ON the artifact: (list 'at p) | (list 'held tag).
;; Exactly one location, by construction (I2).
(struct artifact
  (name type payload ; name string; type: 'text (v1); payload string
   location          ; (list 'at pos) | (list 'held tag)
   creator           ; tag
   lifespan          ; number; +inf.0 = immortal
   remaining-time
   creation-time
   version
   version-creation-time
   past-versions     ; (listof hash)
   users             ; (hash tag -> (listof step))
   deletion-time)    ; #f | step
  #:prefab)

(struct world
  (params
   step-count
   agents      ; (hash tag -> agent)
   artifacts   ; (hash name -> artifact)
   food        ; (hash pos -> nonnegative-number)
   pos->agent  ; (hash pos -> tag)  — derived index, kept in sync (I6)
   name->tag   ; (hash name -> tag) — derived index (I6)
   pos->artifacts ; (hash pos -> (listof name)) — derived index, kept in sync (I6)
   food-count-history ; (listof number)
   chat        ; (hash step -> (listof string))
   food-centers       ; (listof pos) — Gaussian zone centers; '() = uniform
   empty-food         ; (listof pos) — static-food respawn pool
   dead-names         ; (listof string) — names of the dead (I5)
   archived-trajectories) ; (hash tag -> (listof pos)) — of the dead
  #:prefab)

;; An event (SPEC §9): type symbol + data (hash symbol -> datum).
(struct event (type data) #:prefab)
(define (evt type [data (hash)]) (event type data))

;; An agent's chosen action (SPEC §2).
(struct action (name params message) #:prefab) ; name string; params (hash string -> datum)

;; Build an empty world. Note: by default this is empty (no food).
;; Callers that want a fully-initialized world must call world-seed-food
;; explicitly after creating the world. (Cannot fold into make-world
;; because that would create a cyclic import with food.rkt.)
(define (make-world p)
  (world p 0 (hash) (hash) (hash) (hash) (hash) (hash) '() (hash) '() '() '() (hash)))

;; Helper: an O(1) per-cell predicate for "is this pos occupied?".
;; Used by callers that need to avoid the full default-free-cell?
;; when they only care about agent collisions (e.g., respawn).
(define (pos-occupied-by-agent? w p)
  (hash-has-key? (world-pos->agent w) p))

(define (wrap-pos p grid-size)
  (pos (modulo (pos-x p) grid-size)
       (modulo (pos-y p) grid-size)))

(define (pos-add p dp)
  (pos (+ (pos-x p) (pos-x dp))
       (+ (pos-y p) (pos-y dp))))

;; Wrapped Chebyshev distance between two cells (SPEC §2: interaction radii wrap).
(define (chebyshev a b grid-size)
  (define (axis d) (min (abs d) (- grid-size (abs d))))
  (max (axis (- (pos-x a) (pos-x b)))
       (axis (- (pos-y a) (pos-y b)))))

;; ---------------------------------------------------------------------------
;; Pure world transitions

(define (world-agent w tag)
  (hash-ref (world-agents w) tag
            (λ () (error 'world-agent "no agent with tag ~a" tag))))

(define (world-add-agent w a)
  (define tag (agent-tag a))
  (define pos (agent-pos a))
  (define name (agent-name a))
  (when (hash-has-key? (world-agents w) tag)
    (error 'world-add-agent "tag ~a already present" tag))
  (when (hash-has-key? (world-pos->agent w) pos)
    (error 'world-add-agent
           "position ~a already occupied by ~a"
           pos (hash-ref (world-pos->agent w) pos)))
  (when (hash-has-key? (world-name->tag w) name)
    (error 'world-add-agent
           "name ~a already in use by ~a"
           name (hash-ref (world-name->tag w) name)))
  (struct-copy world w
               [agents (hash-set (world-agents w) tag a)]
               [pos->agent (hash-set (world-pos->agent w) pos tag)]
               [name->tag (hash-set (world-name->tag w) name tag)]))

;; Replace an existing agent (same tag, same position). For position
;; changes use world-move-agent to keep the pos->agent index in sync.
(define (world-with-agent w a)
  (define tag (agent-tag a))
  (define old (hash-ref (world-agents w) tag
                        (λ () (error 'world-with-agent "no agent with tag ~a" tag))))
  (unless (equal? (agent-pos old) (agent-pos a))
    (error 'world-with-agent
           "position change for tag ~a; use world-move-agent instead"
           tag))
  (struct-copy world w
               [agents (hash-set (world-agents w) tag a)]))

;; Update an agent's position, keeping the pos->agent index in sync.
;; The agent must already be in the world (call world-add-agent first
;; for new agents). Both old and new positions wrap on the grid.
(define (world-move-agent w a)
  (define tag (agent-tag a))
  (define old-pos (agent-pos (hash-ref (world-agents w) tag
                                      (λ () (error 'world-move-agent "no agent with tag ~a" tag)))))
  (define new-pos (agent-pos a))
  (cond
    [(equal? old-pos new-pos)
     (struct-copy world w [agents (hash-set (world-agents w) tag a)])]
    [else
     (struct-copy world w
                  [agents (hash-set (world-agents w) tag a)]
                  [pos->agent (hash-set (hash-remove (world-pos->agent w) old-pos)
                                        new-pos tag)])]))

;; Removes the agent, archives trajectory, records the name as dead (I5).
(define (world-remove-agent w tag)
  (define a (world-agent w tag))
  (struct-copy world w
               [agents (hash-remove (world-agents w) tag)]
               [pos->agent (hash-remove (world-pos->agent w) (agent-pos a))]
               [name->tag (hash-remove (world-name->tag w) (agent-name a))]
               [dead-names (cons (agent-name a) (world-dead-names w))]
               [archived-trajectories
                (hash-set (world-archived-trajectories w) tag
                          (agent-trajectory a))]))

;; Insert a new artifact (name must not already exist).
(define (world-put-artifact w a)
  (define name (artifact-name a))
  (define loc (artifact-location a))
  (when (hash-has-key? (world-artifacts w) name)
    (error 'world-put-artifact "artifact name ~a already exists" name))
  (struct-copy world w
               [artifacts (hash-set (world-artifacts w) name a)]
               [pos->artifacts
                (if (eq? (car loc) 'at)
                    (pos->artifacts-add (world-pos->artifacts w) (cadr loc) name)
                    (world-pos->artifacts w))]))

;; Replace an existing artifact (same name). If the location changed, the
;; pos->artifacts index is updated to match.
(define (world-update-artifact w a)
  (define name (artifact-name a))
  (define old (hash-ref (world-artifacts w) name #f))
  (when (not old)
    (error 'world-update-artifact "no existing artifact with name ~a" name))
  (define w* (struct-copy world w
                          [artifacts (hash-set (world-artifacts w) name a)]))
  (cond
    [(equal? (artifact-location old) (artifact-location a)) w*]
    [else
     (define old-loc (artifact-location old))
     (define new-loc (artifact-location a))
     (define pos-arts (world-pos->artifacts w*))
     (define pos-arts*
       (cond
         [(and (eq? (car old-loc) 'at) (eq? (car new-loc) 'at))
          (pos->artifacts-add (pos->artifacts-remove pos-arts (cadr old-loc) name)
                              (cadr new-loc) name)]
         [(eq? (car old-loc) 'at)
          (pos->artifacts-remove pos-arts (cadr old-loc) name)]
         [(eq? (car new-loc) 'at)
          (pos->artifacts-add pos-arts (cadr new-loc) name)]
         [else pos-arts]))
     (struct-copy world w* [pos->artifacts pos-arts*])]))

(define (world-remove-artifact w name)
  (define a (hash-ref (world-artifacts w) name #f))
  (when (not a)
    (error 'world-remove-artifact "no artifact with name ~a" name))
  (define w* (struct-copy world w
                          [artifacts (hash-remove (world-artifacts w) name)]))
  (if (eq? (car (artifact-location a)) 'at)
      (struct-copy world w*
                   [pos->artifacts
                    (pos->artifacts-remove (world-pos->artifacts w*)
                                           (cadr (artifact-location a))
                                           name)])
      w*))

;; Helpers for the pos->artifacts derived index (I6: keep in sync).
;; Take a hash directly (not a world) so callers that work on a
;; transient copy don't have to construct a synthetic world struct.
;; Empty cells are dropped from the hash to bound memory.
(define (pos->artifacts-add h pos name)
  (hash-update h pos (λ (ns) (cons name ns)) '()))

(define (pos->artifacts-remove h pos name)
  (define updated
    (hash-update h pos (λ (ns) (remove name ns)) '()))
  (if (null? (hash-ref updated pos))
      (hash-remove updated pos)
      updated))

;; Names of artifacts located at cell p (I2: derived from location fields).
;; O(1) via the pos->artifacts index.
(define (artifacts-at w p)
  (hash-ref (world-pos->artifacts w) p '()))

;; Tags of other agents within vision radius of a cell (wraps — SPEC §2).
(define (nearby-tags w p)
  (define r (params-vision-radius (world-params w)))
  (define gs (params-grid-size (world-params w)))
  (for/list ([(cell tag) (in-hash (world-pos->agent w))]
             #:when (<= (chebyshev p cell gs) r))
    tag))

;; Deduplicate a display name against living AND dead names (I5), Python-style:
;; append "_1" until free.
(define (unique-name w name)
  (define taken?
    (let ([living (list->set (hash-keys (world-name->tag w)))])
      (λ (n) (or (set-member? living n) (member n (world-dead-names w))))))
  (let loop ([n name])
    (if (taken? n) (loop (string-append n "_1")) n)))

;; ---------------------------------------------------------------------------
;; SPEC §6 invariant checker — used by tests and the runner's debug mode.
;; Returns a list of violation strings; '() means the world is consistent.

(define (check-invariants w)
  (define violations '())
  (define (note! fmt . args)
    (set! violations (cons (apply format fmt args) violations)))
  (define agents (world-agents w))
  ;; I1/I6: every agent's position maps back to its tag; name maps to tag.
  (for ([(tag a) (in-hash agents)])
    (unless (equal? (hash-ref (world-pos->agent w) (agent-pos a) #f) tag)
      (note! "I1/I6: agent ~a at ~a missing from pos->agent" tag (agent-pos a)))
    (unless (equal? (hash-ref (world-name->tag w) (agent-name a) #f) tag)
      (note! "I5/I6: name ~a of ~a missing from name->tag" (agent-name a) tag))
    ;; I2: inventory artifacts exist and point back at this agent.
    (for ([art-name (in-list (agent-inventory a))])
      (define art (hash-ref (world-artifacts w) art-name #f))
      (cond
        [(not art) (note! "I2: ~a holds nonexistent artifact ~a" tag art-name)]
        [(not (equal? (artifact-location art) (list 'held tag)))
         (note! "I2: artifact ~a held by ~a but location is ~a"
                art-name tag (artifact-location art))])))
  ;; I6: indices have no stale entries.
  (unless (= (hash-count (world-pos->agent w)) (hash-count agents))
    (note! "I6: pos->agent has ~a entries for ~a agents"
           (hash-count (world-pos->agent w)) (hash-count agents)))
  (unless (= (hash-count (world-name->tag w)) (hash-count agents))
    (note! "I6: name->tag has ~a entries for ~a agents"
           (hash-count (world-name->tag w)) (hash-count agents)))
  ;; I2: map artifacts are not simultaneously held.
  (for ([(name a) (in-hash (world-artifacts w))])
    (when (and (eq? (car (artifact-location a)) 'held)
               (not (hash-has-key? agents (cadr (artifact-location a)))))
      (note! "I2: artifact ~a held by dead/missing agent ~a"
             name (cadr (artifact-location a)))))
  ;; I6: pos->artifacts index is consistent with artifact locations.
  ;; (Order-independent: hash-iteration order is not deterministic.)
  (for ([(name a) (in-hash (world-artifacts w))])
    (define loc (artifact-location a))
    (cond
      [(eq? (car loc) 'at)
       (define p (cadr loc))
       (define names (hash-ref (world-pos->artifacts w) p '()))
       (unless (member name names)
         (note! "I6: artifact ~a at ~a missing from pos->artifacts" name p))]
      [else
       (for ([(p names) (in-hash (world-pos->artifacts w))])
         (when (member name names)
           (note! "I6: held artifact ~a indexed at pos->artifacts[~a]" name p)))]))
  (for ([(p names) (in-hash (world-pos->artifacts w))])
    (for ([name (in-list names)])
      (define a (hash-ref (world-artifacts w) name #f))
      (cond
        [(not a)
         (note! "I6: pos->artifacts[~a] names missing artifact ~a" p name)]
        [(not (equal? (artifact-location a) (list 'at p)))
         (note! "I6: pos->artifacts[~a] has ~a but artifact location is ~a"
                p name (artifact-location a))])))
  (reverse violations))
