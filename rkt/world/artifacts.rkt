#lang racket

;;; SPEC §1 (artifact model), §3 (modify/destroy), §2 S5 (expiry).
;;; Payload cap: 500 tokens ≈ 2000 chars (spec §10 change #12).

(require "state.rkt")

(provide max-payload-chars
         validate-payload
         make-text-artifact
         artifact-record-use!
         artifact-modify
         artifact-destroy
         artifact-expire-step)

(define max-payload-chars 2000) ; 500 tokens × 4 chars

;; Returns #t or an error string (SPEC §3).
(define (validate-payload payload)
  (cond
    [(not (string? payload)) "Payload must be a string"]
    [(> (string-length payload) max-payload-chars)
     (format "Payload exceeds maximum size of ~a chars (got ~a)"
             max-payload-chars (string-length payload))]
    [else #t]))

;; lifespan: integer > 0, or -1 for immortal. creation-time = current step;
;; the artifact does NOT expire on its creation step (spec §10 change #5).
(define (make-text-artifact name payload location creator lifespan step)
  (artifact name 'text payload location creator
            (if (= lifespan -1) +inf.0 lifespan)
            (if (= lifespan -1) +inf.0 lifespan)
            step 0 step '() (hash) #f))

;; Record that tag used the artifact at step (users tracking, SPEC §1).
(define (artifact-record-use! a tag step)
  (struct-copy artifact a
               [users (hash-update (artifact-users a) tag
                                   (λ (steps) (cons step steps))
                                   '())]))

;; modify_artifact_<name> (SPEC §3): new payload, optional new lifespan
;; (integer > 0 or -1). Pushes the past version, bumps version.
;; Returns (values updated-artifact result-string).
(define (artifact-modify a payload lifespan step)
  (define valid (validate-payload payload))
  (cond
    [(string? valid) (values a (format "Failed to modify artifact ~a: ~a"
                                       (artifact-name a) valid))]
    [(and lifespan (not (memv lifespan '(-1)))
          (or (not (integer? lifespan)) (<= lifespan 0)))
     (values a (format "Failed to modify artifact ~a: lifespan must be an integer greater than 0, or -1"
                       (artifact-name a)))]
    [else
     (define past
       (hash 'payload (artifact-payload a)
             'lifespan (artifact-lifespan a)
             'name (artifact-name a)
             'version (artifact-version a)
             'version-creation-time (artifact-version-creation-time a)))
     (define new-lifespan
       (cond [(not lifespan) (artifact-lifespan a)]
             [(= lifespan -1) +inf.0]
             [else lifespan]))
     (values (struct-copy artifact a
                          [payload payload]
                          [lifespan new-lifespan]
                          [remaining-time new-lifespan]
                          [version (add1 (artifact-version a))]
                          [version-creation-time step]
                          [past-versions (cons past (artifact-past-versions a))])
             (format "Artifact ~a updated" (artifact-name a)))]))

;; destroy_artifact_<name> (SPEC §3): expires at the next S5 pass.
(define (artifact-destroy a)
  (struct-copy artifact a [remaining-time 0]))

;; SPEC §2 S5: decrement remaining-time for all artifacts except those
;; created this step; expire those that reach <= 0.
;; Returns (values world events).
(define (artifact-expire-step w)
  (define step-count (world-step-count w))
  (define-values (artifacts* pos-arts events expired-held)
    (for/fold ([arts (hash)] [pos-arts (world-pos->artifacts w)] [evts '()] [held (hash)])
              ([(name a) (in-hash (world-artifacts w))])
      (if (= (artifact-creation-time a) step-count)
          (values (hash-set arts name a) pos-arts evts held) ; just created: no decrement
          (let* ([remaining (if (infinite? (artifact-remaining-time a))
                                +inf.0
                                (sub1 (artifact-remaining-time a)))]
                 [a* (struct-copy artifact a [remaining-time remaining])])
            (if (<= remaining 0)
                (let ([loc (artifact-location a*)])
                  (define pos-arts*
                    (if (eq? (car loc) 'at)
                        (pos->artifacts-remove pos-arts (cadr loc) name)
                        pos-arts))
                  (values arts
                          pos-arts*
                          (cons (evt 'artifact-removed
                                     (hash 'name name
                                           'payload (artifact-payload a*)
                                           'creator (artifact-creator a*)
                                           'location loc
                                           'version (artifact-version a*)
                                           'deletion-time step-count))
                                evts)
                          (if (eq? (car loc) 'held)
                              (hash-update held (cadr loc) (λ (names) (cons name names)) '())
                              held)))
                (values (hash-set arts name a*) pos-arts evts held))))))
  (define agents*
    (if (hash-empty? expired-held)
        (world-agents w)
        (for/hash ([(tag ag) (in-hash (world-agents w))])
          (if (hash-has-key? expired-held tag)
              (let ([to-remove (hash-ref expired-held tag)])
                (values tag (struct-copy agent ag
                                         [inventory (filter (λ (n) (not (member n to-remove)))
                                                            (agent-inventory ag))])))
              (values tag ag)))))
  (values (struct-copy world w [artifacts artifacts*] [pos->artifacts pos-arts] [agents agents*])
          (reverse events)))
