#lang racket

;;; SPEC §7 — checkpoints. One file, atomic (write tmp + rename). Datum:
;;; #s(checkpoint version step prg-state world agents config); version = 2.
;;; The loader migrates v1 chronological live trajectories to v2 newest-first
;;; storage and rejects other versions. PRG round-trips via
;;; pseudo-random-generator->vector / vector->pseudo-random-generator.

(require racket/file
         racket/path
         "../world/state.rkt"
         "spawn.rkt")

(provide (struct-out checkpoint)
         save-checkpoint
         load-checkpoint)

(struct checkpoint (version step prg-vector world agent-decision-states config) #:prefab)

(define current-checkpoint-version 2)

(define (invalid-checkpoint path format-string . args)
  (error 'load-checkpoint
         "Invalid checkpoint data in ~a: ~a"
         path
         (apply format format-string args)))

(define (validate-checkpoint-payload! ckpt path)
  (define step (checkpoint-step ckpt))
  (define w (checkpoint-world ckpt))
  (define states (checkpoint-agent-decision-states ckpt))
  (define cfg (checkpoint-config ckpt))
  (unless (exact-nonnegative-integer? step)
    (invalid-checkpoint path "step must be an exact nonnegative integer"))
  (unless (world? w)
    (invalid-checkpoint path "world payload is not a world"))
  (unless (= step (world-step-count w))
    (invalid-checkpoint
     path
     "checkpoint step ~a does not match world step ~a"
     step
     (world-step-count w)))
  (unless (hash? states)
    (invalid-checkpoint path "agent decision states payload is not a hash"))
  (for ([(tag state) (in-hash states)])
    (unless (and (symbol? tag)
                 (agent-decision-state? state)
                 (equal? tag (agent-decision-state-tag state)))
      (invalid-checkpoint
       path
       "invalid decision state for key ~a"
       tag)))
  (unless (hash? cfg)
    (invalid-checkpoint path "configuration payload is not a hash"))
  (define invariant-violations (check-invariants w))
  (unless (null? invariant-violations)
    (invalid-checkpoint
     path
     "world invariant violations: ~a"
     (string-join invariant-violations "; "))))

(define (migrate-v1-world w)
  (struct-copy
   world w
   [agents
    (for/hash ([(tag a) (in-hash (world-agents w))])
      (values tag
              (struct-copy agent a
                           [trajectory (reverse (agent-trajectory a))])))]))

(define (save-checkpoint path step prg world agent-decision-states config)
  (define p (if (string? path) (string->path path) path))
  (define dir (path-only p))
  (when dir (make-directory* dir))
  (define tmp-path (string->path (string-append (path->string p) ".tmp")))

  (define prg-vec (pseudo-random-generator->vector prg))
  (define ckpt
    (checkpoint current-checkpoint-version
                step prg-vec world agent-decision-states config))

  (call-with-output-file tmp-path
    (λ (out) (write ckpt out) (newline out))
    #:exists 'truncate)
  (rename-file-or-directory tmp-path p #t))

(define (load-checkpoint path)
  (define p (if (string? path) (string->path path) path))
  (unless (file-exists? p)
    (error 'load-checkpoint "No checkpoint file at ~a" path))

  (define ckpt (call-with-input-file p read))
  (unless (checkpoint? ckpt)
    (error 'load-checkpoint "Invalid checkpoint data format in ~a" path))
  (unless (member (checkpoint-version ckpt) '(1 2))
    (error 'load-checkpoint "Unsupported checkpoint version ~a in ~a"
           (checkpoint-version ckpt) path))

  (define prg (vector->pseudo-random-generator (checkpoint-prg-vector ckpt)))
  (validate-checkpoint-payload! ckpt path)
  (define restored-world
    (if (= (checkpoint-version ckpt) 1)
        (migrate-v1-world (checkpoint-world ckpt))
        (checkpoint-world ckpt)))
  (values (checkpoint-step ckpt)
          prg
          restored-world
          (checkpoint-agent-decision-states ckpt)
          (checkpoint-config ckpt)))
