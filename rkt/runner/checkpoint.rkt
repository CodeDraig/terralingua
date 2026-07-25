#lang racket

;;; SPEC §7 — checkpoints. One file, atomic (write tmp + rename). Datum:
;;; #s(checkpoint version step prg-state world agents config); version = 1,
;;; loader rejects other versions. PRG round-trips via
;;; pseudo-random-generator->vector / vector->pseudo-random-generator.

(require racket/file
         racket/path
         "../world/state.rkt")

(provide (struct-out checkpoint)
         save-checkpoint
         load-checkpoint)

(struct checkpoint (version step prg-vector world agent-decision-states config) #:prefab)

(define (save-checkpoint path step prg world agent-decision-states config)
  (define p (if (string? path) (string->path path) path))
  (define dir (path-only p))
  (when dir (make-directory* dir))
  (define tmp-path (string->path (string-append (path->string p) ".tmp")))

  (define prg-vec (pseudo-random-generator->vector prg))
  (define ckpt (checkpoint 1 step prg-vec world agent-decision-states config))

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
  (unless (= (checkpoint-version ckpt) 1)
    (error 'load-checkpoint "Unsupported checkpoint version ~a in ~a"
           (checkpoint-version ckpt) path))

  (define prg (vector->pseudo-random-generator (checkpoint-prg-vector ckpt)))
  (values (checkpoint-step ckpt)
          prg
          (checkpoint-world ckpt)
          (checkpoint-agent-decision-states ckpt)
          (checkpoint-config ckpt)))
