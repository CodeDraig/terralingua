#lang racket

;;; SPEC §5 — internal memory. Budget: internal-memory-size tokens,
;;; approximated as (* 4 size) chars (spec §10 change #7), tail-truncated.

(provide cap-internal-memory)

(define (cap-internal-memory text size)
  (cond
    [(or (not text) (not (string? text))) ""]
    [(or (not size) (<= size 0)) ""]
    [else
     (define max-chars (* 4 size))
     (if (<= (string-length text) max-chars)
         text
         (substring text 0 max-chars))]))
