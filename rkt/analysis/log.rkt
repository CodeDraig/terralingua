#lang racket

;;; Phase 6 — JSONL Event Log Reader

(require json
         racket/file)

(provide read-event-log)

;; Read JSONL event log file into a list of hash records
(define (read-event-log path)
  (define p (if (string? path) (string->path path) path))
  (if (not (file-exists? p))
      '()
      (for/list ([line (in-list (file->lines p))]
                 #:unless (string=? (string-trim line) ""))
        (string->jsexpr line))))
