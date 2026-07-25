#lang racket

;;; Phase 8 Unit & Integration Tests — AI Anthropologist Agent

(require rackunit
         racket/file
         "../eventlog/writer.rkt"
         "../analysis/anthropologist.rkt")

(define test-dir (make-temporary-file "tl_phase8_dir_~a" 'directory))
(define log-path (build-path test-dir "events.jsonl"))

(define logger (open-event-log log-path))
(log-event! logger 0 (hasheq 'seed 42) 'run-started)
(log-event! logger 0 (hasheq 'tag 'being0 'name "Aelion" 'pos (hasheq 'x 1 'y 1)) 'agent-added)
(log-event! logger 5 (hasheq 'tag 'being0 'name "Aelion" 'name "Tablet" 'payload "Laws of Ecology" 'creator 'being0) 'artifact-added)
(close-event-log logger)

(define prompt (run-ai-anthropologist log-path))
(check-true (string? prompt) "anthropologist report string generated")
(check-true (string-contains? prompt "AI Anthropologist") "contains title header")
(check-true (string-contains? prompt "Tablet") "contains artifact name")

(delete-directory/files test-dir)
