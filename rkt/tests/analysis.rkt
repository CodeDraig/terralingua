#lang racket

;;; Phase 6 Unit & Integration Tests — Data Analysis & AI Anthropologist Pipeline

(require rackunit
         racket/file
         "../eventlog/writer.rkt"
         "../analysis/log.rkt"
         "../analysis/stats.rkt"
         "../analysis/network.rkt"
         "../analysis/phylogeny.rkt")

(define test-dir (make-temporary-file "tl_phase6_dir_~a" 'directory))
(define log-path (build-path test-dir "events.jsonl"))

;; Create a synthetic event log for testing analysis pipeline
(define logger (open-event-log log-path))

(log-event! logger 0 (hasheq 'seed 42) 'run-started)
(log-event! logger 0 (hasheq 'tag 'being0 'name "Aelion" 'pos (hasheq 'x 1 'y 1)) 'agent-added)
(log-event! logger 0 (hasheq 'tag 'being1 'name "Balthazar" 'pos (hasheq 'x 2 'y 2)) 'agent-added)
(log-event! logger 5 (hasheq 'tag 'being0 'name "Aelion" 'target-tag 'being1 'target-name "Balthazar" 'amount 10) 'gift-energy)
(log-event! logger 10 (hasheq 'tag 'being0 'name "Aelion" 'name "Monolith" 'payload "Ancient Knowledge" 'creator 'being0) 'artifact-added)
(log-event! logger 12 (hasheq 'tag 'being1 'name "Balthazar" 'action "modify_artifact_Monolith" 'artifact "Monolith" 'payload "New Knowledge" 'result "Success") 'artifact-interaction)
(log-event! logger 15 (hasheq 'tag 'being1 'name "Balthazar" 'reason "hunger" 'position (hasheq 'x 2 'y 2) 'energy 0) 'agent-died)

(close-event-log logger)

;; 1. Test Log Reader
(define loaded-events (read-event-log log-path))
(check-equal? (length loaded-events) 7 "read 7 event records")

;; 2. Test Agent & Population Stats
(define agent-stats (analyze-agent-stats loaded-events))
(check-equal? (hash-count agent-stats) 2 "stats for 2 agents")
(define balthazar (hash-ref agent-stats "being1"))
(check-equal? (hash-ref balthazar 'lifespan) 15 "balthazar lifespan = 15")
(check-equal? (hash-ref balthazar 'death-reason) "hunger" "balthazar death reason = hunger")

(define pop-summary (analyze-population-summary loaded-events))
(check-equal? (hash-ref pop-summary 'total-agents) 2)
(check-equal? (hash-ref pop-summary 'dead-agents) 1)

;; 3. Test Interaction Network
(define net (build-interaction-network loaded-events))
(check-equal? (hash-ref net '("being0" . "being1")) 10 "energy transfer weight recorded")

;; 4. Test Artifact Phylogeny
(define phy (build-artifact-phylogeny loaded-events))
(check-true (hash-has-key? phy "Monolith") "Monolith recorded in phylogeny")
(define mono (hash-ref phy "Monolith"))
(check-equal? (length (hash-ref mono 'versions)) 2 "Monolith has 2 versions")

;; Clean up temporary test directory
(delete-directory/files test-dir)
