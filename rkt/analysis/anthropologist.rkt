#lang racket

;;; Phase 8 — AI Anthropologist Autonomous LLM Analysis Agent

(require json
         "log.rkt"
         "stats.rkt"
         "network.rkt"
         "phylogeny.rkt"
         "../llm/transport.rkt"
         "../llm/retry.rkt")

(provide generate-anthropologist-prompt
         run-ai-anthropologist)

;; Assembles quantitative simulation statistics into an AI Anthropologist prompt
(define (generate-anthropologist-prompt events)
  (define pop-summary (analyze-population-summary events))
  (define net (build-interaction-network events))
  (define phy (build-artifact-phylogeny events))

  (define net-summary
    (for/list ([(pair weight) (in-hash net)])
      (format "- ~a -> ~a (transfer weight: ~a)" (car pair) (cdr pair) weight)))

  (define art-summary
    (for/list ([(name info) (in-hash phy)])
      (format "- Artifact '~a' by ~a: ~a versions, ~a interactions"
              name (hash-ref info 'creator "unknown")
              (length (hash-ref info 'versions '()))
              (hash-ref info 'interactions 0))))

  (string-append
   "You are the AI Anthropologist analyzing an emergent multi-agent social simulation.\n\n"
   "### Simulation Overview\n"
   (format "- Total Beings: ~a\n" (hash-ref pop-summary 'total-agents 0))
   (format "- Deceased Beings: ~a\n" (hash-ref pop-summary 'dead-agents 0))
   (format "- Average Lifespan: ~a steps\n\n" (hash-ref pop-summary 'avg-lifespan 0))
   "### Interaction Network\n"
   (if (pair? net-summary) (string-join net-summary "\n") "No direct energy/artifact transfers recorded.")
   "\n\n### Artifact Phylogeny & Culture\n"
   (if (pair? art-summary) (string-join art-summary "\n") "No persistent artifacts created.")
   "\n\nPlease write a qualitative anthropological report summarizing behavioral emergence, social cohesion, and cultural transmission in this ecology."))

;; Main entry point to run AI Anthropologist analysis
(define (run-ai-anthropologist log-path #:transport [transport #f])
  (define events (read-event-log log-path))
  (define prompt (generate-anthropologist-prompt events))

  (if (not transport)
      prompt
      (with-handlers ([exn:fail? (λ (e) prompt)])
        (define-values (report _code _usage)
          (call-with-retry
           (λ (attempt)
             (llm-complete transport
                           (list (hash 'role "user" 'content prompt))
                           #:system "You are an expert AI Anthropologist reporting on multi-agent ecologies."))))
        report)))
