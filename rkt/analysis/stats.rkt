#lang racket

;;; Phase 6 — Agent & Population Behavioral Statistics

(require racket/set)

(provide analyze-agent-stats
         analyze-population-summary)

;; Analyzes per-agent spawn time, death time, lifespan, and death reason from event records
(define (analyze-agent-stats events)
  (define agent-data (make-hash))

  (for ([rec (in-list events)])
    (define type (hash-ref rec 'type ""))
    (define ts (hash-ref rec 'ts 0))
    (cond
      [(equal? type "agent-added")
       (define tag (format "~a" (hash-ref rec 'tag "")))
       (define name (format "~a" (hash-ref rec 'name "")))
       (hash-set! agent-data tag
                  (hasheq 'tag tag
                          'name name
                          'spawn-time ts
                          'death-time #f
                          'lifespan #f
                          'death-reason #f
                          'reproductions 0
                          'energy-given 0
                          'energy-taken 0))]
      [(equal? type "agent-died")
       (define tag (format "~a" (hash-ref rec 'tag "")))
       (define entry (hash-ref agent-data tag #f))
       (when entry
         (define spawn-t (hash-ref entry 'spawn-time 0))
         (define lifespan (- ts spawn-t))
         (define reason (hash-ref rec 'reason "unknown"))
         (hash-set! agent-data tag
                    (hash-set (hash-set (hash-set entry 'death-time ts)
                                        'lifespan lifespan)
                              'death-reason reason)))]
      [(equal? type "agent-reproduced")
       (define tag (format "~a" (hash-ref rec 'tag "")))
       (define succ? (hash-ref rec 'successful #f))
       (when succ?
         (define entry (hash-ref agent-data tag #f))
         (when entry
           (hash-set! agent-data tag
                      (hash-set entry 'reproductions
                                (add1 (hash-ref entry 'reproductions 0))))))]
      [(or (equal? type "gift-energy") (equal? type "take-energy"))
       (define tag (format "~a" (hash-ref rec 'tag "")))
       (define amt (hash-ref rec 'amount 0))
       (define entry (hash-ref agent-data tag #f))
       (when entry
         (if (equal? type "gift-energy")
             (hash-set! agent-data tag (hash-set entry 'energy-given (+ (hash-ref entry 'energy-given 0) amt)))
             (hash-set! agent-data tag (hash-set entry 'energy-taken (+ (hash-ref entry 'energy-taken 0) amt)))))]))

  agent-data)

;; Computes overall population summary metrics
(define (analyze-population-summary events)
  (define agent-stats (analyze-agent-stats events))
  (define total-agents (hash-count agent-stats))
  (define dead-agents (for/sum ([(k v) (in-hash agent-stats)]) (if (hash-ref v 'death-time #f) 1 0)))
  (define lifespans (for/list ([(k v) (in-hash agent-stats)] #:when (hash-ref v 'lifespan #f)) (hash-ref v 'lifespan)))
  (define avg-lifespan (if (pair? lifespans) (/ (apply + lifespans) (length lifespans)) 0))

  (hasheq 'total-agents total-agents
          'dead-agents dead-agents
          'living-agents (- total-agents dead-agents)
          'avg-lifespan avg-lifespan))
