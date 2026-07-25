#lang racket

;;; Phase 6 — Interaction Network Graph Analysis

(provide build-interaction-network)

;; Builds a directed weighted interaction network map from event records
;; Adjacency hash: (cons src-tag tgt-tag) -> weight
(define (build-interaction-network events)
  (define weights (make-hash))

  (for ([rec (in-list events)])
    (define type (hash-ref rec 'type ""))
    (cond
      [(or (equal? type "gift-energy") (equal? type "take-energy"))
       (define src (format "~a" (hash-ref rec 'tag "")))
       (define tgt (format "~a" (hash-ref rec 'target-tag "")))
       (define amt (hash-ref rec 'amount 0))
       (define key (cons src tgt))
       (hash-set! weights key (+ (hash-ref weights key 0) amt))]
      [(equal? type "give-artifact")
       (define src (format "~a" (hash-ref rec 'tag "")))
       (define tgt (format "~a" (hash-ref rec 'target-tag "")))
       (define key (cons src tgt))
       (hash-set! weights key (+ (hash-ref weights key 0) 10))]))

  weights)
