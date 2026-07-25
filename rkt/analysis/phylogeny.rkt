#lang racket

;;; Phase 6 — Artifact Phylogeny & Version Lineage Analysis

(provide build-artifact-phylogeny)

;; Builds artifact lineage records from events
(define (build-artifact-phylogeny events)
  (define artifacts (make-hash))

  (for ([rec (in-list events)])
    (define type (hash-ref rec 'type ""))
    (define ts (hash-ref rec 'ts 0))
    (cond
      [(equal? type "artifact-added")
       (define name (format "~a" (hash-ref rec 'name "")))
       (define creator (format "~a" (hash-ref rec 'creator "")))
       (define payload (format "~a" (hash-ref rec 'payload "")))
       (hash-set! artifacts name
                  (hasheq 'name name
                          'creator creator
                          'created-at ts
                          'versions (list (hasheq 'v 1 'ts ts 'payload payload))
                          'interactions 0))]
      [(equal? type "artifact-interaction")
       (define name (format "~a" (hash-ref rec 'artifact "")))
       (define payload (hash-ref rec 'payload #f))
       (define entry (hash-ref artifacts name #f))
       (when entry
         (define inter (add1 (hash-ref entry 'interactions 0)))
         (define vers (hash-ref entry 'versions '()))
         (define new-vers
           (if payload
               (cons (hasheq 'v (add1 (length vers)) 'ts ts 'payload (format "~a" payload)) vers)
               vers))
         (hash-set! artifacts name
                    (hash-set (hash-set entry 'interactions inter)
                              'versions new-vers)))]))

  artifacts)
