#lang racket

;;; SPEC §4 — observation contract (v1: list style only).
;;; Pure functions of world state; computed on demand, never stored.
;;; Result: (hash 'cells ((dx . dy) -> (listof string))
;;;               'messages (name -> last broadcast)
;;;               'energy 'time-left 'inventory 'vision-radius)

(require "state.rkt")

(provide observe-agent
         observe-all)

(define (observe-agent w tag)
  (define p (world-params w))
  (define a (world-agent w tag))
  (define r (params-vision-radius p))
  (define gs (params-grid-size p))
  (define use-colors? (params-use-colors? p))
  (define inert? (params-inert-artifacts? p))
  (define origin (agent-pos a))
  (define pos->agent (world-pos->agent w))
  (define-values (cells messages)
    (for*/fold ([cells (hash)] [messages (hash)])
               ([dx (in-inclusive-range (- r) r)]
                [dy (in-inclusive-range (- r) r)])
      (define cell (wrap-pos (pos-add origin (pos dx dy)) gs))
      (define other-tag (hash-ref pos->agent cell #f))
      (define other (and other-tag
                         (not (equal? other-tag tag))
                         (world-agent w other-tag)))
      (define contents
        (let ([items '()])
          ;; food
          (when (hash-has-key? (world-food w) cell)
            (set! items (cons (number->string (hash-ref (world-food w) cell)) items)))
          ;; other beings
          (when other
            (set! items
                  (cons (if use-colors?
                            (format "~a(~a)" (agent-name other)
                                    (or (agent-color other) "no color"))
                            (agent-name other))
                        items)))
          ;; artifacts
          (unless inert?
            (for ([art-name (in-list (hash-ref (world-pos->artifacts w) cell '()))])
              (define art (hash-ref (world-artifacts w) art-name))
              (set! items
                    (cons (format "A(~a): ~a" (artifact-type art) art-name) items))))
          (reverse items)))
      ;; broadcasts from co-visible beings
      (define messages*
        (if other
            (let ([msg (agent-last-message other)])
              (if (> (string-length msg) 0)
                  (hash-set messages (agent-name other) msg)
                  messages))
            messages))
      (values (if (null? contents)
                  cells
                  (hash-set cells (cons dx dy) contents))
              messages*)))
  (hash 'cells cells
        'messages messages
        'energy (agent-energy a)
        'time-left (agent-time-left a)
        'inventory
        (if inert?
            '()
            (for/list ([art-name (in-list (agent-inventory a))])
              (define art (hash-ref (world-artifacts w) art-name #f))
              (and art (format "A(~a): ~a" (artifact-type art) art-name))))
        'vision-radius r))

(define (observe-all w)
  (for/hash ([tag (in-hash-keys (world-agents w))])
    (values tag (observe-agent w tag))))
