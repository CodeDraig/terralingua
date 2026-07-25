#lang racket

;;; Genome property tests — SPEC §1. Run: raco test rkt/

(require rackunit
         "../genome/main.rkt")

(define (mk-prg seed)
  (define prg (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator prg])
    (random-seed seed))
  prg)

;; ocean5: 8 traits, all within ranges (fertility in [0.5, 1.0])
(for ([seed (in-range 20)])
  (define g (random-genome 'ocean5 (mk-prg seed)))
  (check-equal? (genome-kind g) 'ocean5)
  (check-equal? (hash-count (genome-traits g)) 8)
  (for ([(name v) (in-hash (genome-traits g))])
    (if (eq? name 'fertility)
        (check-true (<= 0.5 v 1.0) "fertility range")
        (check-true (<= -1.0 v 1.0) (format "~a range" name)))))

;; rpg6: six integer stats in [3, 18]
(for ([seed (in-range 20)])
  (define g (random-genome 'rpg6 (mk-prg seed)))
  (check-equal? (hash-count (genome-traits g)) 6)
  (for ([(name v) (in-hash (genome-traits g))])
    (check-true (and (integer? v) (<= 3 v 18)) "3d6 range")))

;; notraits: empty
(check-equal? (genome-traits (random-genome 'notraits (mk-prg 0))) (hash))
(check-equal? (genome->string (random-genome 'notraits (mk-prg 0))) "")

;; mutation respects ranges
(for ([seed (in-range 30)])
  (define g (random-genome 'ocean5 (mk-prg seed)))
  (define m (mutate-genome g (mk-prg (+ seed 1000))))
  (for ([(name v) (in-hash (genome-traits m))])
    (if (eq? name 'fertility)
        (check-true (<= 0.5 v 1.0) "mutated fertility range")
        (check-true (<= -1.0 v 1.0) "mutated trait range"))))
(for ([seed (in-range 30)])
  (define g (random-genome 'rpg6 (mk-prg seed)))
  (define m (mutate-genome g (mk-prg (+ seed 2000))))
  (for ([(name v) (in-hash (genome-traits m))])
    (check-true (and (integer? v) (<= 3 v 18)) "mutated rpg6 range")))

;; jsexpr round-trip
(let ([g (random-genome 'ocean5 (mk-prg 7))])
  (check-equal? (jsexpr->genome (genome->jsexpr g)) g))
(let ([g (random-genome 'rpg6 (mk-prg 8))])
  (check-equal? (jsexpr->genome (genome->jsexpr g)) g))

;; jsexpr->genome rejects out-of-range data
(check-exn exn:fail?
           (λ () (jsexpr->genome
                  (hasheq 'kind 'ocean5
                          'traits (hasheq 'honesty 5.0 'neuroticism 0.0
                                          'extraversion 0.0 'agreeableness 0.0
                                          'conscientiousness 0.0 'openness 0.0
                                          'dominance 0.0 'fertility 0.8)))))

;; prompt rendering mentions the trait names
(check-true (regexp-match? #rx"honesty" (genome->string (random-genome 'ocean5 (mk-prg 3)))))
(check-true (regexp-match? #rx"Strength" (genome->string (random-genome 'rpg6 (mk-prg 3)))))

;; unknown kind is a contract error
(check-exn exn:fail? (λ () (random-genome 'dnd5e (mk-prg 1))))
