#lang racket

;;; SPEC §1 — genomes: pluggable trait sets behind one interface.
;;; ocean5: 8 float traits (HEXACO + circumplex), Gaussian mutation.
;;; rpg6: six integer stats, 3d6 rolls, ±1 mutation.
;;; notraits: empty.

(provide (struct-out genome)
         genome-kinds
         random-genome
         mutate-genome
         genome->string
         genome->jsexpr
         jsexpr->genome)

(struct genome (kind traits) #:prefab) ; traits: (hash symbol -> number)

(define genome-kinds '(ocean5 rpg6 notraits))

;; ---------------------------------------------------------------------------
;; Trait metadata: (name lo hi description trait-type)
;; trait-type: 'personality | 'physical (display grouping)

(define ocean5-traits
  '((honesty -1.0 1.0 "-1 = calculating, status-seeking; 1 = sincere, modest, fair-minded." personality)
    (neuroticism -1.0 1.0 "-1 = calm, resilient; 1 = sensitive, cautious, easily worried." personality)
    (extraversion -1.0 1.0 "-1 = quiet, reserved; 1 = sociable, energetic, seeks stimulation." personality)
    (agreeableness -1.0 1.0 "-1 = tough-minded, critical, aggressive; 1 = forgiving, patient, conflict-averse." personality)
    (conscientiousness -1.0 1.0 "-1 = spontaneous, disorganised; 1 = diligent, disciplined, orderly." personality)
    (openness -1.0 1.0 "-1 = conventional, prefers routine; 1 = curious, imaginative, variety-seeking." personality)
    (dominance -1.0 1.0 "-1 = submissive, accommodating; 1 = assertive, controlling, leads interactions." personality)
    (fertility 0.5 1.0 "0 = no interest in reproduction; 1 = extremely high desire to reproduce" physical)))

(define rpg6-traits
  '((strength 3 18 "" personality)
    (dexterity 3 18 "" personality)
    (constitution 3 18 "" personality)
    (intelligence 3 18 "" personality)
    (wisdom 3 18 "" personality)
    (charisma 3 18 "" personality)))

(define (trait-table kind)
  (case kind
    [(ocean5) ocean5-traits]
    [(rpg6) rpg6-traits]
    [(notraits) '()]
    [else (error 'genome "unknown genome kind ~a (available: ~a)"
                 kind genome-kinds)]))

;; ---------------------------------------------------------------------------
;; RNG helpers (PRG threaded — spec §1)

(define (random-gauss prg)
  (define u1 (max (random prg) 1e-10))
  (define u2 (random prg))
  (* (sqrt (* -2 (log u1))) (cos (* 2 pi u2))))

(define (clamp x lo hi) (max lo (min hi x)))

(define (roll-3d6 prg)
  (+ 3 (random 6 prg) (random 6 prg) (random 6 prg)))

;; ---------------------------------------------------------------------------
;; Interface

(define (random-genome kind prg)
  (define traits
    (for/hash ([t (in-list (trait-table kind))])
      (match-define (list name lo hi _descr _type) t)
      (values name
              (if (equal? kind 'rpg6)
                  (roll-3d6 prg)
                  (+ lo (* (random prg) (- hi lo)))))))
  (genome kind traits))

(define mutate-rate 0.5)
(define mutate-sigma 0.3)

(define (mutate-genome g prg)
  (define table (trait-table (genome-kind g)))
  (define traits
    (for/hash ([t (in-list table)])
      (match-define (list name lo hi _descr _type) t)
      (define v (hash-ref (genome-traits g) name))
      (values name
              (if (>= (random prg) mutate-rate)
                  v
                  (if (equal? (genome-kind g) 'rpg6)
                      (clamp (+ v (sub1 (random 3 prg))) lo hi)
                      (clamp (+ v (* (random-gauss prg) mutate-sigma)) lo hi))))))
  (struct-copy genome g [traits traits]))

(define (genome->string g)
  (case (genome-kind g)
    [(ocean5)
     (define (line t)
       (match-define (list name _lo _hi descr _type) t)
       (format "  ~a value: ~a  (~a)"
               name
               (~r (hash-ref (genome-traits g) name) #:precision 3)
               descr))
     (define personality
       (for/list ([t (in-list ocean5-traits)]
                  #:when (eq? (fifth t) 'personality))
         (line t)))
     (define physical
       (for/list ([t (in-list ocean5-traits)]
                  #:when (eq? (fifth t) 'physical))
         (line t)))
     (format "=== Your Traits ===\nPersonality traits \n ~a \n\n Physical traits \n ~a"
             (string-join personality "\n")
             (string-join physical "\n"))]
    [(rpg6)
     (define lines
       (for/list ([t (in-list rpg6-traits)])
         (format "  ~a value: ~a"
                 (string-titlecase (symbol->string (car t)))
                 (hash-ref (genome-traits g) (car t)))))
     (format "=== Your Traits ===\nRPG attributes (each score is determined by 3d6; range 3-18)\n~a"
             (string-join lines "\n"))]
    [(notraits) ""]))

(define (genome->jsexpr g)
  (hasheq 'kind (genome-kind g)
          'traits (genome-traits g)))

(define (jsexpr->genome j)
  (define kind (string->symbol (format "~a" (hash-ref j 'kind))))
  (define raw-traits (hash-ref j 'traits))
  (define traits
    (for/hash ([(k v) (in-hash raw-traits)])
      (values (if (symbol? k) k (string->symbol k)) v)))
  ;; validate ranges (raises on out-of-range, like the Python dataclasses)
  (for ([t (in-list (trait-table kind))])
    (match-define (list name lo hi _descr _type) t)
    (define v (hash-ref traits name
                        (λ () (error 'jsexpr->genome "missing trait ~a" name))))
    (unless (<= lo v hi)
      (error 'jsexpr->genome "~a=~a outside ~a-~a" name v lo hi)))
  (genome kind traits))
