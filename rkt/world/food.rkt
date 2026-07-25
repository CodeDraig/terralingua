#lang racket

;;; SPEC §1 (food model) and §2 S8 (decay + respawn).
;;; Density field: uniform, or Gaussian mixture over food-centers (σ = 2.0,
;;; toroidal distance), zeroed on cells occupied by agents, food, artifacts.

(require "state.rkt")

(provide world-seed-food
         decay-and-respawn-food
         food-sum
         default-free-cell?
         make-world-with-food)

(define food-sigma 2.0)

;; Knuth's algorithm; fine for the small lambdas used here (spawn-rate ≈ 1–3).
(define (random-poisson lam prg)
  (if (<= lam 0)
      0
      (let ([L (exp (- lam))])
        (let loop ([k 0] [p 1.0])
          (define p* (* p (random prg)))
          (if (<= p* L) k (loop (add1 k) p*))))))

(define (food-sum w)
  (for/sum ([v (in-hash-values (world-food w))]) v))

;; Density weight of a cell: uniform when no centers, else Gaussian mixture.
(define (density-weight p centers gs)
  (if (null? centers)
      1.0
      (for/sum ([c (in-list centers)])
        (define (axis d) (min (abs d) (- gs (abs d))))
        (define dx (axis (- (pos-x p) (pos-x c))))
        (define dy (axis (- (pos-y p) (pos-y c))))
        (exp (* -0.5 (/ (+ (* dx dx) (* dy dy)) (* food-sigma food-sigma)))))))

;; Weighted sample of a cell satisfying free? — or #f if none available.
(define (sample-food-cell w free? prg)
  (define gs (params-grid-size (world-params w)))
  (define centers (world-food-centers w))
  (define weighted
    (for*/list ([x (in-range gs)] [y (in-range gs)]
                #:when (free? (pos x y)))
      (cons (pos x y) (density-weight (pos x y) centers gs))))
  (define total (for/sum ([pw (in-list weighted)]) (cdr pw)))
  (if (or (null? weighted) (<= total 0))
      #f
      (let loop ([r (* (random prg) total)] [ws weighted])
        (cond
          [(null? ws) #f]
          [(<= r (cdar ws)) (caar ws)]
          [else (loop (- r (cdar ws)) (cdr ws))]))))

;; Default free? predicate: no food, no agent, no artifact on the cell.
;; O(1) per cell via the pos->artifacts index (SPEC §1 decision 4).
(define (default-free-cell? w)
  (λ (p)
    (and (not (hash-has-key? (world-food w) p))
         (not (hash-has-key? (world-pos->agent w) p))
         (null? (hash-ref (world-pos->artifacts w) p '())))))

;; One-shot helper: build a world and seed it with food. Callers
;; (e.g. the runner) should use this instead of `make-world` so the
;; world never starts empty. Lives here (not in state.rkt) to avoid
;; a cyclic import — food.rkt already requires state.rkt.
(define (make-world-with-food p prg)
  (world-seed-food (make-world p) prg))

;; SPEC §1: choose zone centers (integer count → random cells), seed
;; init-food tiles at max-food-value. Called once at world creation.
(define (world-seed-food w prg)
  (define p (world-params w))
  (define gs (params-grid-size p))
  (define centers
    (cond
      [(exact-positive-integer? (params-food-zones p))
       (for/list ([_ (in-range (params-food-zones p))])
         (pos (random gs prg) (random gs prg)))]
      [(list? (params-food-zones p)) (params-food-zones p)]
      [else '()]))
  (define w1 (struct-copy world w [food-centers centers]))
  (define free? (default-free-cell? w1))
  (define food
    (let loop ([n (params-init-food p)] [acc (hash)])
      (if (or (zero? n) (= (hash-count acc) (* gs gs)))
          acc
          (let ([cell (sample-food-cell w1 (λ (c) (and (free? c)
                                                       (not (hash-has-key? acc c))))
                                        prg)])
            (if (not cell)
                acc
                (loop (sub1 n) (hash-set acc cell (params-max-food-value p))))))))
  (struct-copy world w1
               [food food]
               [food-count-history
                (cons (for/sum ([v (in-hash-values food)]) v)
                      (world-food-count-history w1))]))

;; SPEC §2 S8: each tile decays by food-decay-amount with probability
;; food-decay-rate; expired tiles are removed (and pooled when static-food?);
;; then Poisson(food-spawn-rate) new tiles spawn at max-food-value.
(define (decay-and-respawn-food w prg)
  (define p (world-params w))
  (define rate (params-food-decay-rate p))
  (define amount (params-food-decay-amount p))
  (define-values (food* emptied)
    (for/fold ([f (hash)] [empty '()])
              ([(cell v) (in-hash (world-food w))])
      (define v* (if (< (random prg) rate) (- v amount) v))
      (if (<= v* 0)
          (values f (cons cell empty))
          (values (hash-set f cell v*) empty))))
  (define empty-pool
    (if (params-static-food? p)
        (append (world-empty-food w) emptied)
        (world-empty-food w)))
  (define w1 (struct-copy world w [food food*] [empty-food empty-pool]))
  (define n-spawn (random-poisson (params-food-spawn-rate p) prg))
  (let loop ([wc w1] [i n-spawn])
    (if (zero? i)
        wc
        (let-values ([(cell wc*)
                      (if (params-static-food? p)
                          (pop-empty-pool wc prg)
                          (values (sample-food-cell wc (default-free-cell? wc) prg)
                                  wc))])
          (if (not cell)
              wc*
              (loop (struct-copy world wc*
                                 [food (hash-set (world-food wc*) cell
                                                 (params-max-food-value p))])
                    (sub1 i)))))))

;; Pop a random cell from the static-food pool; (values cell-or-#f world).
(define (pop-empty-pool w prg)
  (define pool (world-empty-food w))
  (if (null? pool)
      (values #f w)
      (let* ([i (random (length pool) prg)]
             [cell (list-ref pool i)])
        (values cell
                (struct-copy world w
                             [empty-food (append (take pool i)
                                                 (drop pool (add1 i)))])))))
