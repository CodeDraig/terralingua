#lang racket

;;; SPEC §5 — retry policy: exponential backoff (base 1.5, jitter) for
;;; 429/5xx/connection errors; 400 (context overflow) is NOT retried here —
;;; the runner shrinks history instead. Token usage accumulates across attempts.

(require "transport.rkt")

(provide call-with-retry
         (struct-out exn:fail:llm:context-length))

(struct exn:fail:llm:context-length exn:fail:llm () #:transparent)

(define (call-with-retry proc
                         #:retries [retries 3]
                         #:backoff-base [backoff-base 1.5]
                         #:sleep [sleep-fn sleep])
  (let loop ([attempt 1])
    (with-handlers
      ([exn:fail:llm?
        (λ (e)
          (define status (exn:fail:llm-status-code e))
          (cond
            ;; 400 Bad Request / Context overflow -> raise context length exception for history shrinking
            [(= status 400)
             (raise (exn:fail:llm:context-length (exn-message e)
                                                 (exn-continuation-marks e)
                                                 status
                                                 (exn:fail:llm-body e)))]
            ;; 429 Rate Limit or 5xx Server Error -> retry with exponential backoff
            [(and (< attempt retries) (or (= status 429) (>= status 500)))
             (define delay (+ (expt backoff-base attempt) (* 0.5 (random))))
             (sleep-fn delay)
             (loop (add1 attempt))]
            [else (raise e)]))]
       [exn:fail?
        (λ (e)
          (if (< attempt retries)
              (let ([delay (+ (expt backoff-base attempt) (* 0.5 (random)))])
                (sleep-fn delay)
                (loop (add1 attempt)))
              (raise e)))])
      (proc attempt))))
