#lang racket

;;; Wraps an LLM completion + parse in a single safe call.
;;;
;;; Failure handling (SPEC §5 + retry.rkt contract):
;;;   - Transport error (network, 5xx, 429 exhausted): return fallback.
;;;   - 400 context overflow (exn:fail:llm:context-length?):
;;;     shrink history (drop oldest half) and retry. The render-fn is
;;;     re-invoked with the trimmed history so the prompt reflects it.
;;;   - Parse error: reprompt with the error message and retry.
;;;   - Exhausted max-attempts: return fallback.
;;;
;;; The render-fn is a (listof string) -> string closure that produces
;;; the user prompt given a (possibly trimmed) history. This keeps the
;;; LLM call wrapper agnostic of how the prompt is composed.

(require "../world/state.rkt"
         "../agent/parse.rkt"
         "transport.rkt"
         "retry.rkt")

(provide call-llm-with-fallback)

(define fallback-act (action "move" (hash "direction" "stay") ""))

;; Returns (values action-struct (or #f memory-string))
(define (call-llm-with-fallback transport sys-prompt render-fn avail
                                #:history [history '()]
                                #:max-attempts [max-attempts 5])
  (let loop ([attempt 1] [feedback #f] [hist history])
    (cond
      [(> attempt max-attempts) (values fallback-act #f)]
      [else
       (define base-prompt (render-fn hist))
       (define final-prompt
         (if feedback
             (string-append base-prompt
                            (format "\n\n[attempt ~a error]: ~a\nPlease retry, providing a valid JSON response."
                                    attempt feedback))
             base-prompt))
       (define-values (kind payload)
         (with-handlers ([exn:fail:llm:context-length?
                          (λ (e) (values 'context-overflow #f))]
                         [exn:fail? (λ (e) (values 'transport-error #f))])
           (define-values (raw-resp _code _usage)
             (call-with-retry
              (λ (_a)
                (llm-complete transport
                              (list (hash 'role "user" 'content final-prompt))
                              #:system sys-prompt))))
           (define-values (parsed-act mem err) (parse-action-response raw-resp avail))
           (cond
             [parsed-act (values 'ok (cons parsed-act mem))]
             [err (values 'parse-error err)]
             [else (values 'ok (cons fallback-act #f))])))
       (cond
         [(eq? kind 'transport-error) (values fallback-act #f)]
         [(eq? kind 'context-overflow)
          ;; Drop the oldest half of the history; keep at least the
          ;; most recent entry. Render-fn is re-invoked on the next
          ;; iteration with the trimmed history.
          (define shrunk
            (cond
              [(null? hist) '()]
              [else (drop hist (max 1 (quotient (length hist) 2)))]))
          (loop (add1 attempt) #f shrunk)]
         [(eq? kind 'parse-error) (loop (add1 attempt) payload hist)]
         [else (values (car payload) (cdr payload))])])))
