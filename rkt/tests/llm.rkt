#lang racket

;;; Phase 2 Unit Tests — LLM Transport & Retry Layer

(require rackunit
         "../llm/transport.rkt"
         "../llm/retry.rkt"
         "../llm/agent-call.rkt"
         "../world/state.rkt")

;; ---------------------------------------------------------------------------
;; 1. Transport Construction Tests
;; ---------------------------------------------------------------------------

(define t-openai (make-openai-transport "gpt-4o-mini" #:base-url "http://localhost:8080/v1" #:api-key "test-key"))
(check-equal? (llm-transport-provider t-openai) 'openai)
(check-equal? (llm-transport-model t-openai) "gpt-4o-mini")
(check-equal? (llm-transport-base-url t-openai) "http://localhost:8080/v1")
(check-equal? (llm-transport-api-key t-openai) "test-key")
(check-equal? (status-code-from-line #"HTTP/1.1 200 OK\r\n") 200)
(check-equal? (status-code-from-line "HTTP/2 429 Too Many Requests\r\n") 429)

;; OpenAI uses the stateless Responses API, not Chat Completions.
(define responses-payload
  (openai-responses-payload
   "gpt-4o-mini"
   (list (hash 'role "user" 'content "hello"))
   "system rules"
   64))
(check-equal? (hash-ref responses-payload 'input)
              (list (hash 'role "user" 'content "hello")))
(check-equal? (hash-ref responses-payload 'instructions) "system rules")
(check-equal? (hash-ref responses-payload 'max_output_tokens) 64)
(check-false (hash-ref responses-payload 'store) "responses remain stateless")
(check-equal?
 (response-output-text
  (hash 'output
        (list (hash 'type "reasoning" 'content '())
              (hash 'type "message"
                    'content (list (hash 'type "output_text" 'text "valid JSON"))))))
 "valid JSON"
 "extracts text from a raw Responses API message")
(check-equal? (response-output-text (hash 'output_text "shortcut")) "shortcut")

(define t-anthropic (make-anthropic-transport "claude-3-5-sonnet" #:base-url "http://localhost:8080" #:api-key "test-ant"))
(check-equal? (llm-transport-provider t-anthropic) 'anthropic)
(check-equal? (llm-transport-model t-anthropic) "claude-3-5-sonnet")

;; ---------------------------------------------------------------------------
;; 2. Retry Policy & Backoff Tests
;; ---------------------------------------------------------------------------

(define noop-sleep (λ (sec) (void)))

;; Success on first try
(define result1
  (call-with-retry (λ (attempt) "success") #:retries 3 #:sleep noop-sleep))
(check-equal? result1 "success")

;; Retry on 429 rate limit then succeed
(define attempts 0)
(define result2
  (call-with-retry
   (λ (attempt)
     (set! attempts (add1 attempts))
     (if (< attempts 3)
         (raise (exn:fail:llm "Rate limit" (current-continuation-marks) 429 (hash)))
         "recovered"))
   #:retries 3 #:sleep noop-sleep))

(check-equal? result2 "recovered")
(check-equal? attempts 3 "tried 3 times before succeeding")

;; 400 Bad Request -> raises context length exception immediately without retrying
(define 400-attempts 0)
(check-exn
 exn:fail:llm:context-length?
 (λ ()
   (call-with-retry
    (λ (attempt)
      (set! 400-attempts (add1 400-attempts))
      (raise (exn:fail:llm "Context overflow" (current-continuation-marks) 400 (hash))))
    #:retries 3 #:sleep noop-sleep)))

(check-equal? 400-attempts 1 "400 error does not retry")

;; ---------------------------------------------------------------------------
;; 3. agent-call fallback (SPEC §5: parse-retry + transport fallback)
;; ---------------------------------------------------------------------------

;; An unreachable transport: every call raises exn:fail (after retries
;; in call-with-retry). The helper should swallow it and return the
;; (move, stay) fallback with no memory.
(define t-bad (make-openai-transport "gpt-4o-mini"
                                    #:base-url "http://127.0.0.1:1"
                                    #:api-key "x"
                                    #:max-tokens 16))
(define-values (fallback-act fallback-mem)
  (call-llm-with-fallback t-bad "system" (λ (h) "user") (hash)))
(check-equal? (action-name fallback-act) "move"
              "transport error -> fallback action is move/stay")
(check-equal? (action-message fallback-act) ""
              "fallback message is empty")
(check-false fallback-mem "no memory on transport error")

;; With a valid parse-shaped string but an unreachable transport, the
;; parse-retry loop is never reached (transport fails first). Confirm
;; the max-attempts kwarg is accepted and respected.
(define-values (act2 mem2)
  (call-llm-with-fallback t-bad "system" (λ (h) "user") (hash) #:max-attempts 2))
(check-equal? (action-name act2) "move"
              "max-attempts kwarg accepted; fallback still returned")

;; render-fn is re-invoked with the supplied history (smoke check that
;; the closure is actually called).
(define hist-rendered '())
(define-values (act3 mem3)
  (call-llm-with-fallback t-bad "system"
                          (λ (h)
                            (set! hist-rendered (cons h hist-rendered))
                            "user")
                          (hash)
                          #:history '("a" "b" "c" "d")))
(check-true (pair? hist-rendered) "render-fn was invoked at least once")
(check-equal? (car hist-rendered) '("a" "b" "c" "d")
              "render-fn received the full history on first call")
