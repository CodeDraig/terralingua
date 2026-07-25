#lang racket

;;; SPEC §5/PLAN decision 9 — owned thin transport. Two endpoints, no SDK:
;;;   OpenAI Responses:      POST {base}/responses
;;;   Anthropic Messages:    POST {base}/v1/messages

(require json
         net/url
         racket/port)

(provide (struct-out llm-transport)
         (struct-out exn:fail:llm)
         make-openai-transport
         make-anthropic-transport
         status-code-from-line
         openai-responses-payload
         response-output-text
         llm-complete)

(struct llm-transport (provider model base-url api-key max-tokens) #:prefab)

(struct exn:fail:llm exn:fail (status-code body) #:transparent)

(define (make-openai-transport model #:base-url [base-url #f] #:api-key [api-key #f] #:max-tokens [max-tokens 4096])
  (define url (or base-url (getenv "OPENAI_BASE_URL") "https://api.openai.com/v1"))
  (define key (or api-key (getenv "OPENAI_API_KEY") "dummy-key"))
  (llm-transport 'openai model url key max-tokens))

(define (make-anthropic-transport model #:base-url [base-url #f] #:api-key [api-key #f] #:max-tokens [max-tokens 4096])
  (define url (or base-url (getenv "ANTHROPIC_BASE_URL") "https://api.anthropic.com"))
  (define key (or api-key (getenv "ANTHROPIC_API_KEY") "dummy-key"))
  (llm-transport 'anthropic model url key max-tokens))

;; net/url returns a byte status line; retain string support for test doubles.
(define (status-code-from-line status-line)
  (define line
    (if (bytes? status-line) (bytes->string/utf-8 status-line) status-line))
  (define m (regexp-match #rx"HTTP/[0-9.]+ ([0-9]+)" line))
  (if m (string->number (cadr m)) 500))

;; Perform POST request via net/url, returning (values status-code json-hash)
(define (http-post-json url-str headers-list body-json)
  (define target-url (string->url url-str))
  (define body-bytes (string->bytes/utf-8 (jsexpr->string body-json)))
  (define-values (status-line resp-headers in-port)
    (http-sendrecv/url target-url
                       #:method "POST"
                       #:headers headers-list
                       #:data body-bytes))
  (define status-code (status-code-from-line status-line))
  (define response-text (port->string in-port))
  (close-input-port in-port)
  (define parsed-json
    (with-handlers ([exn:fail? (λ (_) (hash 'raw_response response-text))])
      (string->jsexpr response-text)))
  (values status-code parsed-json))

;; Keep Responses requests stateless: TerraLingua owns agent history and
;; checkpoints, rather than relying on provider-side response storage.
(define (openai-responses-payload model messages system max-tokens)
  (define payload
    (hasheq 'model model
            'input messages
            'max_output_tokens max-tokens
            'store #f))
  (if system
      (hash-set payload 'instructions system)
      payload))

;; Raw Responses API JSON represents text inside output message content. Some
;; compatible servers also expose a top-level output_text convenience field, so
;; accept it first without depending on it.
(define (response-output-text json-resp)
  (define direct (hash-ref json-resp 'output_text #f))
  (cond
    [(string? direct) direct]
    [else
     (or
      (for*/first ([item (in-list (hash-ref json-resp 'output '()))]
                   #:when (hash? item)
                   [part (in-list (hash-ref item 'content '()))]
                   #:when (and (hash? part)
                               (equal? (hash-ref part 'type #f) "output_text")
                               (string? (hash-ref part 'text #f))))
        (hash-ref part 'text))
      "")]))

;; Main transport completion entry point
;; (llm-complete transport messages #:system [system #f])
;; -> (values text-response status-code usage-hash)
(define (llm-complete transport messages #:system [system #f])
  (define provider (llm-transport-provider transport))
  (define model (llm-transport-model transport))
  (define base-url (llm-transport-base-url transport))
  (define api-key (llm-transport-api-key transport))
  (define max-tokens (llm-transport-max-tokens transport))

  (case provider
    [(openai)
     (define endpoint (string-append (string-trim base-url "/" #:left? #f) "/responses"))
     (define headers
       (list "Content-Type: application/json"
             (format "Authorization: Bearer ~a" api-key)))
     (define payload (openai-responses-payload model messages system max-tokens))
     (define-values (status-code json-resp)
       (http-post-json endpoint headers payload))
     (if (and (>= status-code 200) (< status-code 300))
         (values (response-output-text json-resp)
                 status-code
                 (hash-ref json-resp 'usage (hash)))
         (raise (exn:fail:llm (format "OpenAI API request failed (~a): ~a" status-code json-resp)
                              (current-continuation-marks)
                              status-code
                              json-resp)))]

    [(anthropic)
     (define endpoint (string-append (string-trim base-url "/" #:left? #f) "/v1/messages"))
     (define headers
       (list "Content-Type: application/json"
             (format "x-api-key: ~a" api-key)
             "anthropic-version: 2023-06-01"))
     (define payload
       (hasheq 'model model
               'max_tokens max-tokens
               'messages messages
               'system (or system "")))
     (define-values (status-code json-resp)
       (http-post-json endpoint headers payload))
     (if (and (>= status-code 200) (< status-code 300))
         (let* ([content-list (hash-ref json-resp 'content '())]
                [first-item (if (pair? content-list) (car content-list) (hash))]
                [text (hash-ref first-item 'text "")]
                [usage (hash-ref json-resp 'usage (hash))])
           (values text status-code usage))
         (raise (exn:fail:llm (format "Anthropic API request failed (~a): ~a" status-code json-resp)
                              (current-continuation-marks)
                              status-code
                              json-resp)))]

    [else (error 'llm-complete "Unsupported provider ~a" provider)]))
