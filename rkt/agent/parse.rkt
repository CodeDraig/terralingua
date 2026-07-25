#lang racket

;;; SPEC §5 — tolerant response parsing.
;;; Pipeline:
;;;   1. Strip <think>...</think> prefix
;;;   2. Extract code-fence JSON or first {...} span
;;;   3. Repair common JSON errors (unquoted keys, single quotes, trailing commas)
;;;   4. Case-fold keys to lowercase strings
;;;   5. Validate action name ∈ available-actions
;;;   6. Validate exact param keys match schema

(require json
         "../world/state.rkt")

(provide parse-action-response
         strip-think
         extract-json-substring
         repair-json-string)

;; Strip reasoning / thinking tags: <think>...</think> or orphan </think>
(define (strip-think text)
  (if (not (string? text))
      ""
      (let* ([t1 (regexp-replace* #px"(?s:<think>.*?</think>)" text "")]
             [t2 (regexp-replace* #px"(?s:^.*?</think>)" t1 "")])
        (string-trim t2))))

;; Extract potential JSON substring from text
(define (extract-json-substring text)
  (define cleaned (strip-think text))
  ;; Check for ```json ... ``` or ``` ... ```
  (cond
    [(regexp-match #px"(?s:```(?:json)?\\s*(.*?)\\s*```)" cleaned)
     => (λ (m) (string-trim (cadr m)))]
    [else
     ;; Find first { and last }
     (define match-start (regexp-match-positions #px"\\{" cleaned))
     (define match-end (regexp-match-positions #px"\\}(?=[^\\}]*$)" cleaned))
     (if (and match-start match-end)
         (let ([start (car (car match-start))]
               [end (cdr (car match-end))])
           (if (< start end)
               (substring cleaned start end)
               cleaned))
         cleaned)]))

;; Basic string repair for dirty LLM JSON outputs (unquoted keys, single quotes, trailing commas)
(define (repair-json-string s)
  (let* ([s1 (regexp-replace* #px",\\s*([\\}\\]])" s "\\1")] ; trailing commas
         [s2 (regexp-replace* #px"([\\{\\[,])\\s*([a-zA-Z0-9_-]+)\\s*:" s1 "\\1\"\\2\":")] ; unquoted keys
         [s3 (regexp-replace* #px"'([^'\"]*)'" s2 "\"\\1\"")]) ; single-quoted string values/keys
    s3))

;; Deep case-fold dictionary keys to lowercase strings
(define (case-fold-keys v)
  (cond
    [(hash? v)
     (for/hash ([(k val) (in-hash v)])
       (define k-str (string-downcase (if (symbol? k) (symbol->string k) (format "~a" k))))
       (values k-str (case-fold-keys val)))]
    [(list? v) (map case-fold-keys v)]
    [else v]))

;; Attempts to parse string to a hash table
(define (parse-json-to-hash text)
  (define raw-sub (extract-json-substring text))
  (define (try-parse s)
    (with-handlers ([exn:fail? (λ (_) #f)])
      (define parsed (string->jsexpr s))
      (and (hash? parsed) (case-fold-keys parsed))))

  (or (try-parse raw-sub)
      (try-parse (repair-json-string raw-sub))
      ;; Try double-encoded string unquoting
      (with-handlers ([exn:fail? (λ (_) #f)])
        (define unquoted (string->jsexpr (string-trim raw-sub)))
        (and (string? unquoted) (try-parse (repair-json-string unquoted))))))

;; Main parsing function
;; (parse-action-response text available-actions) -> (values action-struct memory-string error-msg-or-#f)
(define (parse-action-response text available-actions)
  (define parsed (parse-json-to-hash text))
  (if (not parsed)
      (values #f #f "Failed to parse JSON response object")
      (let* ([act-name (hash-ref parsed "action" #f)]
             [msg (format "~a" (hash-ref parsed "message" ""))]
             [raw-params (hash-ref parsed "params" (hash))]
             [params (if (hash? raw-params) raw-params (hash))]
             [mem (let ([m (hash-ref parsed "internal_memory" #f)])
                    (and m (string? m) m))])
        (cond
          [(not (string? act-name))
           (values #f #f "Missing or invalid 'action' key in JSON response")]
          [(not (hash-has-key? available-actions act-name))
           (values #f #f (format "Action '~a' is not available. Available actions: ~a"
                                 act-name (sort (hash-keys available-actions) string<?)))]
          [else
           (define schema (hash-ref available-actions act-name))
           (define req-params-hash (or (hash-ref schema 'params #f)
                                       (hash-ref schema "params" (hash))))
           (define expected-keys (list->set (map (λ (k) (if (symbol? k) (symbol->string k) k))
                                                 (hash-keys req-params-hash))))
           (define actual-keys (list->set (map (λ (k) (if (symbol? k) (symbol->string k) k))
                                               (hash-keys params))))
           (if (not (equal? expected-keys actual-keys))
               (values #f #f (format "Action '~a' parameter keys mismatch. Expected keys ~a, got ~a"
                                     act-name (set->list expected-keys) (set->list actual-keys)))
               (values (action act-name params msg) mem #f))]))))
