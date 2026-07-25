#lang racket

;; CLI configuration must control the OpenAI-compatible endpoint instead of
;; silently falling back to the ambient environment.

(require rackunit
         "../main.rkt"
         "../llm/transport.rkt")

(define configured
  (make-transport-from-config
   (hasheq 'provider "openai"
           'model "test-model"
           'openai-base-url "http://127.0.0.1:9000/v1")))

(check-equal? (llm-transport-provider configured) 'openai)
(check-equal? (llm-transport-model configured) "test-model")
(check-equal? (llm-transport-base-url configured) "http://127.0.0.1:9000/v1")
