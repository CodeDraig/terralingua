#lang racket

;; CLI configuration must control the OpenAI-compatible endpoint instead of
;; silently falling back to the ambient environment.

(require rackunit
         racket/file
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

;; Invalid seeds fail before creating output artifacts.
(define test-dir (make-temporary-file "tl_main_test_~a" 'directory))
(for ([bad-seed (in-list '("marmalade" "1.5" "-1" "2147483648"))]
      [i (in-naturals)])
  (define out (build-path test-dir (format "invalid-~a" i)))
  (check-exn
   #rx"--seed must be an integer"
   (λ ()
     (main (vector "--out-dir" (path->string out)
                   "--seed" bad-seed
                   "--max-ts" "0"))))
  (check-false (directory-exists? out)))

(check-exn
 #rx"--config and --checkpoint cannot be used together"
 (λ ()
   (main (vector "--config" "config.rkt"
                 "--checkpoint" "checkpoint.rktd"))))

(delete-directory/files test-dir)
