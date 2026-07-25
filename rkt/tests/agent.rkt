#lang racket

;;; Phase 2 Unit Tests — Agent Layer (Memory, Parsing, Prompt Rendering)

(require rackunit
         json
         "../world/state.rkt"
         "../world/actions.rkt"
         "../agent/memory.rkt"
         "../agent/parse.rkt"
         "../agent/prompt.rkt")

;; ---------------------------------------------------------------------------
;; 1. Internal Memory Capping Tests
;; ---------------------------------------------------------------------------

(check-equal? (cap-internal-memory "hello world" 10) "hello world" "under budget")
(check-equal? (cap-internal-memory (make-string 100 #\a) 10) (make-string 40 #\a) "capped at 4*10 = 40 chars")
(check-equal? (cap-internal-memory #f 10) "" "false input -> empty string")
(check-equal? (cap-internal-memory "test" 0) "" "zero budget -> empty string")

;; ---------------------------------------------------------------------------
;; 2. Tolerant Response Parsing Tests
;; ---------------------------------------------------------------------------

(define p (params 10 2 100 50 100 10.0 0.05 1.0 1 #f #f #t 'single #t 50 #t 0 #f #t #t #f))
(define w0 (make-world p))
(define a0 (agent 'being0 "Aelion" (pos 2 2) 50 100 #f '() '() '() ""))
(define w (world-add-agent w0 a0))
(define avail (available-actions w 'being0))

;; Standard valid JSON
(define valid-json
  "{\"action\": \"move\", \"message\": \"hi\", \"params\": {\"direction\": \"up\"}}")

(define-values (act1 mem1 err1) (parse-action-response valid-json avail))
(check-false err1 "valid json parses without error")
(check-equal? (action-name act1) "move")
(check-equal? (hash-ref (action-params act1) "direction") "up")
(check-equal? (action-message act1) "hi")
(check-false mem1 "no internal memory in this turn")

;; Reasoning / Thinking tag + Markdown code block
(define thinking-json
  "<think>\nLet us move up to search for food.\n</think>\n```json\n{\"ACTION\": \"move\", \"MESSAGE\": \"hello\", \"PARAMS\": {\"direction\": \"down\"}, \"INTERNAL_MEMORY\": \"remember pos\"}\n```")

(define-values (act2 mem2 err2) (parse-action-response thinking-json avail))
(check-false err2 "thinking tag + code block parses cleanly")
(check-equal? (action-name act2) "move")
(check-equal? (hash-ref (action-params act2) "direction") "down")
(check-equal? mem2 "remember pos")

;; Dirty JSON (unquoted keys, trailing comma)
(define dirty-json
  "{action: \"move\", params: {direction: \"left\"}, message: \"yolo\",}")

(define-values (act3 mem3 err3) (parse-action-response dirty-json avail))
(check-false err3 "dirty json repaired and parsed")
(check-equal? (action-name act3) "move")
(check-equal? (hash-ref (action-params act3) "direction") "left")

;; Unknown action error
(define unknown-act-json
  "{\"action\": \"fly\", \"params\": {}}")

(define-values (act4 mem4 err4) (parse-action-response unknown-act-json avail))
(check-false act4 "unknown action fails")
(check-true (string-prefix? err4 "Action 'fly' is not available") "gives clear error msg")

;; Parameter mismatch error
(define bad-param-json
  "{\"action\": \"move\", \"params\": {\"dir\": \"up\"}}")

(define-values (act5 mem5 err5) (parse-action-response bad-param-json avail))
(check-false act5 "bad param keys fail")
(check-true (string-prefix? err5 "Action 'move' parameter keys mismatch") "gives param mismatch error")

;; ---------------------------------------------------------------------------
;; 3. Prompt Rendering Tests
;; ---------------------------------------------------------------------------

(define sys-prompt (render-system-prompt "Aelion" p 'base))
(check-true (string-contains? sys-prompt "You are Aelion") "system prompt includes agent name")
(check-true (string-contains? sys-prompt "Final remarks:") "system prompt includes exogenous motivation")
(check-true (string-contains? sys-prompt "INTERNAL MEMORY")
              "system prompt includes INTERNAL MEMORY rules when use-internal-memory? is on")

;; Verify use-internal-memory? is its own switch (not aliased to use-inventory?).
;; In this test p has both #t; build a copy with inventory off to confirm memory
;; is independently controlled.
(define p-no-inv
  (struct-copy params p
               [use-inventory? #f]
               [use-internal-memory? #t]))
(define sys-prompt-mem-only (render-system-prompt "X" p-no-inv 'base))
(check-true (string-contains? sys-prompt-mem-only "INTERNAL MEMORY")
              "memory rules present when only use-internal-memory? is on")
(check-false (string-contains? sys-prompt-mem-only "current content of your inventory")
              "inventory rules absent when use-inventory? is off")

(define p-no-mem
  (struct-copy params p
               [use-inventory? #t]
               [use-internal-memory? #f]))
(define sys-prompt-inv-only (render-system-prompt "X" p-no-mem 'base))
(check-false (string-contains? sys-prompt-inv-only "INTERNAL MEMORY")
              "memory rules absent when only use-inventory? is on")
(check-true (string-contains? sys-prompt-inv-only "current content of your inventory")
              "inventory rules present when use-inventory? is on")

(define user-prompt
  (render-user-prompt #:history "Turn 1: moved up"
                      #:genome "Traits: bold"
                      #:observation "{(0,0): <yourself>}"
                      #:messages ""
                      #:energy 50
                      #:time 99
                      #:available-actions avail))

(check-true (string-contains? user-prompt "=== Current State ===") "user prompt includes header")
(check-true (string-contains? user-prompt "=== Available Actions & Params ===") "user prompt includes actions section")
(check-true (string-contains? user-prompt "move:") "user prompt includes move action")

;; Co-visible messages section: a non-empty #:messages string is rendered.
(define user-prompt-with-msgs
  (render-user-prompt #:history ""
                      #:genome ""
                      #:observation ""
                      #:messages "Aelion: hello there; Mara: watch out"
                      #:energy 50
                      #:time 99
                      #:available-actions avail))
(check-true (string-contains? user-prompt-with-msgs "Incoming messages:")
              "user prompt has Incoming messages section")
(check-true (string-contains? user-prompt-with-msgs "Aelion: hello there")
              "user prompt surfaces co-visible Aelion's broadcast")
(check-true (string-contains? user-prompt-with-msgs "Mara: watch out")
              "user prompt surfaces co-visible Mara's broadcast")
