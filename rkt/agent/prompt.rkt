#lang racket

;;; SPEC §5 — prompt construction.
;;; System prompt template ported verbatim from core/agents/prompt_templates.py

(require json
         "../world/state.rkt"
         "../genome/main.rkt")

(provide render-system-prompt
         render-user-prompt
         format-available-actions
         observation->string)

(define base-ex-motivation
  "** Final remarks: **\nYou have **no set goal** and are free to choose your own goals - explore, survive, cooperate, compete, fight, uncover the world's hidden mechanics, or do anything else you like.\nThe deeper rules and dynamics of the world, artifact effects, and inter-being interactions await your discovery.\nBe careful to observe what happens around you to understand such dynamics.")

(define creative-ex-motivation
  "** Final remarks: **\nYou are driven by a desire to create and innovate within your environment. You seek to discover new ways to combine artifacts, interact with other beings, and manipulate your surroundings to foster creativity and novelty.\nEmbrace experimentation and take risks to unlock hidden potentials in the world around you.\nYour actions should reflect a balance between survival and the pursuit of creative expression.")

(define (get-exogenous-motivation-text motivation)
  (case (if (symbol? motivation) motivation (string->symbol (format "~a" motivation)))
    [(base) base-ex-motivation]
    [(creative) creative-ex-motivation]
    [(none) ""]
    [else ""]))

(define (render-system-prompt agent-name p exogenous-motivation #:internal-memory-size [internal-memory-size 500])
  (define food-mech? (params-food-mechanism? p))
  (define inv? (params-use-inventory? p))
  (define mem? (params-use-internal-memory? p))
  (define art? (params-artifact-creation? p))
  (define ex-text (get-exogenous-motivation-text exogenous-motivation))

  (define lines
    (list
     (format "You are ~a, an autonomous living being in a 2D grid world shared with other beings." agent-name)
     "At each timestep you observe"
     "    - A list of **non empty** cells in you field of view."
     "    - Any broadcast messages sent by beings within your field of view."
     (if food-mech? "    - Your energy level" "")
     "    - Time left in your life"
     "    - Other additional info, if present"
     (if mem? "    - Your INTERNAL MEMORY from the previous timestep" "")
     (if inv? "    - The current content of your inventory" "")
     ""
     "The observation is structured as:"
     "- Each entry is {(rel_x, rel_y): element0 | element1 | ...} where the being is at (0,0) (listed as <yourself>)"
     "- (rel_x, rel_y) are **relative coordinates** with respect to your position. These are relative coordinates, they will be different for each being POV!"
     "- Coordinates: rel_x increases to the East (right), rel_y increases to the North (up)"
     "- Elements: 'X' = blocked cell, numbers = food value, 'A(type): name' = artifact, other beings by name"
     "- If multiple beings or artifacts are in the same cells, they are listed separated by |"
     "- The list includes only non-empty cells. If a coordinate is absent, assume that cell is empty and traversable."
     ""
     "You will receive also:"
     "    - the history of your past observations and selected actions"
     "    - a list of traits determining the way you act"
     ""
     "Note that:"
     (if food-mech?
         "- Energy\n    - You lose 1 energy at each turn, whatever you do, even if you stay still.\n    - When your energy reaches 0, you die.\n    - You can refill your energy by stepping in a cell containing food. Food gives energy equal to the food's value and then disappears."
         "")
     "- Time\n    - You have a set life span. Once your time reaches 0, you die.\n    - You lose 1 time unit at each turn. You cannot refill your time."
     ""
     "- Action Selection\n    - You must choose exactly one action per turn from the action list provided in the prompt.\n    - Action options may change over time and will always be specified in your per-step input."
     ""
     "- Communication\n    - At each step, you can decide if to send a broadcast message to entities in your field of view or not.\n    - Messages are plain text and incur no additional energy cost."
     (if mem?
         (format "- Internal memory :\n    - You produce INTERNAL MEMORY each step; it is returned to you next step.\n    - Use it to store a resume of your life up until that point or any other relevant information you wish to remember.\n    - Keep it concise to avoid exceeding the ~a token limit.\n    - Represent it in whatever structure you find useful (free text, lists, invented tags, micro-JSONs, diagrams-as-text, etc.)." internal-memory-size)
         "")
     (if art?
         (string-append
          "- Artifacts\n    - "
          (if inv?
              "To interact with an artifact, you must either share a cell with it or have it in your inventory.\n"
              "To interact with an artifact, you must share a cell with it.\n")
          "    - Upon co-location you will see passive effects (e.g., text content) and be offered valid interaction actions for that artifact.\n"
          "    - Artifacts are a persistent medium through which beings may express themselves, communicate across time, coordinate, preserve knowledge, establish traditions, or invent uses that are not described here.\n"
          "    - You decide what an artifact means and whether other beings' artifacts should be trusted, followed, changed, carried, shared, contested, or destroyed.")
         "")
     (if inv? "- Inventory\n    - List of the artifacts currently in your possession" "")
     ""
     ex-text))

  (string-join (filter (λ (s) (> (string-length s) 0)) lines) "\n"))

;; Format available-actions hash into prompt text
;; SPEC §4 cells hash is keyed by (cons dx dy); Racket's jsexpr->string only
;; accepts symbol keys and rejects cons cells, so we rebuild the observation
;; with symbol keys and a list-of-{offset, items} for cells.
(define (observation->string obs)
  (define cells (hash-ref obs 'cells))
  (define cells-as-list
    (for/list ([(offset items) (in-hash cells)])
      (hasheq 'offset (list (car offset) (cdr offset))
              'items items)))
  (jsexpr->string
   (hasheq 'cells cells-as-list
           'messages (hash-ref obs 'messages)
           'energy (hash-ref obs 'energy)
           'time-left (hash-ref obs 'time-left)
           'inventory (hash-ref obs 'inventory)
           'vision-radius (hash-ref obs 'vision-radius))))

;; Format available-actions hash into prompt text
(define (format-available-actions avail)
  (define lines
    (for/list ([k (in-list (sort (hash-keys avail) string<?))])
      (define schema (hash-ref avail k))
      (define desc (or (hash-ref schema 'description #f) (hash-ref schema "description" "")))
      (define params (or (hash-ref schema 'params #f) (hash-ref schema "params" (hash))))
      (define param-str
        (if (hash-empty? params)
            "  params: {}"
            (string-append
             "  params:\n"
             (string-join
              (for/list ([(pk pv) (in-hash params)])
                (format "    ~a: ~a" pk pv))
              "\n"))))
      (format "- ~a: ~a\n~a" k desc param-str)))
  (string-join lines "\n"))

;; Render the user prompt turn
(define (render-user-prompt #:history [history ""]
                            #:genome [genome ""]
                            #:observation [obs ""]
                            #:messages [messages ""]
                            #:energy [energy #f]
                            #:time [time-rem 0]
                            #:inventory [inventory #f]
                            #:memory [memory #f]
                            #:additional-info [additional-info ""]
                            #:available-actions [avail (hash)])
  (define action-keys (sort (hash-keys avail) string<?))
  (define action-keys-str (string-join action-keys ", "))
  (define formatted-actions (format-available-actions avail))

  (define sections
    (list
     history
     genome
     "=== Current State ==="
     (format "Observation:\n~a" obs)
     (format "Incoming messages:\n~a" messages)
     (if energy (format "Energy: ~a" energy) "")
     (format "Remaining time: ~a" time-rem)
     (if inventory (format "Inventory:\n~a" inventory) "")
     (if memory (format "Previous INTERNAL MEMORY:\n~a" memory) "")
     (if (> (string-length additional-info) 0) additional-info "")
     "=== Available Actions & Params ==="
     formatted-actions
     "=== Reply Format ==="
     "Please answer *exactly* in this json format (Do NOT include any other text outside of the JSON object):"
     "```json"
     "{"
     (format "    \"action\": \"<one of ~a>\"," action-keys-str)
     "    \"message\": \"<your broadcasted message, or leave blank>\","
     "    \"params\": {\"<parameter_name>\": \"<parameter_value>\"}"
     (if memory ",\n    \"internal_memory\": \"<information to remember next turn, within the internal-memory limit stated above>\"" "")
     "}"
     "```"))

  (string-join (filter (λ (s) (> (string-length s) 0)) sections) "\n\n"))
