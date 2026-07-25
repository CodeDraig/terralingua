#lang racket

;;; #lang terralingua — Language Expander & Experiment Macro

(require (for-syntax syntax/parse racket/base)
         "../runner/loop.rkt")

(provide (all-from-out racket)
         experiment
         run-simulation)

(define-syntax (experiment stx)
  (syntax-parse stx
    [(_ (~seq kw:keyword val:expr) ...)
     (define kw-table
       (for/hash ([k (in-list (syntax->datum #'(kw ...)))]
                  [v (in-list (syntax->list #'(val ...)))])
         (values k v)))

     (define (get-val kw-name default-expr)
       (hash-ref kw-table kw-name default-expr))

     #`(begin
         (define config
           (hasheq
            'exp-name #,(get-val '#:name #'"experiment")
            'exp-description #,(get-val '#:description #'"" )
            'max-ts #,(get-val '#:max-ts #'100)
            'provider #,(get-val '#:provider #'"openai")
            'model #,(get-val '#:model #'"gpt-4o-mini")
            'openai-base-url #,(get-val '#:openai-base-url #'#f)
            'agents-name-prefix #,(get-val '#:agents-name-prefix #'"Being")
            'name-seed #,(get-val '#:name-seed #'0)
            'exogenous-motivation #,(get-val '#:exogenous-motivation #''base)
            'genome #,(get-val '#:genome #''ocean5)
            'max-history #,(get-val '#:max-history #'1)
            'internal-memory-size #,(get-val '#:internal-memory-size #'150)
            'use-internal-memory #,(get-val '#:use-internal-memory #'#t)
            'use-inventory #,(get-val '#:use-inventory #'#t)
            'use-colors #,(get-val '#:use-colors #'#f)
            'grid-size #,(get-val '#:grid-size #'50)
            'init-agents #,(get-val '#:init-agents #'20)
            'min-agents #,(get-val '#:min-agents #'2)
            'init-agent-energy #,(get-val '#:init-agent-energy #'50)
            'init-food #,(get-val '#:init-food #'100)
            'max-food-value #,(get-val '#:max-food-value #'10.0)
            'food-zones #,(get-val '#:food-zones #'1)
            'food-mechanism #,(get-val '#:food-mechanism #'#t)
            'static-food #,(get-val '#:static-food #'#f)
            'food-decay-rate #,(get-val '#:food-decay-rate #'0.05)
            'food-decay-amount #,(get-val '#:food-decay-amount #'1.0)
            'food-spawn-rate #,(get-val '#:food-spawn-rate #'1)
            'agent-lifespan #,(get-val '#:agent-lifespan #'100)
            'vision-radius #,(get-val '#:vision-radius #'6)
            'dead-agent-food #,(get-val '#:dead-agent-food #''single)
            'artifact-creation #,(get-val '#:artifact-creation #'#t)
            'artifact-creation-cost #,(get-val '#:artifact-creation-cost #'0)
            'reproduction-cost #,(get-val '#:reproduction-cost #'50)
            'reproduction-allowed #,(get-val '#:reproduction-allowed #'#t)
            'inert-artifacts #,(get-val '#:inert-artifacts #'#f)
            'ckpt-interval #,(get-val '#:ckpt-interval #'10)))
         (provide config)
         (module+ main
           (run-simulation config)))]))
