#lang terralingua

(experiment
 #:name "no_personality"
 #:description "Ablation where agents have no personality traits."
 #:max-ts 3000
 #:provider "openai"
 #:model "deepseek-ai/DeepSeek-R1-Distill-Qwen-32B"
 #:exogenous-motivation 'base
 #:genome 'notraits
 #:max-history 1
 #:grid-size 50
 #:init-agents 20
 #:init-agent-energy 50
 #:food-zones 1
 #:agent-lifespan 100
 #:vision-radius 6
 #:dead-agent-food 'single
 #:artifact-creation-cost 0
 #:reproduction-cost 50)
