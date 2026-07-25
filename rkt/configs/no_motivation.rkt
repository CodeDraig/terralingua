#lang terralingua

(experiment
 #:name "no_motivation"
 #:description "Ablation where agents have no exogenous motivation."
 #:max-ts 3000
 #:provider "openai"
 #:model "deepseek-ai/DeepSeek-R1-Distill-Qwen-32B"
 #:exogenous-motivation 'none
 #:genome 'ocean5
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
