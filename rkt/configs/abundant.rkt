#lang terralingua

(experiment
 #:name "abundant_resources"
 #:description "Ablation extending the temporal context of the agents and providing them with more resources."
 #:max-ts 3000
 #:provider "openai"
 #:model "deepseek-ai/DeepSeek-R1-Distill-Qwen-32B"
 #:exogenous-motivation 'base
 #:genome 'ocean5
 #:max-history 20
 #:grid-size 50
 #:init-agents 20
 #:init-agent-energy 50
 #:food-zones 0
 #:agent-lifespan 100
 #:vision-radius 6
 #:dead-agent-food 'single
 #:artifact-creation-cost 0
 #:reproduction-cost 50)
