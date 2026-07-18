#!/bin/bash

python main.py \
    \
    `# Experiment` \
    --exp_name              "creative" \
    --exp_description       "Ablation encouraging agents to be more creative." \
    --max_ts                3000 \
    \
    `# Agent LLM` \
    --provider              "openai" \
    --model                 "deepseek-ai/DeepSeek-R1-Distill-Qwen-32B" \
    \
    `# Agents` \
    --exogenous_motivation  "creative" \
    --genome                "ocean_5" \
    --max_history           1 \
    --no-procedural_names \
    \
    `# Environment` \
    --grid_size             50 \
    --init_agents           20 \
    --init_agent_energy     50 \
    --food_zones            1 \
    --agent_lifespan        100 \
    --vision_radius         6 \
    --dead_agent_food       "single" \
    --artifact_creation_cost 0 \
    --reproduction_cost     50 \
    \
    `# Output` \
    --save_video \
    --video_fps             10 \
    # --save_root           "/path/to/output"   # defaults to project root
