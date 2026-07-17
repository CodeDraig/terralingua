#!/bin/bash

args=(
    # Experiment
    --exp_name "experiment_name"
    --exp_description "Experiment description"
    --max_ts 300

    # Agent LLM
    --model "claude-haiku-4-5"

    # Agents
    --agents_name_prefix "being" # Stable tags: being0, being1, etc.
    --name_seed 0 # Seed for procedural names shown to agents.
    --exogenous_motivation "base" # One of: base, creative, none.
    --genome "ocean_5" # One of: ocean_5, no_traits.
    --max_history 1 # Past timesteps included in observations.
    --internal_memory_size 150 # Internal-memory token limit.
    --use_internal_memory
    --use_inventory
    --no-use_colors

    # Environment
    --grid_size 50
    --init_agents 10
    --init_human_agents 0
    --min_agents 0
    --init_agent_energy 50
    --init_food 100
    --food_zones 1
    --food_mechanism
    --agent_lifespan 100
    --vision_radius 6
    --dead_agent_food "single" # One of: single, none, area.
    --artifact_creation
    --artifact_creation_cost 0
    --no-inert_artifacts
    --reproduction_allowed
    --reproduction_cost 50
)

python main.py "${args[@]}"
