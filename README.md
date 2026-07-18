# TerraLingua

**Paper:** [Link](https://www.researchgate.net/publication/402263491_TerraLingua_Emergence_and_Analysis_of_Open-endedness_in_LLM_Ecologies) - [ArXiv](https://arxiv.org/abs/2603.16910)

**Dataset:** https://huggingface.co/datasets/GPaolo/TerraLingua

**Dataset dashboard:** https://aianthropology.decisionai.ml/

![TerraLingua agents](assets/environment.gif)

A multi-agent simulation framework for studying emergent behavior, artifact creation, and cultural evolution.

LLM-powered agents (Claude or other models) interact in a shared 2D grid environment — foraging for resources, creating text artifacts, reproducing, and communicating — enabling research into how language-using agents develop social structure and culture over time.

After each experiment, the **AI Anthropologist** — itself an LLM agent — analyzes the simulation logs to annotate agent behaviors, infer group dynamics, classify artifacts, and trace cultural lineages, providing a qualitative and quantitative account of what emerged.

An overview of the TerraLingua system and of the AI-Anthropologist is shown in the figure below.

![TerraLingua and the AI Anthropologist](assets/whole.png)


## Installation

Requires **Python 3.13+**.

**Using venv:**

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

**Using conda:**

```bash
conda create -n terralingua python=3.13
conda activate terralingua
pip install -r requirements.txt
```

Copy `.env.example` to `.env` and fill in your API key(s):

```bash
cp .env.example .env
```

## Running Experiments

Run directly with `main.py` using CLI flags:

```bash
python main.py \
  --provider anthropic \
  --model claude-haiku-4-5 \
  --exp_name my_experiment \
  --init_agents 10 \
  --max_ts 200
```

Or use `run_experiment.sh`, an annotated template covering the common options:

```bash
bash run_experiment.sh
```

Logs are written to `logs/<exp_name>/`.

Agents receive deterministic procedural display names by default while retaining
stable tags such as `being0` for log files, checkpoints, and analysis. The
default naming seed is `0`; use another integer to create a different
reproducible roster:

```bash
python main.py --provider anthropic --name_seed 1729
```

Because names are visible to agents, treat the seed as part of the experimental
condition. Pass `--no-procedural_names` to use legacy `being0`, `being1`, …
display names.

### Paper experiment configurations

The `paper_experiment_scripts/` folder retains the environmental configurations
used for the paper experiments while using the current exact-model-ID transport.
Run scripts from the project root after pointing `OPENAI_BASE_URL` at the model
server:

```bash
export OPENAI_BASE_URL=http://127.0.0.1:9000/v1
bash paper_experiment_scripts/run_core.sh
```

## Testing

Run the local unit suite from the project root:

```bash
python -m unittest discover -v
```

## Model configuration

TerraLingua has no model registry or aliases. Pass the exact server-side model
ID with `--model` and select its API with `--provider openai` or
`--provider anthropic`. The provider is required for new runs; resumed runs
recover it from their saved parameters. Checkpoints created before provider
metadata was recorded cannot be resumed.

### Local models (vLLM)

Local models require a running [vLLM](https://github.com/vllm-project/vllm)
server that exposes an OpenAI Responses-compatible `/v1/responses` endpoint.
TerraLingua does not fall back to `/v1/chat/completions`; servers that only
implement Chat Completions must be upgraded or reconfigured:

```bash
vllm serve Qwen/Qwen3-32B --port 9000
python main.py \
  --provider openai \
  --model Qwen/Qwen3-32B \
  --openai_base_url http://127.0.0.1:9000/v1
```

### Custom OpenAI-compatible endpoints

OpenAI models and compatible servers use the **Responses API** exclusively.
Requests are stateless (`store=false`); TerraLingua retains agent history and
checkpoints locally. Supply the server's `/v1` base URL:

```bash
python main.py \
  --provider openai \
  --model vendor/strange-model \
  --openai_base_url http://127.0.0.1:8080/v1
```

Set `OPENAI_API_KEY` in `.env` to the credential expected by the server. The
base URL can instead be set with `OPENAI_BASE_URL`. API keys remain
environment-only and are not written to experiment parameters or checkpoints.
Custom endpoints must expose `/v1/responses`; Chat Completions-only endpoints
are not supported.

## Data Analysis

Analysis is performed by the **AI Anthropologist**, a post-hoc LLM-based framework that annotates agent behaviors, infers group dynamics, classifies artifacts, and traces cultural lineages. See [`analysis_scripts/AI_ANTHROPOLOGIST.md`](analysis_scripts/AI_ANTHROPOLOGIST.md) for a detailed description of the pipeline.

Scripts follow a numbered order and must be run from the **project root** (they import from `core` and `analysis_scripts` as packages):

| Script | Description |
|---|---|
| `001_llm_agent_analyser.py` | Annotate agent logs with LLM-generated behavior labels |
| `002_make_graph.py` | Build interaction graphs and compute network metrics |
| `003_llm_group_analyser.py` | Group-level behavioral analysis |
| `004_artifact_analysis.py` | Compute artifact complexity metrics |
| `005_artifact_classification.py` | Classify artifacts into behavioral categories |
| `006_artifact_philogeny.py` | Analyze artifact genealogy and conceptual ancestry |

```bash
python analysis_scripts/001_llm_agent_analyser.py
```

## Data Visualization

Notebooks in `analysis_scripts/notebooks/` mirror the analysis pipeline:

| Notebook | Description |
|---|---|
| `n000_general_stats.ipynb` | Overall experiment statistics |
| `n001_llm_agent_analyser.ipynb` | Per-agent behavior visualization |
| `n002_graph_analysis.ipynb` | Interaction network plots |
| `n003_llm_group_analysis.ipynb` | Group dynamics |
| `n004_artifact_analysis.ipynb` | Artifact complexity over time |
| `n005_artifact_categories.ipynb` | Classification results |
| `n006_artifact_phylogeny.ipynb` | Artifact lineage trees |
| `n007_interactive_phylogeny.ipynb` | Interactive phylogeny explorer |

```bash
jupyter notebook analysis_scripts/notebooks/
```

## Citation

If you use TerraLingua in your research, please cite:

```bibtex
@techreport{paolo26terralingua,
title = "TerraLingua: Emergence and Analysis of Open-Endedness in LLM Ecologies",
author = "Giuseppe Paolo and Jamieson Warner and Hormoz Shahrzad and Babak Hodjat and Risto Miikkulainen and Elliot Meyerson",
year = 2026,
month = jan,
institution = "Cognizant AI Lab",
url = "https://arxiv.org/abs/2603.16910",
doi = "10.48550/arXiv.2603.16910",
number = "2026-01",
}
```
