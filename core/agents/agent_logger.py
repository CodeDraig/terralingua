import json
import os
import threading
from collections import Counter
from datetime import datetime
from pathlib import Path
from typing import Any, ClassVar, Dict

from core.utils import ROOT


class AgentLogger:
    """Persist agent traces and run-scoped decision-attempt diagnostics."""

    _retry_locks: ClassVar[dict[Path, threading.Lock]] = {}
    _retry_locks_guard: ClassVar[threading.Lock] = threading.Lock()

    def __init__(self, log_dir: Path | str | None, agent_tag: str = "agent"):
        if log_dir is None:
            self.log_dir = (
                ROOT / "logs" / datetime.now().strftime("%Y%m%d_%H%M") / "agent_logs"
            )
        else:
            self.log_dir = Path(log_dir)

        os.makedirs(self.log_dir, exist_ok=True)
        self.filename = self.log_dir / f"{agent_tag}.jsonl"
        self.data_dict: Dict[str, Dict[str, Any]] = {}

    def save_genome(self, agent_tag: str, genome: dict):
        genome_filename = self.log_dir / f"{agent_tag}_genome.json"
        with open(genome_filename, "w") as f:
            json.dump(genome, f, indent=4)

    def log(
        self,
        agent_name: str,
        agent_tag: str,
        observation: dict,
        available_actions: dict,
        action: dict,
        time: str,
        internal_memory: str,
        input_prompt: str,
    ):
        """
        Record the agent’s observation and action, both in a live file and in-memory dictionary.
        """
        observation["observation"] = {
            str(k): v for k, v in observation["observation"].items()
        }

        record = {
            "timestamp": time,
            "agent": agent_name,
            "agent_tag": agent_tag,
            "action": action,
            "observation": observation,
            "internal_memory": internal_memory,
            "available_actions": available_actions,
            "input_prompt": input_prompt,
        }

        # Save line to file
        with open(self.filename, "a") as f:
            f.write(json.dumps(record) + "\n")

        # Save to internal dict
        self.data_dict[time] = {
            "agent": agent_name,
            "agent_tag": agent_tag,
            "action": action,
            "internal_memory": internal_memory,
            "available_actions": available_actions,
            "observation": observation,
            "input_prompt": input_prompt,
        }

    @classmethod
    def _retry_lock(cls, filename: Path) -> threading.Lock:
        """Return the process-local lock for a shared run diagnostic file."""
        filename = filename.resolve()
        with cls._retry_locks_guard:
            if filename not in cls._retry_locks:
                cls._retry_locks[filename] = threading.Lock()
            return cls._retry_locks[filename]

    def log_retry(self, **event: Any):
        """Append one decision-attempt diagnostic without recording model text.

        Agents decide concurrently, so this writer is shared and locked at the
        experiment level.  The event remains intentionally small: response
        content is not duplicated in the diagnostic ledger.
        """
        filename = self.log_dir / "retry_events.jsonl"
        record = {
            "logged_at": datetime.now().astimezone().isoformat(),
            **event,
        }
        with self._retry_lock(filename):
            with open(filename, "a") as f:
                f.write(json.dumps(record) + "\n")

    @classmethod
    def write_retry_summary(cls, log_dir: Path | str) -> dict[str, Any]:
        """Summarize the retry ledger into a stable, run-level JSON artifact."""
        log_dir = Path(log_dir)
        events_path = log_dir / "retry_events.jsonl"
        events: list[dict[str, Any]] = []
        malformed_lines = 0

        if events_path.exists():
            with cls._retry_lock(events_path):
                with open(events_path) as f:
                    for line in f:
                        if not line.strip():
                            continue
                        try:
                            events.append(json.loads(line))
                        except json.JSONDecodeError:
                            malformed_lines += 1

        response_events = [
            event for event in events if event.get("layer") == "agent_response"
        ]
        outcome_counts = Counter(
            str(event.get("outcome", "unknown")) for event in response_events
        )
        retry_events = [
            event
            for event in events
            if event.get("layer") in {"agent_selection", "outer_selection"}
            and event.get("outcome") in {"retry", "fallback"}
        ]
        malformed_or_empty = outcome_counts["malformed"] + outcome_counts["empty"]
        api_attempts = len(response_events)

        summary = {
            "schema_version": 1,
            "generated_at": datetime.now().astimezone().isoformat(),
            "api_attempts": api_attempts,
            "accepted_responses": outcome_counts["accepted"],
            "malformed_responses": outcome_counts["malformed"],
            "empty_responses": outcome_counts["empty"],
            "incomplete_responses": outcome_counts["incomplete"],
            "rejected_responses": outcome_counts["rejected"],
            "transport_failures": outcome_counts["transport_error"],
            "malformed_or_empty_responses": malformed_or_empty,
            "malformed_or_empty_rate": (
                malformed_or_empty / api_attempts if api_attempts else 0.0
            ),
            "api_retry_attempts": api_attempts - outcome_counts["accepted"],
            "selection_retry_or_fallback_events": len(retry_events),
            "fallbacks": sum(
                event.get("outcome") == "fallback" for event in retry_events
            ),
            "response_outcomes": dict(sorted(outcome_counts.items())),
            "invalid_ledger_lines": malformed_lines,
        }
        summary_path = log_dir.parent / "retry_summary.json"
        with open(summary_path, "w") as f:
            json.dump(summary, f, indent=2)
            f.write("\n")
        return summary

    def close(self):
        return
