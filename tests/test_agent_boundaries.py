import tempfile
import unittest
from json import loads
from pathlib import Path

from core.agents.llm_agent import LLMAgent
from core.agents.procedural_names import procedural_name
from core.agents.prompt_templates import AGENT_PROMPT, ERROR_MSG, SYS_PROMPT
from core.environment.actions import ACTION_TEXT
from core.environment.artifact import ArtifactCreationError, TextArtifact
from core.utils.llm_client import Response, ResponseRejectedError


class _CharacterEncoder:
    def encode(self, value):
        return list(value)

    def decode(self, tokens):
        return "".join(tokens)


class _Logger:
    def __init__(self, log_dir):
        self.log_dir = Path(log_dir)

    def log(self, **_kwargs):
        return None


class _Client:
    def get_response(self, messages, request_params):
        return Response(content="{}", input_tokens=1, output_tokens=1)


class _SequenceClient:
    def __init__(self):
        self.calls = []

    def get_response(self, messages, request_params):
        self.calls.append([message.copy() for message in messages])
        return Response(content="{}", input_tokens=1, output_tokens=1)


class _IncompleteThenValidClient:
    def __init__(self, incomplete_count, input_tokens=5, output_tokens=3):
        self.incomplete_count = incomplete_count
        self.input_tokens = input_tokens
        self.output_tokens = output_tokens
        self.calls = 0

    def get_response(self, messages, request_params):
        self.calls += 1
        if self.calls <= self.incomplete_count:
            raise ResponseRejectedError(
                "OpenAI Responses request ended with status 'incomplete': "
                "max_output_tokens",
                input_tokens=self.input_tokens,
                output_tokens=self.output_tokens,
            )
        return Response(content="valid", input_tokens=1, output_tokens=1)


class AgentBoundaryTests(unittest.TestCase):
    def test_procedural_display_name_is_rendered_into_system_prompt(self):
        with tempfile.TemporaryDirectory() as log_dir:
            display_name = procedural_name(0, 0)
            agent = LLMAgent(
                agent_name=display_name,
                agent_tag="being0",
                log_dir=log_dir,
            )

            self.assertIn(f"You are {display_name}", agent.system_prompt)
            self.assertNotIn("You are being0", agent.system_prompt)
            agent.close()

    def test_artifact_prompt_encourages_open_ended_culture_with_real_boundaries(self):
        system_prompt = SYS_PROMPT.render(
            agent_name="test",
            short_obs_descr="an observation",
            detailed_obs_descr="details",
            obs_style="list",
            food_mechanism=True,
            use_internal_memory=True,
            internal_memory_size=150,
            use_inventory=True,
            artifact_creation=True,
            exogenous_motivation="",
        )
        artifact_action = ACTION_TEXT["create_artifact"]["params"]

        self.assertIn("persistent medium", system_prompt)
        self.assertIn("establish traditions", system_prompt)
        self.assertIn("invent uses that are not described here", system_prompt)
        self.assertIn("create, preserve, and transmit culture", artifact_action["type"])
        self.assertIn("These examples are not limits", artifact_action["payload"])
        self.assertIn("does not execute", artifact_action["payload"])
        self.assertIn("Maximum 500 tokens", artifact_action["payload"])
        self.assertNotIn("code snippet", artifact_action["payload"])

    def test_agent_response_examples_are_valid_json(self):
        for template, arguments in (
            (
                AGENT_PROMPT,
                {"action_keys": "move", "use_internal_memory": True},
            ),
            (
                ERROR_MSG,
                {
                    "action_keys": ["move"],
                    "error": "invalid",
                    "use_internal_memory": True,
                },
            ),
            (
                ERROR_MSG,
                {
                    "action_keys": ["move"],
                    "error": "invalid",
                    "use_internal_memory": False,
                },
            ),
        ):
            rendered = template.render(**arguments)
            example = rendered.split("```json", 1)[1].split("```", 1)[0]
            loads(example)

    def test_grid_with_directions_includes_direction_labels(self):
        agent = object.__new__(LLMAgent)

        rendered = agent._format_grid_with_directions({(0, 0): ["self"]}, 0)

        for label in ("up", "down", "left", "right", "self"):
            self.assertIn(label, rendered)

    def test_internal_memory_is_truncated_to_configured_limit(self):
        agent = object.__new__(LLMAgent)
        agent.internal_memory_size = 3
        agent.internal_memory_encoder = _CharacterEncoder()

        self.assertEqual(agent.validate_internal_memory("abcde"), "cde")

    def test_zero_internal_memory_limit_discards_memory(self):
        agent = object.__new__(LLMAgent)
        agent.internal_memory_size = 0
        agent.internal_memory_encoder = _CharacterEncoder()

        self.assertEqual(agent.validate_internal_memory("abcde"), "")

    def test_checkpoint_round_trip_restores_memory_limit_and_prompt(self):
        with tempfile.TemporaryDirectory() as log_dir:
            original = LLMAgent(
                agent_name="current",
                agent_tag="current",
                log_dir=log_dir,
                internal_memory_size=23,
            )
            checkpoint = original.get_state_ckpt()

            resumed = LLMAgent(
                agent_name="current",
                agent_tag="current",
                log_dir=log_dir,
                internal_memory_size=99,
            )
            resumed.set_state_ckpt(checkpoint)

            self.assertEqual(resumed.agent_name, "current")
            self.assertEqual(resumed.internal_memory_size, 23)
            self.assertIn("You are current", resumed.system_prompt)
            self.assertIn("23 token limit", resumed.system_prompt)
            self.assertNotIn("99 token limit", resumed.system_prompt)
            original.close()
            resumed.close()

    def test_c1_legacy_checkpoint_uses_configured_memory_limit(self):
        with tempfile.TemporaryDirectory() as log_dir:
            original = LLMAgent(
                agent_name="legacy",
                agent_tag="legacy",
                log_dir=log_dir,
                internal_memory_size=23,
            )
            checkpoint = original.get_state_ckpt()
            checkpoint.pop("internal_memory_size")

            resumed = LLMAgent(
                agent_name="legacy",
                agent_tag="legacy",
                log_dir=log_dir,
                internal_memory_size=23,
            )
            resumed.set_state_ckpt(checkpoint)

            self.assertEqual(resumed.internal_memory_size, 23)
            self.assertIn("23 token limit", resumed.system_prompt)
            original.close()
            resumed.close()

    def test_select_action_does_not_mutate_shared_request_parameters(self):
        with tempfile.TemporaryDirectory() as log_dir:
            agent = object.__new__(LLMAgent)
            agent.agent_name = "test"
            agent.agent_tag = "test"
            agent.internal_memory = ""
            agent.history = []
            agent.max_history = 0
            agent.verbose = 0
            agent.system_prompt = "system"
            agent.logger = _Logger(log_dir)
            agent._format_observation = lambda _obs: {}
            agent._make_prompt = lambda **_kwargs: "prompt"
            agent._parse_response = lambda _text, available_actions: (
                "move",
                "",
                {"direction": "stay"},
                "",
                None,
            )
            request_params = {"model": "test", "post_prompt": "stay concise"}

            agent.select_action(
                obs={},
                available_actions={"move": {"params": {"direction": ""}}},
                reward=0,
                info=None,
                time=0,
                request_params=request_params,
                client=_Client(),
            )

            self.assertEqual(
                request_params, {"model": "test", "post_prompt": "stay concise"}
            )
            self.assertEqual(agent.history, [])

    def test_retry_keeps_memory_field_when_enabled_but_current_memory_is_empty(self):
        with tempfile.TemporaryDirectory() as log_dir:
            agent = object.__new__(LLMAgent)
            agent.agent_name = "test"
            agent.agent_tag = "test"
            agent.internal_memory = ""
            agent.use_internal_memory = True
            agent.history = []
            agent.max_history = 0
            agent.verbose = 0
            agent.system_prompt = "system"
            agent.logger = _Logger(log_dir)
            agent._format_observation = lambda _obs: {}
            agent._make_prompt = lambda **_kwargs: "prompt"
            outcomes = iter(
                [
                    ValueError("bad response"),
                    ("move", "", {"direction": "stay"}, "remember", None),
                ]
            )

            def parse_response(_text, available_actions):
                outcome = next(outcomes)
                if isinstance(outcome, Exception):
                    raise outcome
                return outcome

            agent._parse_response = parse_response
            client = _SequenceClient()

            agent.select_action(
                obs={},
                available_actions={"move": {"params": {"direction": ""}}},
                reward=0,
                info=None,
                time=0,
                request_params={"model": "test"},
                client=client,
            )

            retry_prompt = client.calls[1][-1]["content"]
            self.assertIn('"internal_memory"', retry_prompt)

    def test_incomplete_response_is_retried_inside_agent_attempt_loop(self):
        with tempfile.TemporaryDirectory() as log_dir:
            agent = self._stub_agent(log_dir)
            client = _IncompleteThenValidClient(incomplete_count=1)

            action = agent.select_action(
                obs={},
                available_actions={"move": {"params": {"direction": ""}}},
                reward=0,
                info=None,
                time=0,
                request_params={"model": "test"},
                client=client,
                max_attempts=2,
            )

            self.assertEqual(client.calls, 2)
            self.assertEqual(action["params"], {"direction": "up"})

    def test_c2_rejected_response_usage_is_included_in_agent_token_log(self):
        with tempfile.TemporaryDirectory() as log_dir:
            agent = self._stub_agent(log_dir)
            client = _IncompleteThenValidClient(incomplete_count=1)

            agent.select_action(
                obs={},
                available_actions={"move": {"params": {"direction": ""}}},
                reward=0,
                info=None,
                time=0,
                request_params={"model": "test"},
                client=client,
                max_attempts=2,
            )

            token_log = loads(
                (Path(log_dir) / "token_counts.jsonl").read_text().strip()
            )
            self.assertEqual(token_log["total_input_tokens"], 6)
            self.assertEqual(token_log["total_output_tokens"], 4)

    def test_incomplete_response_exhaustion_falls_back_to_stay(self):
        with tempfile.TemporaryDirectory() as log_dir:
            agent = self._stub_agent(log_dir)
            client = _IncompleteThenValidClient(incomplete_count=2)

            action = agent.select_action(
                obs={},
                available_actions={"move": {"params": {"direction": ""}}},
                reward=0,
                info=None,
                time=0,
                request_params={"model": "test"},
                client=client,
                max_attempts=2,
            )

            self.assertEqual(client.calls, 2)
            self.assertEqual(action["params"], {"direction": "stay"})

    def _stub_agent(self, log_dir):
        agent = object.__new__(LLMAgent)
        agent.agent_name = "test"
        agent.agent_tag = "test"
        agent.internal_memory = ""
        agent.history = []
        agent.max_history = 0
        agent.verbose = 0
        agent.system_prompt = "system"
        agent.logger = _Logger(log_dir)
        agent._format_observation = lambda _obs: {}
        agent._make_prompt = lambda **_kwargs: "prompt"
        agent._parse_response = lambda _text, available_actions: (
            "move",
            "",
            {"direction": "up"},
            "",
            None,
        )
        return agent


class TextArtifactTests(unittest.TestCase):
    def test_non_string_payload_is_rejected(self):
        with self.assertRaises(ArtifactCreationError):
            TextArtifact(
                name="invalid",
                payload=123,
                lifespan=1,
                pose=(0, 0),
                creator="agent",
                creation_time=0,
            )

    def test_string_lifespan_is_normalized_when_artifact_is_modified(self):
        artifact = TextArtifact(
            name="marker",
            payload="original",
            lifespan=5,
            pose=(0, 0),
            creator="agent",
            creation_time=0,
        )

        result = artifact.interact(
            agent_name="agent",
            action="modify_artifact_marker",
            params={"payload": "updated", "lifespan": "-1"},
            timestamp=1,
        )

        self.assertEqual(result, "Artifact marker updated")
        self.assertEqual(artifact.lifespan, float("inf"))
        self.assertEqual(artifact.remaining_time, float("inf"))

    def test_string_remaining_time_is_normalized_when_artifact_is_loaded(self):
        artifact = TextArtifact.deserialize(
            {
                "name": "marker",
                "art_type": "text",
                "payload": "updated",
                "lifespan": "-1",
                "pose": [0, 0],
                "creator_tag": "agent",
                "users_tag": {},
                "creation_time": 0,
                "past_versions": [],
                "version": 1,
                "version_creation_time": 1,
                "remaining_time": "-1",
            }
        )

        self.assertEqual(artifact.lifespan, float("inf"))
        self.assertEqual(artifact.remaining_time, float("inf"))

    def test_invalid_modified_lifespan_does_not_partially_update_artifact(self):
        artifact = TextArtifact(
            name="marker",
            payload="original",
            lifespan=5,
            pose=(0, 0),
            creator="agent",
            creation_time=0,
        )

        result = artifact.interact(
            agent_name="agent",
            action="modify_artifact_marker",
            params={"payload": "updated", "lifespan": "forever"},
            timestamp=1,
        )

        self.assertIn("lifespan must be an integer", result)
        self.assertEqual(artifact.payload, "original")
        self.assertEqual(artifact.version, 0)
        self.assertEqual(artifact.past_versions, [])


if __name__ == "__main__":
    unittest.main()
