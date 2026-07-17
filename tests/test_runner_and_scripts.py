import importlib
import subprocess
import sys
import tempfile
import types
import unittest
from datetime import datetime, timedelta
from pathlib import Path
from types import SimpleNamespace
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]

try:
    importlib.import_module("pygame")
except ModuleNotFoundError as error:
    if error.name != "pygame":
        raise
    sys.modules["pygame"] = types.ModuleType("pygame")

OpenGridWorld = importlib.import_module("core.environment.env").OpenGridWorld
runner_module = importlib.import_module("core.experiment.runner")
cli_module = importlib.import_module("core.experiment.cli")
config_module = importlib.import_module("core.experiment.config")
checkpoint_module = importlib.import_module("core.experiment.checkpoint")
AgentConfig = config_module.AgentConfig
EnvConfig = config_module.EnvConfig
ExperimentConfig = config_module.ExperimentConfig
RunConfig = config_module.RunConfig


class RunScriptTests(unittest.TestCase):
    def test_annotated_launcher_preserves_each_argument(self):
        command = """
python() { printf '<%s>\\n' "$@"; }
export -f python
bash run_experiment.sh
"""

        result = subprocess.run(
            ["bash", "-c", command],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        )

        arguments = result.stdout.splitlines()
        self.assertEqual(arguments[0], "<main.py>")
        self.assertIn("<--internal_memory_size>", arguments)
        self.assertIn("<--name_seed>", arguments)
        self.assertIn("<--reproduction_cost>", arguments)
        self.assertNotIn("< >", arguments)

    def test_paper_launchers_pin_legacy_display_names(self):
        for script in (ROOT / "paper_experiment_scripts").glob("*.sh"):
            with self.subTest(script=script.name):
                self.assertIn("--no-procedural_names", script.read_text())


class RunnerConfigurationTests(unittest.TestCase):
    def test_procedural_names_are_enabled_by_default_with_seed_zero(self):
        config = config_module.build_config({})

        self.assertTrue(config.agent.procedural_names)
        self.assertEqual(config.agent.name_seed, 0)

    def test_cli_accepts_name_seed_and_legacy_name_opt_out(self):
        with mock.patch.object(
            sys,
            "argv",
            ["main.py", "--name_seed", "1729", "--no-procedural_names"],
        ):
            config = config_module.build_config(cli_module.parse_args())

        self.assertFalse(config.agent.procedural_names)
        self.assertEqual(config.agent.name_seed, 1729)

    def test_legacy_checkpoint_parameters_preserve_legacy_names(self):
        with tempfile.TemporaryDirectory() as log_dir:
            params_path = Path(log_dir) / "params.json"
            params_path.write_text(
                '{"agent": {"model": "test"}, "env": {}, "run": {}}'
            )

            config = checkpoint_module.CheckpointManager(
                Path(log_dir)
            ).update_parameters()

        self.assertFalse(config.agent.procedural_names)

    def test_incomplete_checkpoint_outputs_are_rebuilt(self):
        runner = object.__new__(runner_module.SimulationRunner)
        runner.agents = {"being0": object()}
        runner.obs = {"being0": {"stale": True}}
        runner.infos = {"being0": {}}
        runner.env = SimpleNamespace(
            agent_registry={"being0": "text"},
            _observe_all=mock.Mock(return_value={"being0": {"fresh": True}}),
            _get_avail_actions=mock.Mock(return_value={"move": {}}),
        )

        runner._repair_incomplete_checkpoint_outputs()

        self.assertEqual(runner.obs, {"being0": {"fresh": True}})
        self.assertEqual(
            runner.infos["being0"]["available_actions"], {"move": {}}
        )

    def test_initial_agents_receive_configured_internal_memory_size(self):
        created_agents = []
        added_agents = []

        class FakeAgent:
            def __init__(self, **kwargs):
                created_agents.append(kwargs)

        class FakeEnvironment:
            agent_names = {}

            def add_agent(self, **kwargs):
                added_agents.append(kwargs)
                self.agent_names[kwargs["agent_tag"]] = kwargs["agent_name"]
                return None

            def restart_env(self, **_kwargs):
                return {"being0": {}}, {"being0": {}}

        with tempfile.TemporaryDirectory() as log_dir:
            runner = object.__new__(runner_module.SimulationRunner)
            runner.params = ExperimentConfig(
                agent=AgentConfig(internal_memory_size=37),
                env=EnvConfig(init_agents=1),
                run=RunConfig(save_root=log_dir),
            )
            runner.exp_logdir = Path(log_dir)
            runner.agents = {}
            runner._make_env = lambda: setattr(runner, "env", FakeEnvironment())

            with mock.patch.object(runner_module, "LLMAgent", FakeAgent):
                runner._init_state()

        self.assertEqual(created_agents[0]["internal_memory_size"], 37)
        self.assertEqual(created_agents[0]["agent_tag"], "being0")
        self.assertEqual(created_agents[0]["agent_name"], "Neraria")
        self.assertEqual(added_agents[0]["agent_tag"], "being0")
        self.assertEqual(added_agents[0]["agent_name"], "Neraria")

    def test_procedural_names_can_be_disabled_without_changing_tags(self):
        created_agents = []

        class FakeAgent:
            def __init__(self, **kwargs):
                created_agents.append(kwargs)

        class FakeEnvironment:
            agent_names = {}

            def add_agent(self, **kwargs):
                self.agent_names[kwargs["agent_tag"]] = kwargs["agent_name"]

            def restart_env(self, **_kwargs):
                return {"being0": {}}, {"being0": {}}

        with tempfile.TemporaryDirectory() as log_dir:
            runner = object.__new__(runner_module.SimulationRunner)
            runner.params = ExperimentConfig(
                agent=AgentConfig(procedural_names=False),
                env=EnvConfig(init_agents=1),
                run=RunConfig(save_root=log_dir),
            )
            runner.exp_logdir = Path(log_dir)
            runner.agents = {}
            runner._make_env = lambda: setattr(runner, "env", FakeEnvironment())

            with mock.patch.object(runner_module, "LLMAgent", FakeAgent):
                runner._init_state()

        self.assertEqual(created_agents[0]["agent_tag"], "being0")
        self.assertEqual(created_agents[0]["agent_name"], "being0")

    def test_reproduction_cannot_gift_negative_energy(self):
        with tempfile.TemporaryDirectory() as log_dir:
            environment = OpenGridWorld(
                grid_size=5,
                init_agent_energy=100,
                init_food=1,
                reproduction_cost=10,
                log_path=log_dir,
                verbose=0,
            )
            environment.add_agent(
                agent_tag="parent",
                agent_name="parent",
                agent_type="text",
                position=(2, 2),
            )
            environment.restart_env(agent_poses={"parent": (2, 2)})
            environment.food.clear()

            environment.step(
                {
                    "parent": {
                        "action": "reproduce",
                        "message": "",
                        "params": {"energy": -50, "name": "child"},
                    }
                }
            )

            self.assertEqual(environment.agent_energy["parent"], 89)
            self.assertEqual(environment.agent_energy["parent_0"], 99)
            environment.close()

    def test_video_encoder_is_skipped_when_video_is_disabled(self):
        class FakeEnvironment:
            def step(self, _actions):
                return {}, {}, {}, {}, {}

            def close(self):
                return None

        runner = object.__new__(runner_module.SimulationRunner)
        runner.params = SimpleNamespace(
            run=SimpleNamespace(
                max_ts=0,
                ckpt_interval=0,
                empty_countdown=20,
                max_parallel_workers=1,
                save_video=False,
                video_fps=10,
            ),
            env=SimpleNamespace(min_agents=0),
        )
        runner.start_ts = 0
        runner.last_refresh = datetime.now()
        runner.refresh_interval = timedelta(hours=1)
        runner.agents = {}
        runner.obs = {}
        runner.infos = {}
        runner.rewards = {}
        runner.dones = {}
        runner.terminate = False
        runner.env = FakeEnvironment()
        runner._render = mock.Mock()
        runner._save_checkpoint = mock.Mock()
        runner._handle_reproduction = mock.Mock()
        runner._cleanup_dead = mock.Mock()
        runner._respawn_if_needed = mock.Mock()

        fake_thread = mock.Mock()
        with (
            mock.patch.object(runner_module.signal, "signal"),
            mock.patch.object(
                runner_module.threading, "Thread", return_value=fake_thread
            ),
            mock.patch.object(runner_module, "create_video") as create_video,
        ):
            runner.run()

        create_video.assert_not_called()


if __name__ == "__main__":
    unittest.main()
