import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from core.agents.llm_agent import LLMAgent
from core.experiment import cli as cli_module
from core.experiment import config as config_module
from core.experiment import runner as runner_module
from core.genome.rpg_6 import Genome


class RPG6GenomeTests(unittest.TestCase):
    def test_c1_cli_and_runner_accept_rpg_6(self):
        with mock.patch.object(
            sys,
            "argv",
            ["main.py", "--provider", "anthropic", "--genome", "rpg_6"],
        ):
            config = config_module.build_config(cli_module.parse_args())

        runner = object.__new__(runner_module.SimulationRunner)
        runner.params = config

        self.assertEqual(config.agent.genome, "rpg_6")
        self.assertIs(runner._get_genome_cls(), Genome)

    def test_c2_has_exactly_the_six_rpg_attributes(self):
        genome = Genome()

        self.assertEqual(
            list(genome.as_dict()),
            [
                "strength",
                "dexterity",
                "constitution",
                "intelligence",
                "wisdom",
                "charisma",
            ],
        )

    def test_c3_random_generation_rolls_three_d6_per_attribute(self):
        rolls = [
            1,
            2,
            3,
            4,
            5,
            6,
            1,
            1,
            1,
            6,
            6,
            6,
            2,
            3,
            4,
            5,
            5,
            5,
        ]
        with mock.patch(
            "core.genome.rpg_6.random.randint", side_effect=rolls
        ) as randint:
            genome = Genome.random()

        self.assertEqual(
            genome.as_dict(),
            {
                "strength": 6,
                "dexterity": 15,
                "constitution": 3,
                "intelligence": 18,
                "wisdom": 9,
                "charisma": 15,
            },
        )
        self.assertEqual(randint.call_count, 18)
        randint.assert_has_calls([mock.call(1, 6)] * 18)

    def test_c3_rejects_non_integer_or_out_of_range_scores(self):
        for invalid in (2, 19):
            with self.subTest(value=invalid):
                with self.assertRaises(ValueError):
                    Genome(strength=invalid)

        with self.assertRaises(TypeError):
            Genome(strength=10.5)

    def test_c4_prompt_has_scores_and_roll_contract_without_added_meanings(self):
        prompt = Genome(
            strength=3,
            dexterity=4,
            constitution=5,
            intelligence=16,
            wisdom=17,
            charisma=18,
        ).as_string()

        self.assertEqual(
            prompt,
            """=== Your Traits ===
RPG attributes (each score is determined by 3d6; range 3-18)
  Strength value: 3
  Dexterity value: 4
  Constitution value: 5
  Intelligence value: 16
  Wisdom value: 17
  Charisma value: 18""",
        )

    def test_c5_round_trip_preserves_class_and_values(self):
        original = Genome(
            strength=3,
            dexterity=6,
            constitution=9,
            intelligence=12,
            wisdom=15,
            charisma=18,
        )

        restored = Genome().from_dict(original.as_dict())

        self.assertIsInstance(restored, Genome)
        self.assertEqual(restored, original)

    def test_c5_agent_checkpoint_restores_rpg_6_class_and_values(self):
        original = Genome(
            strength=3,
            dexterity=6,
            constitution=9,
            intelligence=12,
            wisdom=15,
            charisma=18,
        )
        with tempfile.TemporaryDirectory() as log_dir:
            agent = LLMAgent(
                agent_name="RPG Agent",
                agent_tag="being0",
                genome=original,
                log_dir=log_dir,
            )
            state = agent.get_state_ckpt()
            restored = LLMAgent(
                agent_name="Temporary",
                agent_tag="temporary",
                genome=Genome(),
                log_dir=log_dir,
            )
            restored.set_state_ckpt(state)

            self.assertEqual(
                state["genome_class"], "core.genome.rpg_6:Genome"
            )
            self.assertIsInstance(restored.genome, Genome)
            self.assertEqual(restored.genome, original)
            self.assertEqual(
                Path(restored.logger.log_dir), Path(log_dir) / "agent_logs"
            )
            agent.close()
            restored.close()

    def test_c6_mutation_uses_existing_integer_inheritance_convention(self):
        parent = Genome(
            strength=3,
            dexterity=18,
            constitution=10,
            intelligence=10,
            wisdom=10,
            charisma=10,
        )
        with (
            mock.patch(
                "core.genome.rpg_6.random.random",
                side_effect=[0.0, 0.0, 0.0, 1.0, 1.0, 1.0],
            ),
            mock.patch(
                "core.genome.rpg_6.random.choice",
                side_effect=[-1, 1, 1],
            ),
        ):
            child = parent.mutate()

        self.assertEqual(parent.strength, 3)
        self.assertEqual(child.strength, 3)
        self.assertEqual(child.dexterity, 18)
        self.assertEqual(child.constitution, 11)
        self.assertEqual(child.intelligence, 10)
        self.assertEqual(child.wisdom, 10)
        self.assertEqual(child.charisma, 10)


if __name__ == "__main__":
    unittest.main()
