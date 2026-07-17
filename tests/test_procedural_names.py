import unittest

from core.agents.procedural_names import procedural_name, procedural_names


class ProceduralNameTests(unittest.TestCase):
    def test_default_seed_has_stable_roster(self):
        self.assertEqual(
            procedural_names(0, 5),
            ["Neraria", "Aena", "Lumaven", "Selawen", "Avava"],
        )

    def test_same_seed_returns_same_unique_names(self):
        first = procedural_names(1729, 1200)
        second = procedural_names(1729, 1200)

        self.assertEqual(first, second)
        self.assertEqual(len(first), len(set(first)))

    def test_different_seed_changes_roster(self):
        self.assertNotEqual(procedural_names(1, 20), procedural_names(2, 20))

    def test_large_indices_remain_deterministic_and_unique(self):
        names = [procedural_name(7, index) for index in range(2100)]

        self.assertEqual(len(names), len(set(names)))
        self.assertEqual(names[2048], f"{names[0]}3")

    def test_negative_inputs_are_rejected(self):
        with self.assertRaisesRegex(ValueError, "index"):
            procedural_name(0, -1)
        with self.assertRaisesRegex(ValueError, "count"):
            procedural_names(0, -1)


if __name__ == "__main__":
    unittest.main()
