import unittest
from HighPF import highest_prime_factor


class TestHighestPrimeFactor(unittest.TestCase):
    def test_small_numbers(self):
        self.assertEqual(highest_prime_factor(2), 2)
        self.assertEqual(highest_prime_factor(3), 3)
        self.assertEqual(highest_prime_factor(4), 2)
        self.assertEqual(highest_prime_factor(6), 3)
        self.assertEqual(highest_prime_factor(15), 5)

    def test_prime_number(self):
        self.assertEqual(highest_prime_factor(97), 97)

    def test_power_of_prime(self):
        self.assertEqual(highest_prime_factor(2**10), 2)
        self.assertEqual(highest_prime_factor(3**7), 3)

    def test_mixed_composite(self):
        self.assertEqual(highest_prime_factor(13195), 29)  # Project Euler example

    def test_large_composite(self):
        # 600851475143 = 71 * 839 * 1471 * 6857
        self.assertEqual(highest_prime_factor(600851475143), 6857)

    def test_invalid(self):
        with self.assertRaises(ValueError):
            highest_prime_factor(1)
        with self.assertRaises(ValueError):
            highest_prime_factor(0)


if __name__ == "__main__":
    unittest.main()
