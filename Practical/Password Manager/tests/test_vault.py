import tempfile
from pathlib import Path
import sys
import unittest

MODULE_DIR = Path(__file__).resolve().parents[1]
if str(MODULE_DIR) not in sys.path:
    sys.path.insert(0, str(MODULE_DIR))

from vault import PasswordVault, generate_password


class PasswordVaultTests(unittest.TestCase):
    def setUp(self) -> None:
        self.tempdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tempdir.cleanup)
        self.vault_path = Path(self.tempdir.name) / "vault.pm"
        self.password = "correct horse battery staple"
        self.vault = PasswordVault.create(self.vault_path, self.password)

    def test_round_trip_encryption(self) -> None:
        entry = self.vault.add_entry(
            "Email", "alice", "hunter2", category="personal", notes="IMAP"
        )
        reloaded = PasswordVault.load(self.vault_path, self.password)
        entries = reloaded.list_entries()
        self.assertEqual(len(entries), 1)
        loaded_entry = entries[0]
        self.assertEqual(loaded_entry.name, entry.name)
        self.assertEqual(loaded_entry.password, "hunter2")
        self.assertEqual(loaded_entry.category, "personal")

    def test_wrong_password_rejected(self) -> None:
        self.vault.add_entry("Email", "alice", "hunter2")
        with self.assertRaises(ValueError):
            PasswordVault.load(self.vault_path, "wrong password")

    def test_audit_log_records_actions(self) -> None:
        self.vault.add_entry("VPN", "bob", "secret")
        self.vault.update_entry(self.vault.list_entries()[0].entry_id, notes="updated")
        events = self.vault.audit_log()
        actions = [event.action for event in events]
        self.assertIn("create", actions)
        self.assertIn("update", actions)

    def test_generate_password_constraints(self) -> None:
        password = generate_password(length=20)
        self.assertEqual(len(password), 20)
        with self.assertRaises(ValueError):
            generate_password(length=4)


if __name__ == "__main__":
    unittest.main()
