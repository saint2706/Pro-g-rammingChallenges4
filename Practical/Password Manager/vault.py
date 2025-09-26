"""Core vault logic for the Practical Password Manager.

The module exposes the :class:`PasswordVault` class for loading/creating
AES-GCM encrypted vaults, along with data helpers for entries and audit
records.
"""
from __future__ import annotations

import base64
import json
import os
import secrets
import uuid
from dataclasses import asdict, dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, Iterable, List, Optional

from cryptography.exceptions import InvalidTag
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC

KDF_ITERATIONS = 200_000
KDF_SALT_BYTES = 16
AES_KEY_BYTES = 32
GCM_NONCE_BYTES = 12
ASSOCIATED_DATA = b"PracticalPasswordManager::vault"


def _utc_now() -> str:
    """Return a Z-suffixed ISO 8601 timestamp without microseconds."""
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


@dataclass
class VaultEntry:
    """A single credential stored in the vault."""

    entry_id: str
    name: str
    username: str
    password: str
    category: str = "general"
    notes: str = ""
    created_at: str = field(default_factory=_utc_now)
    updated_at: str = field(default_factory=_utc_now)

    def to_dict(self) -> Dict[str, str]:
        return asdict(self)

    @classmethod
    def from_dict(cls, data: Dict[str, str]) -> "VaultEntry":
        return cls(**data)


@dataclass
class AuditEvent:
    """Represents a single audit log entry."""

    timestamp: str
    action: str
    entry_id: Optional[str]
    detail: str

    def to_dict(self) -> Dict[str, Optional[str]]:
        return asdict(self)

    @classmethod
    def from_dict(cls, data: Dict[str, Optional[str]]) -> "AuditEvent":
        return cls(**data)


class PasswordVault:
    """Encrypted credential store backed by a JSON + AES-GCM file."""

    version = 1

    def __init__(self, path: Path, key: bytes, *, salt: bytes, iterations: int, data: Dict):
        self._path = Path(path)
        self._key = key
        self._salt = salt
        self._iterations = iterations
        self._data = data

    # ------------------------------------------------------------------
    # Creation / loading helpers
    # ------------------------------------------------------------------
    @classmethod
    def create(cls, path: Path, password: str) -> "PasswordVault":
        """Create a brand-new vault at *path* using *password*."""
        path = Path(path)
        if path.exists():
            raise FileExistsError(f"Vault already exists at {path}")

        salt = secrets.token_bytes(KDF_SALT_BYTES)
        key = cls._derive_key(password, salt)
        now = _utc_now()
        data = {
            "entries": [],
            "audit_log": [],
            "created": now,
            "updated": now,
        }
        vault = cls(path, key, salt=salt, iterations=KDF_ITERATIONS, data=data)
        vault._log_event("init", None, "Vault created")
        vault.save()
        return vault

    @classmethod
    def load(cls, path: Path, password: str) -> "PasswordVault":
        """Load an existing vault from *path* using *password*."""
        path = Path(path)
        if not path.exists():
            raise FileNotFoundError(f"Vault not found: {path}")

        file_data = json.loads(path.read_text("utf-8"))
        if file_data.get("version") != cls.version:
            raise ValueError("Unsupported vault version")

        kdf_info = file_data.get("kdf", {})
        salt = base64.b64decode(kdf_info.get("salt", ""))
        iterations = int(kdf_info.get("iterations", KDF_ITERATIONS))
        nonce = base64.b64decode(file_data["vault"].get("nonce", ""))
        ciphertext = base64.b64decode(file_data["vault"].get("ciphertext", ""))

        key = cls._derive_key(password, salt, iterations=iterations)
        try:
            plaintext = cls._decrypt(key, nonce, ciphertext)
        except InvalidTag as exc:  # pragma: no cover - exception branch
            raise ValueError("Invalid password or corrupted vault") from exc

        data = json.loads(plaintext.decode("utf-8"))
        return cls(path, key, salt=salt, iterations=iterations, data=data)

    # ------------------------------------------------------------------
    # CRUD operations
    # ------------------------------------------------------------------
    def list_entries(self, *, category: Optional[str] = None) -> List[VaultEntry]:
        entries = [VaultEntry.from_dict(e) for e in self._data.get("entries", [])]
        if category:
            entries = [e for e in entries if e.category == category]
        return entries

    def get_entry(self, entry_id: str) -> VaultEntry:
        for entry in self._data.get("entries", []):
            if entry["entry_id"] == entry_id:
                return VaultEntry.from_dict(entry)
        raise KeyError(f"Entry not found: {entry_id}")

    def add_entry(
        self,
        name: str,
        username: str,
        password: str,
        *,
        category: str = "general",
        notes: str = "",
        save: bool = True,
    ) -> VaultEntry:
        entry = VaultEntry(
            entry_id=str(uuid.uuid4()),
            name=name,
            username=username,
            password=password,
            category=category or "general",
            notes=notes,
        )
        self._data.setdefault("entries", []).append(entry.to_dict())
        self._touch_updated()
        self._log_event("create", entry.entry_id, f"Created entry '{entry.name}'")
        if save:
            self.save()
        return entry

    def update_entry(self, entry_id: str, **updates: str) -> VaultEntry:
        allowed = {"name", "username", "password", "category", "notes"}
        changed = {}
        for entry in self._data.get("entries", []):
            if entry["entry_id"] == entry_id:
                for key, value in updates.items():
                    if key in allowed and value is not None:
                        entry[key] = value
                        changed[key] = value
                entry["updated_at"] = _utc_now()
                self._touch_updated()
                if changed:
                    self._log_event(
                        "update",
                        entry_id,
                        "Updated fields: " + ", ".join(sorted(changed.keys())),
                    )
                    self.save()
                return VaultEntry.from_dict(entry)
        raise KeyError(f"Entry not found: {entry_id}")

    def delete_entry(self, entry_id: str) -> None:
        entries = self._data.get("entries", [])
        for idx, entry in enumerate(entries):
            if entry["entry_id"] == entry_id:
                removed = entries.pop(idx)
                self._touch_updated()
                self._log_event("delete", entry_id, f"Deleted entry '{removed['name']}'")
                self.save()
                return
        raise KeyError(f"Entry not found: {entry_id}")

    # ------------------------------------------------------------------
    # Import / export / audit
    # ------------------------------------------------------------------
    def export_entries(self) -> List[Dict[str, str]]:
        return [dict(entry) for entry in self._data.get("entries", [])]

    def import_entries(
        self,
        entries: Iterable[Dict[str, str]],
        *,
        replace: bool = False,
        source: str = "import",
    ) -> int:
        if replace:
            count = len(self._data.get("entries", []))
            if count:
                self._log_event("import", None, f"Cleared {count} entries before import")
            self._data["entries"] = []

        imported = 0
        for incoming in entries:
            self.add_entry(
                name=incoming.get("name", "Unnamed"),
                username=incoming.get("username", ""),
                password=incoming.get("password", ""),
                category=incoming.get("category", "general"),
                notes=incoming.get("notes", ""),
                save=False,
            )
            imported += 1

        if imported:
            self._log_event("import", None, f"Imported {imported} entries from {source}")
            self.save()
        return imported

    def audit_log(self, *, limit: Optional[int] = None) -> List[AuditEvent]:
        events = [AuditEvent.from_dict(e) for e in self._data.get("audit_log", [])]
        if limit is not None:
            events = events[-limit:]
        return events

    # ------------------------------------------------------------------
    # Persistence helpers
    # ------------------------------------------------------------------
    def save(self) -> None:
        plaintext = json.dumps(self._data, indent=2, ensure_ascii=False).encode("utf-8")
        nonce, ciphertext = self._encrypt(self._key, plaintext)
        file_data = {
            "version": self.version,
            "kdf": {
                "name": "PBKDF2HMAC-SHA256",
                "length": AES_KEY_BYTES,
                "iterations": self._iterations,
                "salt": base64.b64encode(self._salt).decode("ascii"),
            },
            "vault": {
                "nonce": base64.b64encode(nonce).decode("ascii"),
                "ciphertext": base64.b64encode(ciphertext).decode("ascii"),
            },
        }
        tmp_path = self._path.with_suffix(self._path.suffix + ".tmp")
        tmp_path.write_text(json.dumps(file_data, indent=2), encoding="utf-8")
        os.replace(tmp_path, self._path)

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------
    def _touch_updated(self) -> None:
        self._data["updated"] = _utc_now()

    def _log_event(self, action: str, entry_id: Optional[str], detail: str) -> None:
        log = self._data.setdefault("audit_log", [])
        log.append(AuditEvent(timestamp=_utc_now(), action=action, entry_id=entry_id, detail=detail).to_dict())

    @staticmethod
    def _derive_key(password: str, salt: bytes, *, iterations: int = KDF_ITERATIONS) -> bytes:
        kdf = PBKDF2HMAC(
            algorithm=hashes.SHA256(),
            length=AES_KEY_BYTES,
            salt=salt,
            iterations=iterations,
        )
        return kdf.derive(password.encode("utf-8"))

    @staticmethod
    def _encrypt(key: bytes, plaintext: bytes) -> (bytes, bytes):
        aesgcm = AESGCM(key)
        nonce = secrets.token_bytes(GCM_NONCE_BYTES)
        ciphertext = aesgcm.encrypt(nonce, plaintext, ASSOCIATED_DATA)
        return nonce, ciphertext

    @staticmethod
    def _decrypt(key: bytes, nonce: bytes, ciphertext: bytes) -> bytes:
        aesgcm = AESGCM(key)
        return aesgcm.decrypt(nonce, ciphertext, ASSOCIATED_DATA)


def generate_password(
    length: int = 16,
    *,
    use_upper: bool = True,
    use_digits: bool = True,
    use_symbols: bool = True,
) -> str:
    """Generate a random password suitable for credentials."""
    if length < 8:
        raise ValueError("Password length should be at least 8 characters")

    alphabet = list("abcdefghijklmnopqrstuvwxyz")
    if use_upper:
        alphabet.extend(list("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    if use_digits:
        alphabet.extend(list("0123456789"))
    if use_symbols:
        alphabet.extend(list("!@#$%^&*()-_=+[]{};:,.?/"))

    if not alphabet:
        raise ValueError("No characters available for password generation")

    return "".join(secrets.choice(alphabet) for _ in range(length))


__all__ = [
    "PasswordVault",
    "VaultEntry",
    "AuditEvent",
    "generate_password",
]
