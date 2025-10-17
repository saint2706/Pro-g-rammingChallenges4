# Password Manager

A secure, file-based password vault that encrypts credentials with a master password. The tool ships with both a CLI and a lightweight Tk GUI for day-to-day use, plus helpers for importing/exporting data and generating strong passwords.

## Encryption & Storage Model

- **Algorithm**: AES-256-GCM provided by `cryptography.hazmat.primitives.ciphers.aead.AESGCM`.
- **Key Derivation**: PBKDF2-HMAC-SHA256 with a 16-byte random salt and 200,000 iterations derives the symmetric key from the master password.
- **Envelope Format**: Encrypted payload contains entries, audit log, and metadata (timestamps, schema version). The ciphertext, salt, and nonce are stored Base64-encoded inside a JSON document.
- **Integrity**: AES-GCM provides authentication; a wrong password or tampered file raises an error before any data is returned.
- **Audit Trail**: Every CRUD action is appended to an in-vault audit log with timestamp, entry id, action, and a short human-readable note.

## Command Line Interface

```
python "challenges/Practical/Password Manager/password_manager.py" --help
```

Supported commands:

| Command | Description |
|---------|-------------|
| `init` | Create a new encrypted vault. Prompts for a master password. |
| `list` | Show stored entries (optionally filter by category). |
| `show` | Display full details of one entry. |
| `add` | Create a new entry. Can auto-generate a password if none is provided. |
| `update` | Modify an entry's fields. Only provided values change. |
| `delete` | Remove an entry permanently. |
| `generate` | Produce a random password using the built-in generator. |
| `export` | Write entries to JSON or CSV (plaintext) for backups or migration. |
| `import` | Load entries from JSON or CSV. Supports merge or replace strategies. |
| `audit` | View recent audit trail events. |

Each command accepts `--vault` to specify the vault file path and will prompt for the master password if `--password` is omitted.

## GUI Overview

```
python "challenges/Practical/Password Manager/gui.py" --vault vault.pm
```

The Tk GUI wraps the same core vault logic:

- Prompts for the master password (or allows creating a new vault).
- Lists saved entries; selecting one enables `View`, `Edit`, or `Delete` actions.
- Provides `Add Entry`, `Generate Password`, and `Export` buttons.
- All edits immediately persist via AES-GCM re-encryption and append to the audit log.

## Import / Export Formats

- **JSON**: Array of entry objects. Sensitive data is plaintext once exported—store securely!
- **CSV**: UTF-8 header row: `name,username,password,category,notes`.

## Security Considerations

- Choose a strong, unique master password; there is no recovery if forgotten.
- Vault files are only as secure as their storage location—protect backups and exports.
- Passwords provided on the CLI via `--password` may end up in shell history; prefer interactive prompts.
- Exports are plaintext by design. Delete them after use or encrypt separately (e.g., with age or GPG).
- GUI clipboard operations leave passwords in the system clipboard until overwritten.

## Tests

Unit tests in `tests/test_vault.py` cover the encryption/decryption flow, ensuring that data written with one password can be read back and that an incorrect password is rejected.
