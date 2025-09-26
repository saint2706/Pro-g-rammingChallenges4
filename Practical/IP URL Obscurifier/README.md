# IP & URL Obscurifier

Turn a readable IPv4 address or URL into a grab-bag of equivalent but harder-to-spot representations. This tool demonstrates how deceptively easy it is to mask a destination by leaning on alternate numeric bases, URL credential embedding, and browser parsing quirks.

> ⚠️ **Ethical use only.** The examples below are for defenders, educators, and curious learners who need to recognise suspicious inputs. Shipping obscured URLs to unsuspecting recipients is phishing.

---

## Feature Overview

| Capability | Summary |
|------------|---------|
| IPv4 encoding | Convert dotted quads into integer forms (decimal, hex, octal, binary) and dotted variants (hex, octal, zero padded). |
| Mixed-base disguises | Generate dotted quads that mix bases per-octet (e.g., decimal + hex + octal). |
| URL rewriting | Swap the host component of a URL for any of the generated variants, optionally injecting fake credentials. |
| Reverse decoding | Parse an unknown dotted/mixed/integer IPv4 representation back into canonical dotted-decimal form. |
| Safety checks | Validation ensures octets stay inside 0-255, warns on credential injection, and highlights the canonical endpoint. |

---

## Installation

The script is pure Python (3.9+) and only uses the standard library.

```bash
python "Practical/IP URL Obscurifier/obscurifier.py" --help
```

---

## Usage Examples

### Encode an IPv4 address

```bash
python "Practical/IP URL Obscurifier/obscurifier.py" encode-ip 192.168.0.1
```

Outputs a table of disguises such as:

- `3232235521` (decimal integer)
- `0xC0A80001` (hex integer)
- `0300.0250.0000.0001` (dotted octal)
- `192.0xA8.0o0.0b00000001` (mixed bases)

### Rewrite a URL with credentials

```bash
python "Practical/IP URL Obscurifier/obscurifier.py" encode-url \
    "http://192.168.0.1/admin" \
    --credentials "support:ticket"
```

This yields variants such as:

- `http://support:ticket@3232235521/admin`
- `http://support:ticket@0xC0A80001/admin`
- `http://support:ticket@0300.0250.0000.0001/admin`

### Decode unknown input

```bash
python "Practical/IP URL Obscurifier/obscurifier.py" decode-ip 0xC0A80001
python "Practical/IP URL Obscurifier/obscurifier.py" decode-ip 0300.0250.0000.0001
```

Both commands emit `192.168.0.1` and show the detected bases for each component.

---

## Mixed-base Strategies

The encoder ships with pre-defined patterns that mirror the obfuscation tricks often seen in capture-the-flag puzzles or phishing kits:

1. **Hex Spread:** `0xC0.0xA8.0x00.0x01` – every octet rendered in hexadecimal.
2. **Binary Tail:** `192.168.0b00000000.0b00000001` – trailing octets in binary with padding.
3. **Octal Sandwich:** `0300.168.0000.0x01` – mixes octal, decimal, and hex in a single host.
4. **Credential Bait:** Adds `user:pass@` before the host to draw attention away from the endpoint.

You can create custom mixes via the CLI flag `--mix decimal,hex,octal,binary`.

---

## Safety Notes

- **Browsers will follow these URLs.** Use in controlled environments only.
- **Log everything.** When testing, record which variant you clicked to avoid confusion.
- **Educate end-users.** Share this README with trainees so they can spot obfuscation tactics.
- **Canonical output.** Every run prints the standard dotted-decimal alongside variants for verification.

---

## Extending

- Add IPv6 support (much harder to disguise, but fun!).
- Emit JSON with `--format json` for automation.
- Build a tiny web UI to paste suspicious URLs and reveal their targets.

