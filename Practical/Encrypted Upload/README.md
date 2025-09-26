# Encrypted Upload CLI

The Encrypted Upload CLI encrypts files locally with AES-GCM and sends them to an upload target. It supports Amazon S3 buckets and generic HTTP endpoints so you can integrate it into existing storage workflows.

## Features

- **Authenticated encryption** with AES-256-GCM and per-upload nonces.
- **Password-based key derivation** via PBKDF2-HMAC-SHA256 or direct hexadecimal keys.
- **Integrity manifests** that capture hashes, encryption parameters, and upload metadata.
- **Optional manifest signing** using HMAC-SHA256 for offline verification.
- **Pluggable upload targets**: S3 (via `boto3`) or arbitrary HTTP forms/JSON APIs.

## Quick Start

1. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

2. Create an endpoint configuration file, for example `endpoints.json`:
   ```json
   {
     "endpoints": [
       {
         "name": "personal-s3",
         "type": "s3",
         "bucket": "my-upload-bucket",
         "region": "us-east-1",
         "key_prefix": "encrypted/",
         "expires_in": 7200
       },
       {
         "name": "staging-http",
         "type": "http",
         "url": "https://uploads.example.net/files",
         "method": "POST",
         "response_url_field": "download_url"
       }
     ]
   }
   ```

3. Encrypt and upload a file:
   ```bash
   python "Practical/Encrypted Upload/encrypted_upload.py" upload \
       --file report.pdf \
       --config endpoints.json \
       --endpoint personal-s3 \
       --password "correct horse battery staple" \
       --manifest manifest.json \
       --signing-key 1f2d3c4b5a69788790abccddeeff0011
   ```

4. The CLI prints a shareable link, writes the encrypted payload to the remote storage, and stores the manifest (with optional signature) locally.

## Manifest Format

Each manifest is JSON with the following schema:

```json
{
  "original_filename": "report.pdf",
  "filesize": 12345,
  "sha256": "...",
  "encryption": {
    "algorithm": "AES-256-GCM",
    "nonce": "base64==",
    "salt": "base64==",
    "associated_data": "report.pdf"
  },
  "upload": {
    "endpoint": "personal-s3",
    "object_key": "encrypted/2024-08-19T10-03-15Z_report.pdf.enc",
    "timestamp": "2024-08-19T10:03:16Z",
    "link": "https://my-upload-bucket.s3.us-east-1.amazonaws.com/encrypted/2024-08-19T10-03-15Z_report.pdf.enc"
  },
  "signature": {
    "algorithm": "HMAC-SHA256",
    "value": "base64=="
  }
}
```

The `signature` block is only present when `--signing-key` is provided.

## Upload Targets

### Amazon S3

The CLI uploads with the AWS SDK (`boto3`). Provide credentials through the standard AWS environment variables, shared configuration files, or an assigned IAM role. Configure your endpoint block with:

- `bucket` (required): target S3 bucket name.
- `region` (required): AWS region for the bucket.
- `key_prefix` (optional): path prefix prepended to generated object keys.
- `expires_in` (optional): seconds for the generated pre-signed URL (default 3600).

The shareable link is produced via a pre-signed `GetObject` URL.

### Generic HTTP Endpoint

For custom storage services, specify a JSON block with:

- `url`: where the encrypted file is uploaded.
- `method`: `POST` (default) or `PUT`.
- `field_name`: multipart field name for the binary payload (default `file`).
- `headers`: optional dictionary of HTTP headers.
- `response_url_field`: JSON field that contains a download URL (default `url`).

The CLI performs a multipart request (`files={field_name: (filename, encrypted_bytes)}`) and expects a JSON response.

## Security Considerations

- **Key management is your responsibility.** Never hardcode keys or reuse weak passwords.
- **Password-derived keys need strong passphrases.** Use a password manager to generate random phrases.
- **Salts and nonces are randomly generated** per upload and stored in the encrypted payload/manifest. Do not modify them.
- **Manifests contain sensitive metadata.** Store them securely and consider encrypting or signing them for audit purposes.
- **Review your HTTP targets.** Ensure TLS is enforced and the server validates authentication before storing uploads.
- **Audit dependencies and environments** before using this tool in production. This repository is educational and does not replace a vetted security solution.

## Testing

Run automated tests (which include mocked S3 and HTTP servers):

```bash
pytest Practical/Encrypted\ Upload/tests
```

These tests ensure encryption correctness, manifest integrity, and upload flows without contacting external services.

