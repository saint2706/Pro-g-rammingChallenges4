#!/usr/bin/env bash
# Thin wrapper to invoke the Python CLI with all arguments.
# Ensures we run relative to this script's directory so data files stay together.
set -euo pipefail
SCRIPT_DIR="$(cd "${BASH_SOURCE[0]%/*}" && pwd)"
cd "$SCRIPT_DIR"
exec python todo.py "$@"
