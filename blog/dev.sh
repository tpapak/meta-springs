#!/usr/bin/env bash
# Build, serve, and watch the blog. Optional argument: port (default 8000).
set -euo pipefail
cd "$(dirname "$0")"
exec python3 tools/dev.py "$@"
