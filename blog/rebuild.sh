#!/usr/bin/env bash
# Rebuild the blog HTML from content/*.md  (run from anywhere).
#   ./rebuild.sh          rebuild once
#   ./rebuild.sh --watch  rebuild automatically whenever a .md / animation / css changes
set -euo pipefail
cd "$(dirname "$0")"

build() { python3 tools/mdblog.py build; }

if [[ "${1:-}" == "--watch" || "${1:-}" == "-w" ]]; then
  echo "watching content/, animations, style.css … (Ctrl-C to stop)"
  build
  if command -v fswatch >/dev/null 2>&1; then
    fswatch -o content *.html style.css tools/mdblog.py | while read -r _; do
      echo "change detected — rebuilding"; build || echo "build failed"; done
  else
    # portable fallback: poll for changes every 2s (no fswatch needed)
    stamp() { find content *.html style.css tools/mdblog.py -type f -newer /tmp/.mdblog_last 2>/dev/null | head -1; }
    : > /tmp/.mdblog_last
    while true; do
      if [[ -n "$(stamp)" ]]; then echo "change detected — rebuilding"; build || echo "build failed"; : > /tmp/.mdblog_last; fi
      sleep 2
    done
  fi
else
  build
fi
