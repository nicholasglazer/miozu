#!/bin/bash
# Run Claude Code routed through DeepSeek's Anthropic-compatible API.
# Usage: claude-deepseek.sh [claude args...]
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ENV_FILE="$SCRIPT_DIR/../.env"

if [ -f "$ENV_FILE" ]; then
  set -a; source "$ENV_FILE"; set +a
else
  echo "Error: $ENV_FILE not found" >&2
  exit 1
fi

exec claude "$@"
