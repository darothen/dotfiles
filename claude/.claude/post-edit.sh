#!/bin/bash
# Auto-format Python files after Claude Code edits them.
# Triggered via PostToolUse hook (Edit|Write); receives tool info as JSON on stdin.
# Universal — no machine-specific variant needed since ruff runs everywhere.

input=$(cat)
file=$(echo "$input" | jq -r '.tool_input.file_path // ""')

# Only act on Python files
[[ "$file" == *.py ]] && [[ -n "$file" ]] || exit 0

# cd to the file's directory so pixi/uv can locate the project config
# (pixi.toml or pyproject.toml) by walking up the directory tree.
cd "$(dirname "$file")" || exit 0

# Try pixi first (conda-forge projects), then uv, then fall back to system ruff.
if pixi run ruff format "$file" 2>/dev/null; then
    pixi run ruff check --fix "$file" 2>/dev/null || true
elif uv run ruff format "$file" 2>/dev/null; then
    uv run ruff check --fix "$file" 2>/dev/null || true
elif command -v ruff >/dev/null 2>&1; then
    ruff format "$file" 2>/dev/null || true
    ruff check --fix "$file" 2>/dev/null || true
fi

exit 0
