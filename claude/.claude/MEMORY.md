# Claude Code Memory

Persistent session-to-session learnings. Check this file before making assumptions
about workflows or project configurations. See `CLAUDE.md` for the general standards;
this file records observed exceptions and project-specific patterns.

## Dotfiles Setup

- Claude config lives in `~/software/dotfiles/claude/.claude/`, symlinked to `~/.claude/`
- Machine-specific files use a `.<machine>` suffix; active machine: `brightband`
- Install (or re-install after changes): `./dotfiles brightband -O` from the dotfiles root
- Dry-run to preview: `./dotfiles brightband -n`
- `settings.json.brightband` is the active Claude Code settings (symlinked → `settings.json`)
- `permission-alert.sh.brightband` → `permission-alert.sh` (macOS permission notifications)
- `task-complete.sh.brightband` → `task-complete.sh` (macOS task-complete notifications)
- `post-edit.sh` is universal (no machine suffix); handles PostToolUse ruff formatting

## Workflow Preferences

<!-- Add learnings here as they are discovered. Format: "Prefer X over Y because Z." -->

## Project-Specific Patterns

<!-- Add project-specific patterns here. Format: "project-name: pattern description" -->

## Common Mistakes to Avoid

<!-- Add mistakes here. Format: "Do NOT X — instead do Y." -->
