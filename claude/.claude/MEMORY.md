# Claude Code Memory

Persistent session-to-session learnings. Check this file before making assumptions
about workflows or project configurations. See `CLAUDE.md` for the general standards;
this file records observed exceptions and project-specific patterns.

## Dotfiles Setup

- Claude config lives in `dotfiles/claude/.claude/`, symlinked into `~/.claude/` by `./dotfiles <machine> -O` (`-n` for dry-run)
- Machine-specific files use a `.<machine>` suffix and win over the unsuffixed file
- Machines: `mbp` (personal; no Obsidian vault, no Brightband resources), `brightband` (work MBP, full vault), `brightband-gcp` (Linux VM; vault absent, publishes via `work-digest`)
- `CLAUDE.md` is a thin importer: `CLAUDE.base.md` (all machines) plus `CLAUDE.vault.md` (brightband machines only; the `-gcp` files are symlinks to the `.brightband` ones)
- Vault/Brightband skills are gated per machine by `MACHINE_ONLY` in the `dotfiles` script (`work-digest` also on gcp; the rest brightband only)
- `settings.json` is the vanilla default; `.mbp`, `.brightband`, `.brightband-gcp` override it
- `permission-alert.sh` / `task-complete.sh` have macOS `.brightband` variants; `post-edit.sh` is universal
- The installer never deletes stale links: after switching a machine, remove old `~/.claude/skills/<name>` links by hand

## Workflow Preferences

<!-- Add learnings here as they are discovered. Format: "Prefer X over Y because Z." -->

## Project-Specific Patterns

<!-- Add project-specific patterns here. Format: "project-name: pattern description" -->

## Common Mistakes to Avoid

<!-- Add mistakes here. Format: "Do NOT X — instead do Y." -->
