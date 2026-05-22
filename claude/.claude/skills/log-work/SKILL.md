---
name: log-work
description: This skill should be used when the user types "/log-work", asks to "log
  this session", "write my daily note", "update obsidian", "write session notes",
  "document what we did", "record this work", or wants to capture the current
  session's accomplishments in their Obsidian knowledge base.
---

# Log Work to Obsidian

Record the current session's work to the user's Obsidian vault: a concise daily note
entry and, when the session was substantial, a long-form reference note.

---

## Step 1 — Review the full session

Scan the complete conversation history from the beginning. Identify every meaningful
unit of work:

- Commits made (message + what changed)
- PRs opened, updated, or merged (number + title)
- Bugs diagnosed and fixed
- Features implemented
- Architectural or design decisions
- Issues filed (number + title + origin)
- Documents or artifacts produced
- Research findings

Cross-reference with shell commands to catch anything not explicitly discussed:

```bash
git log --oneline -20
gh pr list --state merged --limit 5
```

---

## Step 2 — Detect what's already logged

Read today's daily note to avoid duplicates:

- Path: `~/Documents/workspace/daily/YYYY/YYYY-MM-DD.md` (use today's actual date)
- Note any bullets already present under `## Notes` that cover this session's work

Also scan this conversation for a prior `/log-work` invocation. If found, only log
work done *after* that checkpoint.

---

## Step 3 — Write the daily note entry

**Locate or create the file:**

- If the daily note exists, read it and prepare to insert bullets
- If missing, create it from `~/Documents/workspace/templates/daily_log.md`,
  replacing all Templater expressions (`<% ... %>`) with today's literal dates
  and correct nav links (yesterday/tomorrow/this-week)

**Compose the entry:**

- 1–4 bullets summarizing the session's key outcomes
- Lead each bullet with the project name or context (e.g. `debufr:`, `NNJA:`)
- Keep bullets concise — one clear outcome per bullet
- Use indented sub-bullets only for detail that won't be in the long-form note
- If a long-form note is being written, end the final relevant bullet with
  `→ [[notes/Title of Long-Form Note]]`

**Insert location:** Under `## Notes`, immediately before `# Activity Summary`.
Never touch anything from `# Activity Summary` downward.

---

## Step 4 — Determine where to write artifacts

Before writing any notes, assess whether the session is part of a recognized long-running
project with its own directory in `~/Documents/workspace/projects/`.

**Check for a project directory:**

```bash
ls ~/Documents/workspace/projects/
```

A session belongs to a project directory if:
- The work is clearly scoped to a named, ongoing project (e.g. "PREPBUFR processing", a
  specific library rewrite, a multi-month engineering initiative)
- A matching subdirectory already exists under `projects/`, OR the session clearly warrants
  starting one (multi-session work with durable design decisions or tracked plans)

**Routing rules:**

| Document type | Project session | General session |
|---|---|---|
| Session notes | `projects/<Name>/` | `notes/` |
| Design decisions / plans | `projects/<Name>/` | `notes/` |
| Data/format references specific to the project | `projects/<Name>/` | `notes/` |
| General technical knowledge | `notes/` | `notes/` |
| Cross-project reference material | `notes/` | `notes/` |

If no existing project directory matches, default to `notes/` unless the session clearly
warrants creating a new project directory (ask the user if uncertain).

---

## Step 5 — Write the long-form note

Always produce a long-form note unless the session was trivial (a single quick
question with no code changes, commits, decisions, or filed issues).

**Assess whether the session is substantial:**

Substantial if any of the following apply:
- One or more PRs merged or opened
- A new feature or significant bug fix was implemented
- An architectural or design decision was made
- Multiple files were modified with meaningful logic changes
- Research was completed with findings worth preserving
- New tracking issues were filed from review findings

**Find or create the note:**

1. Check the appropriate destination (project directory or `notes/`) for an existing note
   on this topic (e.g. a prior session note for the same project or a related evergreen doc)
2. If found: update in place, adding a new dated section rather than overwriting
3. If not found: create a new file

**Naming conventions:**

- Session notes: `<Project> <Topic> — Session Notes <YYYY-MM-DD>.md`
  e.g. `debufr Multi-DX-Table BUFR Fix — Session Notes 2026-03-25.md`
- Evergreen reference: `<Descriptive Topic>.md`
  e.g. `BUFR DX Table Architecture.md`
- Project-specific plans/decisions (in project dir): `plan.md`, `decisions.md`, `context.md`

**Frontmatter:**

```yaml
---
created: YYYY-MM-DD
tags:
  - project-name
  - topic
  - engineering   # or: research, design, etc.
---
```

For updates to an existing note, add `updated: YYYY-MM-DD` alongside `created`.

**Typical body structure for a session note:**

```markdown
## Session Overview

1-3 sentence summary of what was accomplished.

---

## PRs Landed / Work Completed

### [PR #N](url) — title
- What it fixed/added and why

## Key Technical Decisions

**Decision name:** Explanation of what was decided and why (constraints, trade-offs).

## Issues Filed

| # | Title | Origin |
|---|-------|--------|
| [#N](url) | Title | Where it came from |

## Pending / Follow-up

- Any open threads worth flagging
```

Adapt the sections to what actually happened — omit sections that don't apply.

---

## Step 6 — Confirm

After writing, output a brief confirmation listing exactly what was written:

```
Logged to Obsidian:
- Daily note: ~/Documents/workspace/daily/YYYY/YYYY-MM-DD.md  (N bullet(s) added)
- Long-form note: ~/Documents/workspace/notes/<Title>.md  (created / updated)
  OR
- Project note: ~/Documents/workspace/projects/<Name>/<Title>.md  (created / updated)
```

If no long-form note was written (trivial session), say so explicitly so the user
knows the skip was intentional.
