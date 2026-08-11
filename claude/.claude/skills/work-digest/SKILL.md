---
name: work-digest
description: This skill should be used when the user types "/work-digest", asks for a
  "work digest", "session digest", "summary of what you did", "handoff note", or when an
  agent loop / scheduled workflow running on a remote machine needs to leave behind a
  portable 1-2 page Markdown summary of completed work. Use this instead of "log-work"
  whenever the Obsidian vault at ~/Documents/workspace/ is unavailable (remote host, CI
  runner, cloud sandbox, MCP server down). Produces a single self-contained file that a
  local agent later folds into the daily log and a literature note.
---

# Work Digest

Write a **single, self-contained Markdown file** — 1–2 pages — summarizing the work an
agent just completed. The file is the only thing that survives the session: it will be
read later, on a different machine, by a different agent that has none of this context.

**Do not** attempt to write to `~/Documents/workspace/` from this skill. That is the
local ingest agent's job. If the vault happens to exist on this machine, use `log-work`
instead — this skill is for the case where it doesn't.

---

## Step 1 — Confirm this is the right skill

```bash
test -d ~/Documents/workspace && echo "vault present — prefer /log-work" || echo "no vault — proceed"
```

If the vault is present and the user did not explicitly ask for a digest file, say so and
offer `log-work` instead.

---

## Step 2 — Reconstruct what actually happened

Scan the full conversation from the beginning, then corroborate with the repo. Never
summarize from memory alone when the shell can confirm it:

```bash
git log --oneline -20
git diff --stat HEAD~1 2>/dev/null || git status --short
gh pr list --author @me --limit 5 2>/dev/null
```

Collect:

- The task as originally stated (quote or tightly paraphrase the user's ask)
- The approach taken, including approaches tried and **abandoned** — the dead ends are
  often the most valuable part of the digest
- Concrete results: commits, PRs, files changed, benchmarks, test outcomes, data produced
- Learnings: gotchas, surprising behavior, constraints discovered, corrected assumptions
- Anything left unfinished, blocked, or deliberately deferred

For loop or scheduled runs, cover only work since the previous digest. Check for one:

```bash
ls -t "${CLAUDE_DIGEST_DIR:-$HOME/agent-digests}"/*.md 2>/dev/null | head -3
```

If a prior digest covers part of this work, reference it by filename rather than
repeating it.

---

## Step 3 — Choose the output path

Resolve in this order:

1. A path the user specified in the request
2. `$CLAUDE_DIGEST_DIR` if set
3. `<repo-root>/.agent-digests/` if the repo already has that directory
4. `~/agent-digests/` (create it)

Filename: `YYYY-MM-DD-<kebab-slug>.md`, where the slug names the work, not the session —
`2026-08-11-prepbufr-decoder-rewrite.md`, not `2026-08-11-session-3.md`. If a file with
that name exists, append `-2`, `-3`, … rather than overwriting.

If the directory sits inside a git repo, make sure it is ignored (`.gitignore` or
`.git/info/exclude`) unless the user wants digests committed.

---

## Step 4 — Write the digest

Hard constraints:

- **400–900 words of body.** One page is the target, two is the ceiling. If the work
  genuinely exceeds that, write the digest at the ceiling and list the overflow under
  "Follow-ups" — do not sprawl.
- **Self-contained.** No "as discussed above", no references to conversation turns, no
  unexplained internal jargon. Expand acronyms on first use.
- **Absolute, resolvable references.** Full repo names, PR/issue URLs, absolute paths,
  full commit SHAs (short SHA plus subject line). The reader is on another machine.
- **Prose over transcript.** Explain what was decided and why. Do not narrate the
  sequence of tool calls.
- **No invention.** If a test was not run, say it was not run. Mark anything uncertain
  as uncertain.

### Template

```markdown
---
created: YYYY-MM-DD
type: work-digest
machine: <hostname>
repo: <org/repo or path>
branch: <branch>
tags:
  - project-name
  - topic
---

# <Descriptive Title of the Work>

## Task

What was asked for, and the context that made it necessary. 2–4 sentences.

## Approach

How it was tackled, and why that way. Name the key design decisions and the
constraints that forced them. Include approaches that were tried and rejected,
with the reason for rejection.

## Key Results

- Concrete, verifiable outcomes — commits, PRs, files, measurements
- `abc1234` — commit subject line
- [org/repo#123](https://github.com/org/repo/pull/123) — PR title, merged/open
- Test/benchmark results, stated exactly (including failures)

## Key Learnings

- Non-obvious things discovered that would save time next session
- Corrected assumptions, tool gotchas, undocumented behavior
- Keep each to 1–3 sentences; these are the seeds of the literature note

## Outcomes & Status

Where things stand now. What is done, what is verified, what is merely written.

## Follow-ups

- [ ] Open threads, deferred work, known limitations
- [ ] Include enough detail to act on without this session's context

---

## Ingest Hints

*For the local agent folding this into Obsidian.*

- **Daily log bullet:** `- <project>: <one-line outcome> → [[<Suggested Note Title>]]`
- **Suggested note:** `notes/<Suggested Note Title>.md` — or
  `projects/<Project Name>/` if the work belongs to a tracked project
- **Suggested tags:** `tag-one`, `tag-two`
- **Related existing notes:** `[[Note A]]`, `[[Note B]]` (best guess; verify locally)
```

Omit `Follow-ups` if there genuinely are none. Every other section is required — an
empty-feeling section usually means the reconstruction in Step 2 was incomplete.

The `Ingest Hints` block is what makes the digest cheap to consume locally. Fill it in
even when guessing; a wrong guess is easy for the local agent to correct, a missing one
costs it a search.

---

## Step 5 — Verify and report

Re-read the written file once as if you had no context. If any sentence would be
unintelligible to that reader, rewrite it.

```bash
wc -w <path-to-digest>
```

Then report:

```
Work digest written:
  <absolute path>  (<N> words)

Topic: <title>
Covers: <N commits, N PRs, ...>

To ingest locally:
  "Read <path> and fold it into today's daily log and a literature note."
```

If the digest was written to a remote machine, remind the user how to retrieve it
(`scp`, `gh` artifact, shared volume) — a digest the user cannot reach is worthless.
