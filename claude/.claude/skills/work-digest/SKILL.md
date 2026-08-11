---
name: work-digest
description: This skill should be used when the user types "/work-digest", asks for a
  "work digest", "session digest", "summary of what you did", "handoff note", or when an
  agent loop / scheduled workflow running on a remote machine needs to leave behind a
  portable 1-2 page Markdown summary of completed work. Use this instead of "log-work"
  whenever the Obsidian vault at ~/Documents/workspace/ is unavailable (remote host, CI
  runner, cloud sandbox, MCP server down). Produces a single self-contained file that a
  local agent later folds into the daily log and a literature note. The digest is
  published to the brightbandtech/daniel-misc repo under work-logs/, which is cloned on
  every machine during initialization.
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

For loop or scheduled runs, cover only work since the previous digest. Query the digest
repo directly — no clone needed for a read:

```bash
gh api "repos/brightbandtech/daniel-misc/contents/work-logs/$(date +%Y)" \
  --jq '.[].name' 2>/dev/null | tail -5
```

If a prior digest covers part of this work, reference it by filename rather than
repeating it.

---

## Step 3 — Decide the destination

Digests are published to **`brightbandtech/daniel-misc`** (private, default branch
`main`) under `work-logs/`.

**Do not assume a working copy exists.** No machine is expected to have this repo
checked out — Step 6 clones it into a temp directory, publishes, and throws the clone
away. The repo is a publishing target, not a workspace.

**Path within the repo:** `work-logs/YYYY/YYYY-MM-DD Topic Name.md`

One digest per file, always — never append to or edit an existing digest. Each run of
this skill produces exactly one new file.

The topic names the work, not the session: `2026-08-11 PREPBUFR Decoder Rewrite.md`,
not `2026-08-11 Session 3.md`. Write it as a human-readable title in Title Case; it
matches the vault's note-naming convention and often becomes the literature note title
verbatim. Spaces in the filename are intentional for that reason — **quote every path
in every shell command that touches it.**

If that filename is already taken, append the hostname (`2026-08-11 PREPBUFR Decoder
Rewrite (gcp-a100).md`), and only then a numeric suffix. Two machines running the same
loop on the same day is the expected collision, and the hostname is the useful
disambiguator.

Because many machines write to this one repo, the `machine:` frontmatter field is
load-bearing — always fill it with the real hostname.

A running index lives at `work-logs/_INDEX.md` — the leading underscore sorts it to the
top of the directory listing. Step 6 appends one line to it per digest.

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

## Step 5 — Verify

Re-read the written file once as if you had no context. If any sentence would be
unintelligible to that reader, rewrite it.

```bash
wc -w <path-to-digest>
```

---

## Step 6 — Publish to the digest repo

### 6a. Keep a durable copy first

Before touching git, write the digest to a path that survives temp-directory cleanup:

```bash
mkdir -p "${CLAUDE_DIGEST_DIR:-$HOME/agent-digests}"
```

Everything below happens in a throwaway clone. If publishing fails, this copy is the
only thing standing between the session's work and oblivion — write it first, always.

### 6b. Clone into a temp directory

```bash
WORKDIR="$(mktemp -d)"
gh repo clone brightbandtech/daniel-misc "$WORKDIR/daniel-misc" -- --filter=blob:none -q
cd "$WORKDIR/daniel-misc"
```

`--filter=blob:none` gives a blobless clone: full history, ~9 MB, a few seconds. Use it
rather than `--depth=1` — a shallow clone makes `push` and `pull --rebase` unreliable,
which is exactly what Step 6d depends on.

This requires `gh` to be authenticated with `repo` scope. If the clone fails, stop and
skip to the failure report — do not attempt to work around missing credentials.

### 6c. Write the digest and update the index

```bash
mkdir -p "work-logs/$(date +%Y)"
# ...write "work-logs/<YYYY>/<YYYY-MM-DD Topic Name>.md" here...
```

Append one line to `work-logs/_INDEX.md`, newest at the bottom:

```markdown
- `2026-08-11` — [PREPBUFR Decoder Rewrite](<2026/2026-08-11 PREPBUFR Decoder Rewrite.md>) — `gcp-a100` — one-line summary of the outcome
```

Note the angle brackets around the link target: filenames contain spaces, and
`[text](<path with spaces>)` is the form that both GitHub and Obsidian resolve. A bare
space in a Markdown link silently breaks it.

If `_INDEX.md` does not exist, create it with an `# Work Log Index` heading, a one-line
explanation, and the first entry. Do not re-sort existing lines — see 6d.

**Also create `work-logs/.gitattributes` on first run**, containing:

```
_INDEX.md merge=union
```

This is what makes a shared index safe. Without it, two agents appending different lines
to `_INDEX.md` produce a rebase conflict every time they overlap. With it, git keeps both
sides automatically and the push just succeeds.

The one gap: `.gitattributes` must already be committed for the union driver to apply, so
if two agents race on the very first digest ever written, that one can still conflict.
After the first successful publish the protection is in place permanently.

### 6d. Commit and push

Stage only the digest and the index — never `git add -A`, and never commit unrelated
files that happen to be in the clone.

```bash
git add "work-logs/<YYYY>/<filename>.md" work-logs/_INDEX.md work-logs/.gitattributes
git commit -q -m "log: <YYYY-MM-DD> <short topic> (<hostname>)"

for i in 1 2 3; do
  git push && break
  git pull --rebase && sleep $((i * 3))
done
```

**Pushing is a race** — many machines publish here, so a rejected push is routine, not
exceptional. The digest file itself can never conflict (unique path per run), and
`merge=union` handles the index, so the retry loop should resolve cleanly.

Union merge can leave index lines slightly out of date order after a concurrent append.
That is fine and expected — do not "fix" it by re-sorting, which would rewrite lines
other agents are appending to and reintroduce the conflicts the union driver just
eliminated. Lines carry their own date prefix; sort at read time.

If a rebase conflicts anyway, stop and report rather than resolving it — that means
something unexpected is in the repo.

### 6e. Clean up

On success, remove the temp clone:

```bash
rm -rf "$WORKDIR"
```

**On failure, do not remove it.** Report the failure plainly, with both the durable copy
from 6a and the temp clone path so the commit can be pushed later:

Push failure is the outcome most likely to go unnoticed in an unattended loop, so it
belongs in the first line of the report, never as a footnote. Never claim a digest was
published when it wasn't.

---

## Step 7 — Report

```
Work digest published:
  work-logs/<YYYY>/<YYYY-MM-DD Topic Name>.md  (<N> words)
  <commit sha> pushed to brightbandtech/daniel-misc

Topic: <title>
Covers: <N commits, N PRs, ...>
```

If the push failed, lead with that instead:

```
Work digest written but NOT PUBLISHED (<reason>):
  Durable copy:  <path from 6a>  (<N> words)
  Pending commit: <sha> in <temp clone path>  (retry: git -C <path> push)
```

---

## Ingesting locally

For the local agent, on the machine with the Obsidian vault. Ask it to:

> Pull the latest `work-logs` from daniel-misc and fold any new digests into my daily
> log and literature notes.

Read `work-logs/_INDEX.md` for the human-readable list. To find what has *not* been
ingested yet, use git history rather than the index — it is authoritative about what
arrived when, regardless of which machine wrote it:

```bash
git log --since="7 days ago" --name-only --diff-filter=A --pretty=format: -- work-logs/ \
  | rg -v '_INDEX|gitattributes' | sort -u
```

Each digest's `Ingest Hints` block carries the suggested daily-log bullet, note title,
destination, and tags — the local agent should verify those against the vault (the
remote agent was guessing) and then follow the `log-work` conventions to write them.

Digests are never deleted or edited after ingest. The repo is the durable append-only
record; the vault is the curated one.
