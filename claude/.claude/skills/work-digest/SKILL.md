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

For loop or scheduled runs, cover only work since the previous digest. Check the digest
repo (see Step 3) for recent entries from this machine:

```bash
ls -t "$DIGEST_REPO/work-logs/$(date +%Y)"/*.md 2>/dev/null | head -3
```

If a prior digest covers part of this work, reference it by filename rather than
repeating it.

---

## Step 3 — Locate the digest repo

Digests are published to **`brightbandtech/daniel-misc`** (private, default branch
`main`) under `work-logs/`. This repo is cloned during machine initialization, so it
should already be present on any remote host.

Resolve its path in this order:

1. `$DIGEST_REPO` if set
2. First hit from the usual parents:

```bash
for d in ~/software/daniel-misc ~/src/daniel-misc ~/daniel-misc ~/work/daniel-misc; do
  test -d "$d/.git" && export DIGEST_REPO="$d" && break
done
echo "${DIGEST_REPO:-not found}"
```

3. If absent, clone it: `gh repo clone brightbandtech/daniel-misc ~/software/daniel-misc`

If the repo cannot be found *and* cannot be cloned (no network, no credentials), fall
back to `~/agent-digests/` and say so loudly in the final report — an unpublished digest
is invisible to the local ingest agent.

**Path within the repo:** `work-logs/YYYY/YYYY-MM-DD-<kebab-slug>.md`

The slug names the work, not the session — `2026-08-11-prepbufr-decoder-rewrite.md`,
not `2026-08-11-session-3.md`. If that filename is taken, append `-2`, `-3`, … rather
than overwriting; a same-day second digest on the same topic is a different digest.

Because several machines and loops write to this one repo, the `machine:` frontmatter
field is load-bearing — always fill it with the real hostname.

Do **not** create an index or manifest file. Concurrent writers would conflict on it,
and the local agent gets a better answer from `git log` (Step 6).

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

Commit and push the digest. Only ever stage the digest file itself — the repo may hold
unrelated work in progress, and a scheduled agent must never sweep that into a commit.

`work-logs/` does not exist in the repo yet — the first digest creates it, so `mkdir -p`
before writing the file.

```bash
cd "$DIGEST_REPO"
git pull -q                                  # start from current main
mkdir -p "work-logs/$(date +%Y)"
# ...write the digest file here...
git add "work-logs/<YYYY>/<filename>.md"
git commit -q -m "log: <YYYY-MM-DD> <short topic> (<hostname>)"
```

**Pushing is a race.** Several machines and loops publish to this one repo, so a
rejected push is expected, not exceptional. Rebase and retry:

```bash
for i in 1 2 3; do
  git push && break
  git pull --rebase --autostash && sleep $((i * 3))
done
```

Rebasing is safe here because each digest is a new file — two agents publishing at once
touch disjoint paths and cannot textually conflict. If a rebase *does* conflict, stop
and report rather than resolving it; that means something unexpected is in the repo.

**If the push ultimately fails**, leave the commit in place and report the failure
plainly with the local path. The digest still exists on disk and can be pushed later —
never delete it, and never claim it was published when it wasn't.

Push failure is the one outcome most likely to go unnoticed in an unattended loop, so
state it in the first line of the report, not as a footnote.

---

## Step 7 — Report

```
Work digest published:
  work-logs/<YYYY>/<filename>.md  (<N> words)
  <commit sha> pushed to brightbandtech/daniel-misc

Topic: <title>
Covers: <N commits, N PRs, ...>
```

If the push failed, lead with that instead:

```
Work digest written but NOT PUSHED (<reason>):
  <absolute local path>  (<N> words)
  Commit <sha> is staged locally in <repo path> — retry with: git -C <repo> push
```

---

## Ingesting locally

For the local agent, on the machine with the Obsidian vault. Ask it to:

> Pull `daniel-misc` and fold any new `work-logs/` digests into my daily log and
> literature notes.

New digests are found from git history rather than a manifest — no index file to
conflict over, and it works no matter which machine wrote them:

```bash
cd "$DIGEST_REPO" && git pull
# digests added since the last local ingest
git log --since="3 days ago" --name-only --diff-filter=A --pretty=format: -- work-logs/ | sort -u
```

Each digest's `Ingest Hints` block carries the suggested daily-log bullet, note title,
destination, and tags — the local agent should verify those against the vault (the
remote agent was guessing) and then follow the `log-work` conventions to write them.

Digests are never deleted after ingest. The repo is the durable record; the vault is the
curated one.
