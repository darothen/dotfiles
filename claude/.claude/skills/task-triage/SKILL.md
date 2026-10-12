---
name: task-triage
description: >
  Triage, prioritize, prune and query Daniel's org-mode-style GTD task system in Obsidian
  (`TODO` checkbox lines with statuses [ ] inbox, [t] todo, [/] in progress, [w] waiting,
  [s] someday). Triggers on: "/task-triage", "triage my tasks", "process my inbox", "inbox zero",
  "review my tasks", "prune my task list", "what should I work on", "what's next", "what's on my
  plate", "prep for my 1:1 with <person>", "what's waiting on <person>", "tasks for <project>",
  "promote these to tasks", "reopen that task", or when daily-log/daily-summary report a growing
  or stale inbox. Also defines the shared "Task import" procedure other skills use to write
  meeting action items into the vault. Designed for Claude Code.
---

# Task Triage

**Read the spec first:** `~/Documents/workspace/projects/Task Management System/Reference.md`.
It defines the line grammar (§2), statuses (§3), tags (§4), dates (§5), notes (§6) and — most
importantly — the **agent contract (§10)**: what you may change without asking (new Inbox
tasks from imports, source links, notes) vs. only after Daniel approves (status, tags,
priority, dates, completing, cancelling). Never delete task lines, and never write or edit
` ```tasks ` query blocks (JavaScript is enabled in queries — views are Daniel's). This skill only adds
procedures on top of that spec.

```bash
T=~/.claude/skills/task-triage/scripts/vault_tasks.py
python3 $T <view> [--tag who/hans] [--json] [--count] [--notes]
# views: inbox todo doing waiting someday tickler next due active stale unprojected all done(--since D) bare
```

Paths in output are relative to `~/Documents/workspace`. Re-run before each batch of edits —
line numbers shift. Existing tag slugs:
`rg -o '#(project|who)/[\w-]+' ~/Documents/workspace --glob '!templates/**' -N --no-filename | sort | uniq -c | sort -rn`.

## A. Triage the inbox

1. `python3 $T inbox --json`. If empty → "Inbox zero"; offer **C**.
2. Batches of ~6–8, oldest first. Use each task's `heading`, `notes`, and a few surrounding
   lines in its file for context (the heading is usually the meeting it came from — a
   "Hans 1:1" heading suggests `#who/hans`).
3. Propose per task, as a compact table `# · task · → status · tags · dates · why`:
   - `[t]` Todo with `#project/<slug>` (reuse slugs), `#who/<name>` where a person is involved,
     📅 only for real deadlines, ⏳ if Daniel should start on a given day, `#next` only if it
     clearly belongs in his current focus (keep `#next` ≲ 7 total — check `next --count`).
   - `[w]` Waiting + `#who/<name>` of whoever it's blocked on.
   - `[s]` Someday — an idea worth logging but not a commitment; suggest a ⏳ review date
     (e.g. next quarter) so it resurfaces in the 🔔 Tickler.
   - `[-]` Cancel (stale, duplicate, superseded) · `[x]` Done (already happened).
   - Waiting vs Someday: if Daniel still owes the outcome and is blocked on someone, it's
     `[w]` + `#who/<name>`; if it's "maybe later", it's `[s]`.
   - Linear: if it's real team engineering work, suggest a Linear issue; after Daniel confirms
     and it's filed, `[-]` the vault task with the Linear URL appended.
4. Daniel approves/adjusts ("all", "all but 3; 5 → someday"). Apply only what he approved.
5. Report `inbox --count`; continue until empty or he stops.

## B. Promote plain checkboxes

1. `python3 $T bare`. Group near-identical lines (old habit: copying an "Open" list forward
   daily — the same item appears on several dates).
2. For each approved group, convert the **earliest** occurrence to
   `- [ ] TODO <text> ➕ <that file's date>`; leave later copies alone. If a later copy is
   checked, make the promoted task `[x]` instead.
3. Promoted tasks are Inbox → offer **A**.

## C. Focus — "what should I work on?"

1. Gather `due`, `doing`, `next` (JSON), plus today's meetings from today's daily note.
2. Recommend 3–5: overdue/due-soon → in progress → `#next` by priority → aged `#next` with
   no date. Mention calendar constraints.
3. Offer to set ⏳ today (shows in GTD's ▶️ section), `[/]`, or adjust `#next`.

## D. Prune (weekly; offered by weekly-review)

Surface for decisions: `stale` inbox; `waiting` older than 14 days (chase via `#who` or
drop); `next` older than 14 days with no date (still focus?); `unprojected`; `tickler` (Someday items
whose ⏳ review date arrived: promote to `[t]`, push the date, or cancel); `someday` older than
90 days with no review date. Same approve-then-apply loop.

## E. Person / project agenda — "prep for my 1:1 with Hans"

1. `python3 $T all --tag who/hans --notes` (or `--tag project/<slug>`).
2. Present grouped: waiting on them · things to raise with them · their related todos, with
   each task's notes/links. Offer to set the Master page `tag:` property or paste the
   per-tag embed (Reference §8) into the 1:1 note.

## F. Reopen ("I checked that by accident")

Last two days: the "Just closed" sections on GTD/Master handle it by click. Older:
`python3 $T done --since <date>` (or `rg -n '\[-\] .*TODO.*❌'` for cancelled), confirm the
task, set `[t]` and remove the ` ✅ YYYY-MM-DD` / ` ❌ YYYY-MM-DD`.

## Task import (shared procedure for other skills)

Used by `daily-summary` (EOD meeting import) and `daily-log` (backstop) to write
externally-sourced tasks without duplicates. Permitted without asking (Reference §10).

1. Normalize: strip assignee prefixes (`[Daniel Rothenberg]`, `@Daniel`, `Daniel →`), extra
   whitespace and trailing punctuation; make it verb-first if trivial; ≤ ~140 chars.
2. Dedup: `python3 $T all --json` and `python3 $T done --json`; compare case-insensitively on
   the first ~50 alphanumeric chars of `text`; also
   `rg -i -F '<first 40 chars>' ~/Documents/workspace/daily ~/Documents/workspace/notes`
   for cancelled ones. Any match → skip.
3. Emit `- [ ] TODO <text> [↗](<source-url>) ➕ <YYYY-MM-DD>` under the relevant meeting
   heading in the daily note. Status Inbox; no tags, priority or dates — that's triage.
   Useful context from the source (e.g. the linked doc section) may go in indented notes.

## Edge cases

- **Recurring tasks (🔁)**: from the CLI, don't complete them — ask Daniel to click in
  Obsidian so the next instance is generated.
- **Unknown status char** (`Unknown[c]` in indexer output): report it; don't "fix" silently.
- **Task only in a Google Doc**: don't import ad hoc during triage; point to `daily-summary`
  or run Task import for that doc if asked.
- **Spec drift**: if a dashboard, skill or the indexer disagrees with Reference.md, say so and
  propose fixing the outlier rather than following it.
