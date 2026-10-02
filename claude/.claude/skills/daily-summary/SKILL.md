---
name: daily-summary
description: >
  Run this skill at end of day or before standup to generate a short 2-4 sentence summary of
  what Daniel worked on. Triggers on: "daily summary", "end of day summary", "EOD", "what did I
  work on today", "write my standup", "recap my day", or any request to summarize the day's work
  for sharing with the team. Scans Gmail for Gemini meeting notes and patches summaries and
  action items into the daily log's meeting blocks, checks brightbandtech/daniel-misc for
  remote-agent work digests and folds any in via log-work, then reads Linear activity, GitHub
  PRs and reviews, and Todoist completions to build a concise, human-sounding summary. Always
  use this skill when Daniel wants to prepare for standup or recap his day. Designed for
  Claude Code.
compatibility: "Requires MCP servers (gmail, linear) and gh, td CLIs. See references/setup.md."
---

# Daily Summary Skill

Generates a short, honest summary of Daniel's day for standup. Evidence-based — reads what
actually happened rather than what was planned. Run inside Claude Code.

> **First time?** See `references/setup.md` for MCP and CLI setup.

## Vault
`~/Documents/workspace/daily/YYYY/YYYY-MM-DD.md`

---

## Workflow

### 1. Read today's daily log
```bash
cat ~/Documents/workspace/daily/$(date +%Y)/$(date +%Y-%m-%d).md
```
Focus on:
- The **# Notes** section — freeform notes written during the day (everything between
  `# Notes` and `## Meetings`)
- The **## Summary** section (may already have content — see edge cases)
- Which meeting blocks had their summary line filled in (signals real engagement)

### 2. Scan Gmail for Gemini meeting notes

Search for Gemini-generated meeting notes sent today:

```
search_threads(query='from:gemini-notes@google.com newer_than:1d', pageSize=20)
```

For each thread returned, the subject follows the pattern `Notes: "<Meeting Title>" <Date>`.
Extract the meeting title and match it against meeting blocks in today's log. A meeting block
looks like `### HH:MM – [<Title>](...)` or `### HH:MM – <Title>`.

**Matching**: normalize both sides to lowercase, strip punctuation, and check for substring
containment — e.g. `"data prioritization (weekly)"` matches the block
`### 10:15 – [Data prioritization (Weekly)](...)`.

For each matched thread, fetch the full content:
```
get_thread(threadId=<id>, messageFormat='FULL_CONTENT')
```

From the plaintext body, extract:
1. **Summary paragraphs** — the section between `Summary` and `Suggested next steps`
   (or end of body). Condense to 1–2 sentences covering the main topics discussed.
2. **Action items for Daniel** — lines in `Suggested next steps` that contain
   `[Daniel Rothenberg]` or `[Daniel]`. Strip the assignee prefix; keep the action text.
   Cap at 5 items.

**Update the meeting block in the daily log** — for each matched meeting, append after
any existing content in that block (but before the next `###` heading):

```markdown
**Today:** <1–2 sentence summary of what was discussed>

**Action items:**
- [ ] <action item text>
```

Omit the **Action items:** block entirely if no items are assigned to Daniel.

**Skip** any meeting block that already contains `**Today:**` (idempotent — don't
overwrite if this step already ran or was done manually).

Use a Python regex substitution targeting each meeting block individually:

```python
import re, pathlib, datetime

today = datetime.date.today()
path = pathlib.Path(
    f"~/Documents/workspace/daily/{today.year}/{today.strftime('%Y-%m-%d')}.md"
).expanduser()
content = path.read_text()

for meeting_title, summary, action_items in matched_meetings:
    # Build the block to append
    today_block = f"\n**Today:** {summary}"
    if action_items:
        items = "\n".join(f"- [ ] {a}" for a in action_items)
        today_block += f"\n\n**Action items:**\n{items}"

    # Match the meeting section: from the ### heading to the next ### or ##
    pattern = rf'(### \d{{2}}:\d{{2}}[^\n]*{re.escape(meeting_title)}[^\n]*\n(?:(?!### |\*\*Today:\*\*).)*?)(### |\*\*Today:\*\*|## |\Z)'
    content = re.sub(
        pattern,
        lambda m: m.group(1).rstrip() + today_block + '\n\n' + m.group(2),
        content, flags=re.DOTALL, count=1,
    )

path.write_text(content)
```

Report which meetings were updated, which were skipped (already had **Today:**), and
which Gemini emails had no matching block in the log.

### 3. Check for remote-agent work digests

Some sessions run on remote/cloud machines with no vault access; instead of writing
directly to Obsidian, they publish a curated 1–2 page Markdown summary (a "work digest":
Task / Approach / Key Results / Key Learnings / Outcomes / Follow-ups) to
`brightbandtech/daniel-misc` under `work-logs/YYYY/` (written by the `work-digest` skill
on the remote end).

Run the **log-work** skill's *"Finding un-ingested digests"* procedure (Digest Ingest
Mode). It scans the last 14 days of the index — not just today — dedups against the whole
vault, and ingests each new digest into the note for its own date, backfilling older days'
`## Summary` callouts when they exist. This is the same procedure `daily-log` runs in the
morning, so at EOD it usually only picks up digests that landed during the day.

Digests dated today (new or already ingested earlier by `daily-log`) are signal for this
summary: carry their **Key Results** and **Outcomes & Status** forward into Step 8's
synthesis — a PR merged or a benchmark completed on a remote machine is exactly the kind of
notable output the EOD summary should mention. Don't skip a digest as input just because
`daily-log` already ingested it.

If nothing new is found, or `gh` fails, skip silently — remote digests are a bonus signal,
not a requirement.

### 4. Re-read today's daily log

After the meeting-notes patch and any digest ingestion, re-read the note so the meeting
summaries and freshly-added digest bullets are available as context for the synthesis
step.

### 5. Check Linear for today's activity
Use the **linear** MCP to find:
- Issues whose status changed today (moved to In Review, Done, etc.)
- Issues where Daniel added a comment today
- New issues created today

### 6. Check GitHub for today's activity
```bash
# PRs opened or updated today
gh pr list --author "@me" --state all --limit 10

# PR reviews submitted today
gh api "search/issues?q=reviewed-by:@me+updated:>=$(date +%Y-%m-%d)&type=pr" \
  --jq '.items[] | .title'
```

### 7. Check Todoist for today's completions
```bash
# Completions today (all projects)
td completed list --since $(date +%Y-%m-%d)
```

### 8. Synthesize the summary
Write 2–4 sentences in first-person, casual professional tone:
1. Main focus area or theme of the day
2. Notable outputs (PR opened/merged, issue closed, decision made, task completed) —
   including any from remote-agent digests ingested in Step 3

**Tone**: Sound like a real person talking to their team, not a status report.
Avoid bullet points — prose only. Example:
> "Spent most of today on the Zarr compression pass — got int16 encoding working for the
> pressure fields and opened a PR for review. Also had a good sync with the Argonne team
> about EWB extensions."

### 9. Write into the daily log
Write the summary as an Obsidian callout under `## Summary`:
```python
import re, pathlib, datetime

today = datetime.date.today()
path = pathlib.Path(
    f"~/Documents/workspace/daily/{today.year}/{today.strftime('%Y-%m-%d')}.md"
).expanduser()
content = path.read_text()

summary_text = "<generated summary>"
callout = f"> [!summary]\n> {summary_text}"

# Replace the section content; do not overwrite if already filled
content = re.sub(
    r'(## Summary\n)(?!> \[!)',  # only replace if no callout already present
    rf'\g<1>{callout}\n',
    content, flags=re.DOTALL
)
path.write_text(content)
```

### 10. Present to Daniel
Show the summary in a copyable block, and separately list which (if any) remote work
digests were ingested in Step 3 and where each landed in the vault.

---

## Edge cases
- **# Notes section empty**: Rely on Linear/GitHub/Todoist signals and meeting summaries. Note the log was empty.
- **No signals from any source**: Ask Daniel briefly what he worked on, then write it up.
- **## Summary already filled**: Show existing content and ask to regenerate or keep as-is.
- **Gmail MCP unavailable**: Skip step 2 silently; proceed with synthesis from other sources.
- **No Gemini notes found**: Skip step 2 silently; note the absence in the confirmation output.
- **Meeting block already has `**Today:**`**: Skip that block — do not overwrite content written manually or from a prior run.
- **Gemini email with no matching meeting block**: Log a warning to the user ("Found Gemini notes for '<Title>' but no matching meeting block in today's log") and skip.
- **daniel-misc repo/work-logs unreachable or `gh` unauthenticated**: Skip step 3 silently; note in the final confirmation that remote digests weren't checked.
- **Digest references a PR/issue number that doesn't resolve, or a "related note" that doesn't exist**: Drop that specific claim rather than inventing a link; the rest of the digest can still be ingested.
