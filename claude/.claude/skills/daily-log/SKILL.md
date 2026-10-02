---
name: daily-log
description: >
  Run this skill every weekday morning to populate Daniel's daily Obsidian log note.
  Triggers on: "morning setup", "start my day", "prep my day", "daily log", "populate today's note",
  or any request to set up or scaffold today's note in Obsidian. The note must already exist —
  created via the Obsidian Calendar plugin before invoking this skill. This skill fills in the
  existing note with today's Google Calendar events, linked Google Docs, In Progress/In Review/Todo Linear issues,
  @next Todoist tasks, GitHub PRs (authored by or review-assigned to darothen), action items from meeting docs,
  starred/Needs-Review emails, and
  any unchecked Focus items carried over from yesterday. Always use this skill when Daniel
  asks to prepare his day or populate his calendar for the day. Designed for Claude Code.
compatibility: "Requires MCP servers (google-calendar, google-drive, linear, gmail) and td + gh CLIs. See references/setup.md."
---

# Daily Log Skill

Populates Daniel's daily Obsidian note every morning. Run inside Claude Code.
The note must be created first via the Obsidian Calendar plugin.

> **First time?** See `references/setup.md` for MCP and CLI setup.

## Vault
`~/Documents/workspace/daily/YYYY/YYYY-MM-DD.md`

---

## Workflow

### 1. Check the note exists
```bash
NOTE=~/Documents/workspace/daily/$(date +%Y)/$(date +%Y-%m-%d).md
test -f "$NOTE" && echo "EXISTS" || echo "MISSING"
```

**If MISSING** — stop and tell Daniel:
> ⚠️ **Today's note doesn't exist yet!** Please create it in Obsidian using the Calendar
> plugin, then re-run this skill.

### 2. Read today's note + carry-over from the prior workday
```bash
cat ~/Documents/workspace/daily/$(date +%Y)/$(date +%Y-%m-%d).md
```
Note which section headings exist so content lands in the right places.

Then compute the **prior workday** (Mon → previous Friday; otherwise yesterday) and read
that note if it exists. From its `## Focus` section, extract any unchecked `- [ ]` bullets
— these are the carry-over **candidates**. Skip checked `- [x]` bullets. If the file doesn't
exist, skip silently.

```bash
# Prior workday: Python handles the weekend rollback
python3 -c "
import datetime
d = datetime.date.today() - datetime.timedelta(days=3 if datetime.date.today().weekday()==0 else 1)
print(f'{d.year}/{d.strftime(\"%Y-%m-%d\")}')
"
```

Don't cap yet — capping happens after dedup (see step 8.5), once the fresh Linear/GitHub/
Todoist/Email lists are available to compare against.

### 2.5. Pull in remote-agent work digests
Agents on remote VMs publish work digests to `brightbandtech/daniel-misc` under
`work-logs/` (via the `work-digest` skill). Run the **log-work** skill's *"Finding
un-ingested digests"* procedure (Digest Ingest Mode) now, so yesterday's remote work is in
the vault before you plan today. It ingests each digest into the note for its own date and
backfills that day's `## Summary` if one exists.

Do this **before** step 9 so any follow-ups from digests can be weighed alongside the
fresh Linear/GitHub/Todoist items. Digest "Follow-ups" are not Focus items on their own;
mention them in the step 10 confirmation and only promote one if it duplicates nothing else
and Daniel would plausibly act on it today.

### 3. Fetch today's calendar events
Use the **google-calendar** MCP `list_events` tool with `startTime`/`endTime` ISO 8601
timestamps (NOT a `date` field — the tool does not accept a bare date):

```
list_events(
  startTime="YYYY-MM-DDT00:00:00",
  endTime="YYYY-MM-DDT23:59:59",
  timeZone="America/Denver",
  pageSize=20
)
```

Fetch the next page if `nextPageToken` is present. Apply the event filter (see
`references/setup.md`). Sort remaining events chronologically.

### 4. For each included meeting
- Extract: title, start time, `recurringEventId` (if present), any URLs in the event
  description (Google Docs, Notion, Linear, etc.)
- If there is a linked Google Doc, use the **google-drive** MCP to read the doc:
  - Generate a **1-sentence summary** of the meeting's purpose from the first ~200 words.
  - If the event is **recurring** (has `recurringEventId`), also scan the doc for unchecked
    action items from the prior occurrence. See ## Action Item Detection below.
    - **Open from last time** (all relevant open items): list under the meeting block as
      `**Open from last time:**` bullets.
    - **Focus list** (Daniel-assigned only): only collect items explicitly assigned to Daniel
      into `meeting_action_items`. An item is Daniel-assigned if it contains `@Daniel`,
      `@daniel`, `@DR`, `[Daniel]`, `[Daniel Rothenberg]`, or is prefixed `Daniel →` / `Daniel:`.
      For each qualifying item, store as a tuple `(doc_url, meeting_title, action_text)` so
      the Focus entry can link to the source doc.
- If the doc is unavailable or empty, leave the summary line blank and skip follow-up detection.

### 5. Fetch open Linear issues
Use the **linear** MCP to fetch issues assigned to Daniel with status **In Progress**,
**In Review**, or **Todo**, sorted by priority, with `updatedAt: -P14D` to exclude issues
not touched in the last two weeks. Run three separate queries (one per state) and merge,
ordering: In Progress → In Review → Todo, each sorted by priority within its group.

### 6. Fetch Todoist tasks (with URLs)
```bash
td task list --filter "@next" --json --show-urls 2>/dev/null
```

Parse the JSON. Surface all tasks tagged `@next`.
For each task, use the `webUrl` field (short stable form) to render a markdown link:

```markdown
- [ ] [Todoist] [<content>](<webUrl>) (p<N>[, due <date>])
```

If `webUrl` is absent, fall back to `url`. Never emit plain text for Todoist items — the
link is the reference.

### 7. Fetch open GitHub PRs (authored + review-requested)
```bash
# Authored by darothen
gh search prs --author "darothen" --state open \
  --json number,title,url,isDraft,updatedAt,repository

# Candidates for review — darothen picked up by any mechanism
gh search prs --review-requested "darothen" --state open \
  --json number,title,url,isDraft,updatedAt,repository,author
```

- **Authored**: exclude Draft PRs and any updated more than 14 days ago. Render as
  `- [ ] [PR] [<title>](<url>) (<repo>)`, most recently updated first.
- **Review-requested**: for each candidate PR, call the GitHub API to verify `darothen`
  appears in `requested_reviewers` (not just `requested_teams`). Exclude team-only
  assignments, Draft PRs, and already-merged PRs. Render survivors as
  `- [ ] [Review] [<title>](<url>) (<repo>, by @<author>)`, most recently updated first.

```bash
# Verify direct reviewer assignment for a specific PR
gh api repos/<owner>/<repo>/pulls/<number> \
  --jq '[.requested_reviewers[].login] | contains(["darothen"])'
```

If either list is empty, skip its block entirely (no placeholder).

### 8. Fetch Gmail starred / Needs Review
Use the **gmail** MCP:
```
search_threads(query='is:starred OR label:"Needs Review"', pageSize=20)
```

Dedupe threads that match both filters. For each thread, extract subject, sender, and a
direct thread URL (`https://mail.google.com/mail/u/0/#inbox/<threadId>`). Cap at **5
items** so Focus stays focused. Render as:

```markdown
- [ ] [Email] [<subject>](<thread-url>) — <sender-name>
```

If Gmail MCP is unavailable or returns nothing, skip this block silently.

### 8.5. Dedupe carry-over against today's fresh fetches

Carry-over candidates (from step 2) almost always reappear naturally in the Linear/GitHub/
Todoist/Email lists you just fetched, since they're still open — listing them twice just
clutters Focus. Before capping:

1. Collect every URL appearing in today's fresh `linear_issues`, `authored_prs`,
   `review_prs`, `todoist_tasks`, and `starred_emails` lists into one set.
2. For each carry-over candidate, extract the first markdown link's URL (the href inside
   the first `[...](...)` in the bullet). If that URL is in the set from step 1, drop the
   candidate — it will already surface today under its native section.
3. Cap the **surviving** candidates at 5 to keep Focus tight.
4. Track two counts for the confirm step: how many were dropped as duplicates, and how many
   (if any) were dropped solely by the 5-item cap.

### 9. Write content into the note
Focus is rendered as sub-sections (omit any sub-section that is entirely empty):

```python
import pathlib, datetime, re

today = datetime.date.today()
path = pathlib.Path(
    f"~/Documents/workspace/daily/{today.year}/{today.strftime('%Y-%m-%d')}.md"
).expanduser()
content = path.read_text()

# Build each sub-section; skip if empty
sections = []

carry_lines = [f"- [ ] [↻] {item}" for item in carryover]
if carry_lines:
    sections += ["### Carry-over", *carry_lines, ""]

# meeting_action_items is a list of (doc_url, meeting_title, action_text)
meeting_lines = [
    f"- [ ] [Meeting] [{action_text}]({doc_url}) ({meeting_title})"
    for doc_url, meeting_title, action_text in meeting_action_items
]
if meeting_lines:
    sections += ["### Meetings", *meeting_lines, ""]

if linear_issues:
    sections += ["### Linear", *[f"- [ ] {issue}" for issue in linear_issues], ""]

if todoist_tasks:
    sections += ["### Todoist", *[f"- [ ] [Todoist] {task}" for task in todoist_tasks], ""]

github_lines = [
    *[f"- [ ] [PR] {pr}" for pr in authored_prs],
    *[f"- [ ] [Review] {pr}" for pr in review_prs],
]
if github_lines:
    sections += ["### GitHub", *github_lines, ""]

if starred_emails:
    sections += ["### Email", *[f"- [ ] [Email] {email}" for email in starred_emails], ""]

focus_block = "\n".join(sections).rstrip()

content = re.sub(
    r'(## Focus\n).*?(\n## |\n# |\Z)',
    lambda m: m.group(1) + focus_block + '\n' + m.group(2).lstrip('\n'),
    content, flags=re.DOTALL,
)

# Meetings: each block may include an Open from last time: subsection
meetings_block = "\n\n".join(meeting_blocks)
content = re.sub(
    r'(## Meetings\n).*?(\n## |\n# |\Z)',
    lambda m: m.group(1) + meetings_block + '\n' + m.group(2).lstrip('\n'),
    content, flags=re.DOTALL,
)

path.write_text(content)
```

Section headings `## Focus` and `## Meetings` exist in the template as empty blocks.
If they're unexpectedly absent, append with clear headers and warn Daniel.

### 10. Confirm
- Meetings included (time + title, one per line), plus count of follow-up items surfaced
- Events skipped by the filter
- Remote work digests ingested in step 2.5 (title · machine · date · where it landed), plus
  any backfilled past-day summaries; omit if none
- Focus item counts: e.g. "2 Carry-over · 1 Meeting · 3 Linear · 2 Todoist · 1 PR · 2 Reviews · 3 Email"
- If any carry-over candidates were dropped in step 8.5, note both counts, e.g.
  "(4 duplicates of today's Linear/GitHub items skipped, 1 more truncated by the 5-item cap)"

---

## Meeting Block Format

**With a linked Google Doc (no follow-ups):**
```markdown
### <HH:MM> – [<Meeting title>](<google-doc-url>)

<1-sentence summary of purpose or agenda>

```

**With a linked Google Doc and carried-over action items:**
```markdown
### <HH:MM> – [<Meeting title>](<google-doc-url>)

<1-sentence summary of purpose or agenda>

**Open from last time:**
- [ ] <action item text>
- [ ] <action item text>

```

**Without a linked doc:**
```markdown
### <HH:MM> – <Meeting title>

```

---

## Action Item Detection

Only attempted for **recurring meetings with a linked Google Doc**. The goal is to surface
unchecked items from the most recent prior occurrence, not to summarize the full history.

1. Identify section headings in the doc that look like dated agendas — patterns like
   `## <Mon DD>`, `## <Mon DD> | <title>`, `## <YYYY-MM-DD>`. Collect them in order.
2. Find the most recent section dated **before today**.
3. Within that section, extract bullets that look like action items:
   - Markdown `- [ ]` / `- [x]` checkboxes (take only unchecked)
   - Lines starting with `**Action:**`, `AI:`, `TODO:`, or `Action item:`
   - Lines containing `@Daniel`, `@daniel`, or `@DR` — always include regardless of checkbox state
   These go into **Open from last time** in the meeting block.
   For the **Focus list**, further filter to items explicitly assigned to Daniel:
   must contain `@Daniel`, `@daniel`, `@DR`, `[Daniel]`, `[Daniel Rothenberg]`,
   or begin with `Daniel →` / `Daniel:`.
4. Cap at 3 items per meeting for Open from last time. Clean up whitespace and truncate each to ~120 chars.
5. If nothing parses cleanly, skip rather than guess — don't fabricate follow-ups.

---

## Edge cases
- **Note missing**: Stop. Tell Daniel to create it in Obsidian first.
- **No meetings today**: Insert `> No meetings today.` into the Meetings section.
- **Focus or Meetings headings absent**: Append content with clear headers and warn Daniel.
- **All PRs are drafts**: Skip the PR block entirely; don't add a placeholder.
- **MCP unavailable**: Skip that integration, fill what's available, report what's missing.
- **Prior workday note missing**: Skip carry-over silently.
- **Doc too large for single read**: Summary still works from first ~2000 chars; action-item
  detection may fail — that's acceptable, just skip it for that meeting.

---

## Companion skill
At end of day, run `/daily-summary` to write the EOD Summary and generate a standup message.
