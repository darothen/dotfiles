---
name: weekly-review
description: >
  Run this skill on Friday afternoons to write a retrospective in Daniel's weekly Obsidian note.
  Triggers on: "weekly review", "end of week review", "Friday wrap-up", "what did I accomplish
  this week", "week recap", "EOW", or any request to summarize or review the current week's work.
  Reads all daily logs, Linear activity, GitHub PRs and reviews, Todoist completions, and peeks
  at next week's calendar. Always use this skill at the end of the week or when Daniel asks to
  reflect on the week's work. Designed for Claude Code.
compatibility: "Requires MCP servers (linear, google-calendar) and gh, td CLIs. See references/setup.md."
---

# Weekly Review Skill

Populates the End-of-Week Review section of Daniel's weekly Obsidian note every Friday.
Evidence-based — reads what actually happened across Linear, GitHub, Todoist, and daily logs.
Run inside Claude Code.

> **First time?** See `references/setup.md` for MCP and CLI setup.

## Vault
`~/Documents/workspace/weekly/YYYY/YYYY-MM-DD.md` (Monday's date)

---

## Workflow

### 1. Determine Monday's date and find the weekly note
```bash
MONDAY=$(python3 -c "
import datetime
t = datetime.date.today()
print(t - datetime.timedelta(days=t.weekday()))
")
YEAR=$(echo $MONDAY | cut -d- -f1)
NOTE=~/Documents/workspace/weekly/$YEAR/$MONDAY.md
test -f "$NOTE" && cat "$NOTE" || echo "MISSING"
```

**If MISSING** — warn Daniel and suggest running `/weekly-plan` first.

### 2. Read all daily logs for the week
```python
import datetime, pathlib

today = datetime.date.today()
monday = today - datetime.timedelta(days=today.weekday())
vault = pathlib.Path("~/Documents/workspace/daily").expanduser()

for i in range(5):
    d = monday + datetime.timedelta(days=i)
    p = vault / str(d.year) / f"{d.strftime('%Y-%m-%d')}.md"
    if p.exists():
        print(f"\n=== {d.strftime('%A %Y-%m-%d')} ===")
        print(p.read_text())
    else:
        print(f"\n=== {d.strftime('%A %Y-%m-%d')} — no log ===")
```

### 3. Fetch Linear activity for the week
Use the **linear** MCP to find:
- Issues whose status changed this week (moved to Done, In Review, etc.)
- Issues where Daniel commented this week
- New issues created this week
- Issues still open that were In Progress at the start of the week (carryover candidates)

### 4. Fetch GitHub activity for the week
```bash
MONDAY=$(python3 -c "import datetime; t=datetime.date.today(); print(t - datetime.timedelta(days=t.weekday()))")

# PRs opened or merged this week
gh pr list --author "@me" --state all --limit 20

# PR reviews submitted this week
gh api "search/issues?q=reviewed-by:@me+updated:>=${MONDAY}&type=pr" \
  --jq '.items[] | "\(.title) - \(.html_url)"'
```

### 5. Fetch Todoist activity for the week
```bash
MONDAY=$(python3 -c "import datetime; t=datetime.date.today(); print(t - datetime.timedelta(days=t.weekday()))")

# Completed this week
td completed list --since $MONDAY
td completed list --since $MONDAY --project "Professional"

# Still open (carryover candidates)
td inbox
td task list --project "Professional"
```

### 5.5. Vault task system (inbox + master)
```bash
T=~/.claude/skills/task-triage/scripts/vault_tasks.py
for v in inbox stale next waiting someday; do echo "$v: $(python3 $T $v --count)"; done
python3 $T next                    # #next tasks still open → Carryover candidates
python3 $T done --since $MONDAY    # TODOs completed this week
```
Completed `TODO`s this week count as Accomplishments (same evidence standard as Linear/PRs).
Open `#next` and In Progress (`doing`) tasks feed Carryover. Spec: `~/Documents/workspace/projects/Task Management System/Reference.md`. Report the counts in "Things to Remember" as one line
(e.g. "Task inbox: 4 untriaged (1 stale) · 9 next actions · 3 waiting"). In step 9, if the
inbox isn't empty or anything is stale/waiting >14 days, offer `/task-triage` (workflow D,
Prune) as the weekly inbox sweep.

### 6. Peek at next week's calendar
Use the **google-calendar** MCP to list events for next Monday–Friday.
Apply the standard event filter. Identify any big meetings or known deadlines worth
noting as context for next week's planning.

### 7. Synthesize the review

**Accomplishments**: What actually shipped or moved forward this week. Group by theme
(e.g., "Infrastructure", "Hiring", "Data"). Draw from Linear status changes, merged PRs,
and completed Todoist tasks. Evidence-based only — no aspirational items.

**Carryover**: Open Linear issues and Todoist tasks that were in-flight but not completed.
One line each with a brief honest note on why it's carrying over.

**Decisions made**: Key decisions from the week, sourced from meeting notes in daily logs.
Worth capturing so next Monday's context isn't lost.

**Things to remember**: Loose ends, follow-ups, or context useful to have next Monday.

**Next week preview**: 1–2 lines on what's already visible on next week's calendar that
warrants awareness or prep.

**Reflection** (optional): Leave as a blank comment for Daniel to fill in himself.

### 8. Write into the weekly note
```python
import re, pathlib, datetime

today = datetime.date.today()
monday = today - datetime.timedelta(days=today.weekday())
path = pathlib.Path(
    f"~/Documents/workspace/weekly/{monday.year}/{monday.strftime('%Y-%m-%d')}.md"
).expanduser()
content = path.read_text()

review_section = """# Week in Review

### Accomplishments
{accomplishments}

### Carryover
{carryover}

### Decisions Made
{decisions}

### Things to Remember
{things_to_remember}

### Next Week
{next_week_preview}

### Reflection
<!-- Fill in yourself -->
"""

content = re.sub(
    r'# Week in Review\n.*',
    review_section,
    content, flags=re.DOTALL
)
path.write_text(content)
```

### 9. Present and offer next step
Show the populated review section in chat. Ask:
- "Does this look right? Anything I missed?"
- "Want me to create next week's note now?" (triggers `/weekly-plan` proactively if yes)

---

## Output format

```markdown
# Week in Review

### Accomplishments
- **<Theme>**: <what shipped or moved forward>

### Carryover
- [ ] <Linear IIF-XX: title> — <why carrying over>
- [ ] [Todoist] <task> — <why carrying over>

### Decisions Made
- <Decision + brief context>

### Things to Remember
- <Follow-up / loose end>

### Next Week
- <Notable meeting or deadline already visible>

### Reflection
<!-- Fill in yourself -->
```

---

## Edge cases
- **No daily logs found**: Rely on Linear/GitHub/Todoist signals; note logs were missing.
- **Weekly note missing**: Warn Daniel, suggest running `/weekly-plan` first.
- **Run on non-Friday**: Warn but proceed if confirmed. Mark as mid-week snapshot.
- **Week in Review already filled**: Show existing content, ask to regenerate or append.
