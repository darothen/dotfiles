# Claude Code Setup for Scheduling Skills

This reference covers the prerequisites for the daily-log, daily-summary, weekly-plan,
and weekly-review skills in Claude Code.

## MCP servers

No configuration needed. These skills use the Google Calendar, Google Drive, Gmail, and
Linear MCPs already connected via the Claude Code /plugins panel. Gmail is used by
`/daily-log` to surface starred threads and those labeled "Needs Review" — create the
label in Gmail if you want to flag threads for the Focus section without starring.

## Required CLIs

| Tool | Purpose | Setup |
|------|---------|-------|
| `gh` | GitHub PRs, commits, reviews | `brew install gh && gh auth login` |
| `td` | Todoist tasks | `npm install -g @doist/todoist-cli && td auth login` |

Verify both are working:
```bash
gh auth status
td auth status
```

## Vault location

`~/Documents/workspace/` — daily notes at `daily/YYYY/YYYY-MM-DD.md`,
weekly notes at `weekly/YYYY/YYYY-MM-DD.md` (Monday's date).

## Event filter (applied in all skills)

Skip any calendar event that is:
- Titled "Standup" (case-insensitive)
- Contains "Daycare" in the title
- An all-day OOO / Out of Office event
- Titled "Weekly Plan" or "Weekly Review"
- Titled "Focus Time" or marked as focus/blocked time
