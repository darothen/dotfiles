#!/usr/bin/env python3
"""Read-only index of the Obsidian GTD task system (Tasks-plugin lines containing ``TODO``).

Mirrors the views in ``tasks/GTD.md`` and ``tasks/Master.md``; the spec is
``projects/Task Management System/Reference.md`` in the vault. Never writes to the vault —
callers edit the reported ``file:line`` directly.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import re
import sys
from collections.abc import Iterator
from dataclasses import asdict, dataclass, field
from pathlib import Path

VAULT = Path.home() / "Documents" / "workspace"
SKIP_DIRS = {".obsidian", ".trash", "templates", "attachments"}
TASK_RE = re.compile(r"^(?P<indent>\s*)[-*] \[(?P<status>.)\] (?P<body>.*)$")
HEADING_RE = re.compile(r"^(#{1,6})\s+(.*)$")
STATUS = {
    " ": "Inbox",
    "t": "Todo",
    "/": "In Progress",
    "w": "Waiting",
    "s": "Someday",
    "x": "Done",
    "X": "Done",
    "-": "Cancelled",
}
CLOSED = {"Done", "Cancelled"}
ACTIVE = {"Todo", "In Progress", "Waiting"}
STATUS_ORDER = ["In Progress", "Todo", "Waiting", "Inbox", "Someday", "Done", "Cancelled"]
PRIORITIES = {"🔺": "highest", "⏫": "high", "🔼": "medium", "🔽": "low", "⏬": "lowest"}
PRIORITY_RANK = {"highest": 0, "high": 1, "medium": 2, None: 3, "low": 4, "lowest": 5}
DATE_FIELDS = {"📅": "due", "⏳": "scheduled", "🛫": "start", "➕": "created", "✅": "done", "❌": "cancelled"}
DATE_RE = re.compile(r"(📅|⏳|🛫|➕|✅|❌)\s*(\d{4}-\d{2}-\d{2})")
TAG_RE = re.compile(r"(?<![\w/])#([A-Za-z][\w/-]*)")
VIEWS = (
    "inbox",
    "todo",
    "doing",
    "waiting",
    "someday",
    "tickler",
    "next",
    "due",
    "active",
    "stale",
    "unprojected",
    "all",
    "done",
    "bare",
)


@dataclass
class Task:
    """One task line in the vault, plus its indented notes."""

    file: str
    line: int
    symbol: str
    status: str
    text: str
    heading: str | None
    priority: str | None = None
    tags: list[str] = field(default_factory=list)
    due: str | None = None
    scheduled: str | None = None
    start: str | None = None
    created: str | None = None
    done: str | None = None
    cancelled: str | None = None
    age_days: int | None = None
    notes: list[str] = field(default_factory=list)

    @property
    def is_open(self) -> bool:
        """Whether the task still needs doing (not Done/Cancelled)."""
        return self.status not in CLOSED

    def tagged(self, prefix: str) -> list[str]:
        """Slugs of tags under a hierarchy, e.g. ``tagged('project')`` for ``#project/<slug>``."""
        p = prefix.strip("#/") + "/"
        return [t[len(p) :] for t in self.tags if t.startswith(p)]

    def has_tag(self, tag: str) -> bool:
        """Exact tag or any descendant (``project`` matches ``project/x``)."""
        t0 = tag.lstrip("#")
        return any(t == t0 or t.startswith(t0 + "/") for t in self.tags)


def _file_date(path: Path) -> dt.date | None:
    try:
        return dt.date.fromisoformat(path.stem)
    except ValueError:
        return None


def _iter_md(vault: Path) -> Iterator[Path]:
    for p in vault.rglob("*.md"):
        if not SKIP_DIRS.intersection(p.relative_to(vault).parts):
            yield p


def _indent(s: str) -> int:
    e = s.expandtabs(4)
    return len(e) - len(e.lstrip())


def _children(lines: list[str], idx: int, base: int) -> list[str]:
    out: list[str] = []
    for nxt in lines[idx + 1 :]:
        if not nxt.strip():
            continue
        if _indent(nxt) <= base:
            break
        out.append(nxt.strip())
    return out


def scan(vault: Path = VAULT, *, bare: bool = False) -> list[Task]:
    """Parse every task line in the vault.

    Args:
        vault: Vault root.
        bare: If True, instead return open plain checkboxes (no ``TODO``) in daily-note
            ``# Notes`` sections — candidates for promotion to tasks.

    Returns:
        Parsed tasks, each with its indented child lines as ``notes``.
    """
    today = dt.date.today()
    out: list[Task] = []
    for path in _iter_md(vault):
        heading: str | None = None
        in_notes = False
        fdate = _file_date(path)
        lines = path.read_text(errors="replace").splitlines()
        for i, line in enumerate(lines):
            if h := HEADING_RE.match(line):
                heading = h.group(2).strip()
                if len(h.group(1)) == 1:
                    in_notes = heading == "Notes"
                elif heading in ("Meetings", "Focus", "Summary", "Tasks"):
                    in_notes = False
                continue
            m = TASK_RE.match(line)
            if not m:
                continue
            body, sym = m.group("body"), m.group("status")
            if not re.search(r"\w", re.sub(r"\bTODO\b", "", body)) or "placeholder" in body:
                continue
            has_kw = re.search(r"\bTODO\b", body) is not None
            if bare:
                if has_kw or sym != " " or not (in_notes and fdate):
                    continue
            elif not has_kw:
                continue
            dates = {DATE_FIELDS[e]: d for e, d in DATE_RE.findall(body)}
            born = dates.get("created") or (fdate.isoformat() if fdate else None)
            out.append(
                Task(
                    file=str(path.relative_to(vault)),
                    line=i + 1,
                    symbol=sym,
                    status=STATUS.get(sym, f"Unknown[{sym}]"),
                    text=re.sub(r"\bTODO\b\s*", "", body, count=1).strip(),
                    heading=heading,
                    priority=next((n for e, n in PRIORITIES.items() if e in body), None),
                    tags=TAG_RE.findall(body),
                    due=dates.get("due"),
                    scheduled=dates.get("scheduled"),
                    start=dates.get("start"),
                    created=born,
                    done=dates.get("done"),
                    cancelled=dates.get("cancelled"),
                    age_days=(today - dt.date.fromisoformat(born)).days if born else None,
                    notes=_children(lines, i, _indent(m.group("indent"))),
                )
            )
    return out


def select(tasks: list[Task], view: str, *, stale_days: int = 30, since: str | None = None) -> list[Task]:
    """Filter tasks to a named view matching the GTD/Master dashboards."""
    if view == "done":
        return [t for t in tasks if t.status == "Done" and (t.done or "") >= (since or "0000")]
    open_ = [t for t in tasks if t.is_open]
    in_week = (dt.date.today() + dt.timedelta(days=7)).isoformat()
    match view:
        case "all" | "bare":
            return open_
        case "inbox":
            return [t for t in open_ if t.status == "Inbox"]
        case "stale":
            return [t for t in open_ if t.status == "Inbox" and (t.age_days or 0) > stale_days]
        case "todo":
            return [t for t in open_ if t.status == "Todo"]
        case "doing":
            return [t for t in open_ if t.status == "In Progress"]
        case "waiting":
            return [t for t in open_ if t.status == "Waiting"]
        case "someday":
            return [t for t in open_ if t.status == "Someday"]
        case "tickler":
            today = dt.date.today().isoformat()
            return [t for t in open_ if t.status == "Someday" and t.scheduled and t.scheduled <= today]
        case "next":
            return [t for t in open_ if t.has_tag("next") and t.status not in ("Waiting", "Someday")]
        case "due":
            return [t for t in open_ if t.due and t.due <= in_week and t.status != "Someday"]
        case "active":
            return [t for t in open_ if t.status in ACTIVE]
        case "unprojected":
            return [t for t in open_ if t.status in ("Todo", "In Progress") and not t.tagged("project")]
    raise ValueError(view)


def _sort_key(t: Task) -> tuple:
    rank = STATUS_ORDER.index(t.status) if t.status in STATUS_ORDER else 9
    return (t.due or "9999", PRIORITY_RANK[t.priority], rank)


def main(argv: list[str] | None = None) -> int:
    """CLI entry point."""
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("view", nargs="?", default="inbox", choices=VIEWS)
    ap.add_argument("--tag", help="only tasks with this tag or a descendant: who/hans, project/nnja-ai-v2, next")
    ap.add_argument("--json", action="store_true", help="emit JSON (includes indented notes)")
    ap.add_argument("--count", action="store_true", help="print only the count")
    ap.add_argument("--notes", action="store_true", help="text mode: also print indented notes")
    ap.add_argument("--stale-days", type=int, default=30)
    ap.add_argument("--since", help="YYYY-MM-DD lower bound for the done view")
    ap.add_argument("--vault", type=Path, default=VAULT)
    args = ap.parse_args(argv)

    tasks = select(scan(args.vault, bare=args.view == "bare"), args.view, stale_days=args.stale_days, since=args.since)
    if args.tag:
        tasks = [t for t in tasks if t.has_tag(args.tag)]
    tasks.sort(key=_sort_key)
    if args.count:
        print(len(tasks))
    elif args.json:
        rows = [asdict(t) | {"projects": t.tagged("project"), "people": t.tagged("who")} for t in tasks]
        json.dump(rows, sys.stdout, ensure_ascii=False, indent=1)
        print()
    else:
        for t in tasks:
            meta = " ".join(
                x
                for x in (
                    f"[{t.symbol}]{t.status}",
                    t.priority and f"!{t.priority}",
                    t.due and f"due:{t.due}",
                    t.scheduled and f"sched:{t.scheduled}",
                    t.age_days is not None and f"{t.age_days}d",
                )
                if x
            )
            print(f"{t.file}:{t.line}\t{meta}\t{t.text}\t« {t.heading or ''} »")
            if args.notes:
                for n in t.notes:
                    print(f"\t    {n}")
        print(f"-- {len(tasks)} {args.view}{f' tag={args.tag}' if args.tag else ''}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
