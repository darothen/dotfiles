# System-Wide Python Project Configuration

This configuration provides intelligent defaults for Python projects using modern tooling (uv, pixi, ruff, pytest, etc.).

## Python Environment Detection

Always detect and use the appropriate environment manager:

1. **Check for `pixi.toml` first** - If present, this is a Pixi workspace:
   - Run commands: `pixi run python`, `pixi run pytest`, `pixi run ruff`, etc.
   - Add dependencies: Edit `pixi.toml`, then run `pixi install`

2. **Otherwise, check for `pyproject.toml`** - This is a uv-managed project:
   - Run commands: `uv run python`, `uv run pytest`, `uv run ruff`, etc.
   - Add dependencies: Edit `pyproject.toml`, then run `uv sync`

3. **Fallback** - Use system commands if neither exists: `python`, `pytest`, `ruff`, etc.

**Example**: If editing code in a project with `pixi.toml`, run tests with `pixi run pytest tests/test_file.py`

## Code Quality Automation

After editing any Python file, automatically run:

1. **Format**: `<env-prefix> ruff format <file>` - Auto-fix formatting issues
2. **Lint**: `<env-prefix> ruff check --fix <file>` - Auto-fix linting issues
3. **Type check** (optional): `<env-prefix> mypy <file>` - Report type issues but don't block

Report any remaining issues concisely. Where `<env-prefix>` is `pixi run` or `uv run` based on environment detection.

## Testing Strategy

Run tests eagerly and smartly after code changes:

1. **Identify changed file**: e.g., `src/package/metrics.py`
2. **Find matching test**: Look for `tests/test_metrics.py` or `tests/package/test_metrics.py`
3. **Run targeted test**: `<env-prefix> pytest tests/test_metrics.py -v`
4. **If no match found**: Check if editing a test file itself - if so, run that test file
5. **Report results**: Show failures clearly, successes briefly

**Example**: Edit `src/weather/metrics.py` → Run `pixi run pytest tests/weather/test_metrics.py -v`

Do not run the entire test suite unless specifically requested. Focus on relevant tests.

## Documentation Standards

Add Google-style docstrings to all new functions and classes:

```python
def calculate_metric(data: np.ndarray, threshold: float = 0.5) -> float:
    """Calculate the primary metric from input data.

    Processes the input array by applying the threshold and computing
    the aggregate score across all dimensions.

    Args:
        data: Input array with shape (samples, features)
        threshold: Minimum value for inclusion in calculation

    Returns:
        Computed metric value as a float

    Raises:
        ValueError: If data array is empty or threshold is negative
    """
```

For existing code, add docstrings when making substantial modifications to undocumented functions.

## Git Commit Style

Use Conventional Commits format:

- `feat: add new metric calculation for precipitation`
- `fix: correct threshold handling in temperature analysis`
- `docs: update API reference for metrics module`
- `refactor: simplify data loading logic`
- `test: add edge cases for empty datasets`
- `chore: update dependencies to latest versions`

Keep commit messages concise and descriptive. Focus on the "what" and "why", not the "how".

## Git Push and PR Workflow

After committing, do not push or open PRs automatically — wait to be asked, or confirm with the user first.

### Pushing

- **New branch**: `git push -u origin <branch-name>`
- **Existing branch**: `git push`
- Never force-push to `main`/`master`

### Pull Requests

Use the `gh` CLI for all GitHub operations (issues, PRs, checks, releases):

```bash
gh pr create --title "feat: short description under 70 chars" --body "$(cat <<'EOF'
## Summary
- Bullet point summary of changes

## Test plan
- [ ] Describe how to verify the changes

🤖 Generated with [Claude Code](https://claude.ai/claude-code)
EOF
)"
```

- Title: Conventional Commits prefix + short description (≤70 chars total)
- Always run from the feature branch, targeting `main`/`master`
- Use `gh pr view`, `gh issue list`, `gh run list` etc. for GitHub inspection tasks

## Dependency Management

Before adding any new dependency:

1. **Identify need**: Determine that a new package is required
2. **Ask user**: "Should I add `<package>` to dependencies? [brief justification]"
3. **Wait for approval**: Do not proceed without explicit confirmation
4. **Classify dependency**:
   - Runtime: Add to `[project.dependencies]` or `[dependencies]`
   - Development: Add to `[project.optional-dependencies.dev]` or `[tool.pixi.dependencies]`
5. **Update environment**:
   - uv: Run `uv sync` after editing `pyproject.toml`
   - pixi: Run `pixi install` after editing `pixi.toml`

## Search Tools

Prefer modern search tools over legacy Unix tools:

- **Use `rg` (ripgrep) instead of `grep`** for searching file contents — it's faster, respects `.gitignore`, and has better defaults
- Example: `rg "pattern" src/` instead of `grep -r "pattern" src/`
- Use `rg --type py "pattern"` to filter by file type
- Use `rg -l "pattern"` to list matching files only

## Tool Ecosystem

### Justfile

If `justfile` or `Justfile` exists in project root:
- Recognize available commands: Read file to identify targets
- Common patterns: `just test`, `just lint`, `just format`, `just release`, `just docs`
- Use Justfile commands when they exist for common tasks

### Pre-commit Hooks

If `.pre-commit-config.yaml` exists:
- Run `pre-commit run --files <changed-files>` before creating commits
- If hooks fail, fix issues and re-stage files
- Do not bypass hooks with `--no-verify` unless explicitly requested

### GitHub Actions

If `.github/workflows/` directory exists:
- Read workflow files to understand CI test patterns
- Match local testing to CI expectations (same pytest args, coverage requirements)
- Recognize deployment and release automation

### MkDocs Documentation

If `mkdocs.yml` exists:
- Recognize `docs/` folder structure
- When making significant code changes, offer to update corresponding documentation
- Follow existing documentation patterns for new pages

## Common Project Patterns

Recognize standard Python project structures:

- **src/ layout**: `src/<package>/` contains source code
- **tests/ layout**: Mirrors src/ structure or flat test files
- **docs/ layout**: MkDocs or Sphinx documentation
- **pyproject.toml**: Project metadata, dependencies, tool configuration
- **pixi.toml**: Pixi workspace with conda-forge dependencies
- **.github/**: CI/CD workflows and GitHub configuration
- **justfile**: Task automation and common commands

## Code Quality Standards

All code must:
- Pass `ruff check` with no errors (warnings acceptable if unavoidable)
- Be formatted with `ruff format` (120 char line length typical)
- Use type hints for function signatures
- Pass existing tests after modifications
- Follow existing code patterns and idioms in the project

Be pragmatic: Follow the existing code style even if it differs slightly from these defaults.

## Obsidian Vault Notes

The Obsidian knowledge base lives at `~/Documents/workspace/`. **Before any Obsidian
operation, check whether `~/Documents/workspace/` exists.** If it does not (e.g., running
on a remote machine), skip all Obsidian logging silently — do not attempt to create the
directory or fall back to any substitute. Record session notes proactively — don't wait to
be asked unless the session is trivial.

### Daily Log

At the end of any meaningful session, append a concise one-liner to today's daily note:

- **Path**: `~/Documents/workspace/daily/YYYY/YYYY-MM-DD.md`
- **Insert** new bullet(s) under the `## Notes` heading, immediately before `# Activity Summary`
- **Format**: `- Brief description of what was accomplished` — if a long-form note was created or
  updated as part of the session, link to it inline: `- Brief description → [[Note Title]]`
- **If the file doesn't exist**: create it by copying `~/Documents/workspace/templates/daily_log.md`,
  replacing all Templater expressions (`<% ... %>`) with today's literal dates and correct nav links

```
# Daily Log

## Notes

- existing item
- ← insert new bullets here

# Activity Summary     ← NEVER touch anything from here down
```

### Long-Form Notes

For significant tools, configurations, research findings, or topics worked on, create or update
a note in `notes/`:

- **Path**: `~/Documents/workspace/notes/<Descriptive Title>.md`
- Check for an existing note on the topic before creating; update in place if found
- **Frontmatter**: `created: YYYY-MM-DD` and `tags: []`
- Freeform markdown body — use headers, bullets, and code blocks as appropriate

**Working notes from coding sessions** — any analysis, survey, design document, or reference
material produced during a session should be saved to `notes/` in addition to wherever it lands
in the project repo. The Obsidian vault is the canonical long-term home; the project copy is
the working artifact.

- After writing a doc to a project (e.g. `docs/something.md`), immediately mirror it to
  `~/Documents/workspace/notes/<Descriptive Title>.md` with appropriate frontmatter
- Link the note from the daily log so it's reachable from any day's context

### When to Record

- **Working notes**: Always mirror to Obsidian immediately after writing any substantive
  document to a project repo (analysis, design docs, survey reports, testing notes, etc.)
- **Check in proactively** during a session: after completing a significant task or milestone,
  briefly ask "Want me to log this to your daily note?" before continuing
- **Summarize at session end** without being asked: write the daily log entry and offer to
  create/update a long-form note for any substantial topic covered
- **Immediately** when explicitly asked (e.g., "add a note about this", "log this")

## Continuous Learning

Record learnings from interactions to improve future sessions:

**When to update memory:**
- User corrects an assumption or approach
- Discover project-specific patterns that differ from defaults (e.g., "project X uses pytest-asyncio with specific fixtures")
- Encounter unexpected tool behavior or gotchas
- Learn about user preferences through corrections (e.g., "user prefers explicit return types over inferred")
- Find that documented approaches don't work in practice

**What to record:**
- Project-specific conventions: "brightbandlib uses custom pytest markers for slow tests"
- Tool configurations: "pixi environments require explicit channel priority in this project"
- User workflow preferences: "user prefers seeing full test output, not just summaries"
- Common mistakes to avoid: "do not auto-run tests on notebooks, only on .py files"
- Successful strategies: "when debugging CI failures, compare local and CI Python versions first"

**How to record:**
- For project-specific patterns: Create or update topic files (e.g., `python-testing.md`, `project-patterns.md`)
- For critical insights: Add concise entries to `MEMORY.md` with links to detailed topic files
- Keep entries actionable: "Do X" or "Check Y before Z", not vague observations
- Update or remove outdated entries when they're corrected

Check existing memory files before making assumptions about workflows or configurations.
