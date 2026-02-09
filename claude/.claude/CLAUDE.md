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
