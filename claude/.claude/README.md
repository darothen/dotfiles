# Claude Code Configuration Guide

This directory contains your Claude Code configuration. Use `config.yaml` to customize Claude's behavior for your project.

## Quick Start

1. **Edit `config.yaml`** - Uncomment and modify sections you want to customize
2. **Start simple** - Begin with small tweaks to see what works
3. **Iterate** - Adjust based on how Claude responds to your changes

## Common Customization Scenarios

### Make Claude More Concise

```yaml
behavior:
  communication:
    verbosity: concise
    explanation_depth: minimal
```

### Enforce Specific Code Style

```yaml
project:
  conventions:
    - "Use 2-space indentation"
    - "Always use async/await, never raw promises"
    - "Prefer functional components over class components"

hooks:
  post_edit: |
    prettier --write "$FILE_PATH"
    eslint --fix "$FILE_PATH"
```

### Add Project Context

```yaml
project:
  type: web_app
  language: typescript
  framework: nextjs

  architecture:
    - "src/app contains Next.js app router pages"
    - "src/components for React components"
    - "All API calls use the client in src/lib/api.ts"
    - "Styling uses Tailwind CSS with custom config"

prompts:
  system_instructions: |
    This is an e-commerce platform. Always consider:
    - Performance (this is a high-traffic site)
    - Security (handle payment data properly)
    - Accessibility (WCAG 2.1 AA compliance)
```

### Auto-Run Tests After Changes

```yaml
behavior:
  testing:
    auto_run_tests: true
    test_framework: jest

hooks:
  post_edit: |
    if [[ "$FILE_PATH" == *.test.* ]]; then
      exit 0  # Don't run tests when editing test files
    fi
    npm test -- --findRelatedTests "$FILE_PATH" --passWithNoTests
```

### Control Git Behavior

```yaml
behavior:
  git:
    commit_message_style: conventional
    include_coauthor: true

hooks:
  pre_commit: |
    npm run lint
    npm run type-check
```

### Language-Specific Instructions

```yaml
prompts:
  file_type_instructions:
    python: |
      - Use type hints for all function signatures
      - Write docstrings in Google style
      - Use pathlib for file operations
      - Prefer f-strings for formatting

    typescript: |
      - Use strict TypeScript settings
      - Prefer interfaces over types for objects
      - Always handle null/undefined explicitly
      - Use const assertions where appropriate
```

## Configuration Structure

### `behavior.*`
Controls how Claude works and communicates

### `project.*`
Defines your project structure and conventions

### `prompts.*`
Custom instructions that influence Claude's decision-making

### `hooks.*`
Shell commands triggered by events (file edits, commits, etc.)

### `model.*`
Control which AI models are used for different tasks

### `tools.*`
Configure behavior of specific tools

## Hook Variables

Hooks have access to environment variables:

- `$FILE_PATH` - Path to the file being operated on (for file hooks)
- `$USER_PROMPT` - The user's prompt text (for prompt hooks)
- `$TOOL_NAME` - Name of the tool being invoked (for tool hooks)

## Tips

1. **Start minimal** - Only configure what you need. Defaults are sensible.

2. **Use hooks for automation** - Format code, run linters, execute tests automatically.

3. **Document your architecture** - The more Claude knows about your project structure, the better decisions it makes.

4. **Version control this** - Commit `.claude/config.yaml` so your team shares the same settings.

5. **Test hooks separately** - Run hook commands manually first to ensure they work.

6. **Be specific in conventions** - Instead of "write good code", say "use 2-space indentation" or "limit functions to 50 lines".

## Examples by Project Type

### Python CLI Application

```yaml
project:
  type: cli
  language: python
  conventions:
    - "Use Click for CLI framework"
    - "Follow PEP 8"
    - "Type hint everything"
    - "Use src/ layout"

hooks:
  post_edit: |
    black "$FILE_PATH"
    isort "$FILE_PATH"
    mypy "$FILE_PATH" || true
```

### React + TypeScript Web App

```yaml
project:
  type: web_app
  language: typescript
  framework: react

  architecture:
    - "Components in src/components (presentational)"
    - "Features in src/features (containers + logic)"
    - "State management uses Zustand"
    - "API client in src/api"

behavior:
  coding_style:
    add_comments: minimal

hooks:
  post_edit: |
    prettier --write "$FILE_PATH"
```

### Rust Systems Project

```yaml
project:
  type: library
  language: rust

  conventions:
    - "Follow Rust API Guidelines"
    - "Use thiserror for error types"
    - "Write doc comments for public APIs"

hooks:
  post_edit: |
    cargo fmt -- "$FILE_PATH"
  pre_commit: |
    cargo clippy -- -D warnings
    cargo test
```

## Troubleshooting

**Changes not taking effect?**
- Ensure YAML syntax is valid (no tabs, proper indentation)
- Try restarting Claude Code
- Check that file path is correct (`.claude/config.yaml`)

**Hooks not running?**
- Verify hook commands work when run manually
- Check file permissions
- Ensure required tools are installed and in PATH
- Add `|| true` to prevent hook failures from blocking operations

**Claude not following conventions?**
- Be more specific and directive in your language
- Add examples to `prompts.system_instructions`
- Use hooks to enforce automatically rather than relying on Claude

## Further Customization

This configuration system is flexible. Experiment with:

- Different hook combinations
- Custom system instructions for your domain
- Model selection for performance vs. quality tradeoffs
- File-type-specific behaviors

Save this file in version control and iterate as your needs evolve.
