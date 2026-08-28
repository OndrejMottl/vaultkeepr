# Commit Message Instructions

Before generating, suggesting, or reviewing a commit message, inspect the actual changed or staged scope and read this file in the current turn. Do not rely on remembered conventions.

When the user asks only for a commit message, return exactly one plain-text line: no body, bullets, quotes, code fences, labels, explanation, or trailing period.

Use:

```text
<subject>: <short summary>
```

Keep the complete line at or below 72 characters. Describe durable package behavior without issue numbers, pull-request numbers, or temporary phase/stage labels.

## Subject selection

Use the narrowest meaningful subject:

- one function: its function name with parentheses, for example `open_vault(): validate SQLite headers before connecting`
- several functions around one API behavior: a plain API/domain label, for example `Lazy queries: preserve database-side filtering`
- schema fixtures or compatibility: `schema`
- roxygen, README, vignettes, or pkgdown: `docs`
- tests only: `tests`
- package dependencies or lock state: `dependencies`
- release metadata or NEWS: `release`
- agent guidance: `agents`
- CI/workflows: `ci`
- editor configuration: `vscode`

## Wording

Start the summary with a specific verb such as `add`, `adjust`, `correct`, `document`, `preserve`, `remove`, `replace`, `split`, `switch`, `update`, or `validate`.

Do not use vague labels or words such as `feat`, `feature`, `fix`, or `enhance`. State what changed.

Examples:

- `open_vault(): reject unsupported database versions`
- `get_taxa(): preserve lazy filtering for empty selections`
- `schema: align test and vignette SQLite fixtures`
- `docs: clarify disposable example database setup`
- `agents: expand R and commit-message guidance`
