# vaultkeepr Agent Guide

`vaultkeepr` is the standalone R client for querying VegVault SQLite databases. These instructions are self-contained: do not depend on a sibling VegVault checkout to work in this repository.

## Required reading

Read the canonical `.ai/` files relevant to the task:

- R implementation: `.ai/r-coding.md`
- exported functions, roxygen, and tests: `.ai/r-functions.md`
- database access or schema-dependent behavior: `.ai/database-contract.md`
- README, vignettes, or pkgdown: `.ai/documentation.md`
- Git, branches, commits, or releases: `.ai/git-workflow.md` and `.ai/commit-messages.md`
- suggesting, writing, or reviewing a commit message: read `.ai/git-workflow.md`, then reload `.ai/commit-messages.md` in the current turn
- diagnosis and temporary experiments: `.ai/debugging.md`
- reviews and large plans: `.ai/review-checklist.md` and `.ai/agents/`

## Non-negotiable safeguards

- Preserve public API behavior, lazy query semantics, schema compatibility, and connection ownership unless the requested change explicitly revises them.
- Use strict test-first development for behavior changes. Follow `.ai/r-functions.md` in order.
- Never connect tests to a live VegVault database. Use the in-memory fixture from `tests/testthat/helper_make_database.R`.
- Do not expose credentials, private database paths, licensed source data, ignored databases, or user data.
- Do not edit generated `NAMESPACE`, `man/`, `README.md`, or `docs/` directly.
- Do not commit, push, merge, tag, publish, or alter branch/worktree state unless the user explicitly asks.
- Avoid unrelated style refactors in legacy code. Validate proportionally, ending with full tests and package check for package behavior changes.

Tool-native instruction files are short compatibility adapters. Canonical policy lives only under `.ai/`.
