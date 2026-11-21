# Git and Release Workflow

Git operations are human-controlled. Inspect status and diffs freely, but do not commit, push, merge, rebase, tag, publish, switch branches, create worktrees, or discard changes unless explicitly requested. Preserve unrelated tracked and untracked work.

## Commit message requests

Before suggesting, generating, or reviewing any commit message, read `.ai/commit-messages.md` in the current turn. Treat it as the canonical source for format, subject selection, banned wording, length, and response shape. Inspect the actual diff or staged scope first; do not infer a message from the task description alone.

## Branch model

- `main` represents released work.
- Prepare a release on its version branch.
- Branch features/fixes from the active version branch and squash them back after review.
- Keep commits behavior- or domain-focused; avoid temporary phase labels.

Before any authorized commit, run `git diff --check` and the validation appropriate to the touched files. Before release, run full tests and package check, update version metadata and `NEWS.md`, regenerate documentation, build/inspect pkgdown, merge the completed version branch as directed, then tag/publish only with explicit authorization.

Do not mix generated documentation, behavior changes, or release metadata accidentally; describe deliberate combinations clearly.
