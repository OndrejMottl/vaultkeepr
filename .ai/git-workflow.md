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

## Issue, pull-request, and comment text

Write every issue, pull request, and comment for a reader who knows ecology and R but has never seen this work: a collaborator, a reviewer, or the maintainer months later. Assume they have not read the release plan, other issues, design notes, chat sessions, or the agent's working files. The text must explain itself; links are supporting evidence, never a replacement for the explanation.

Include:

- An opening of two or three plain sentences: what this is about, why it matters, and what the reader is asked to do (review, decide, or nothing).
- The background the reader needs: which repository and data source, what happens now, and what is wrong or missing. Explain each project-specific term on first use, in one clause, even when you also link to its definition.
- For an issue: the task and what "done" means, as checkable points.
- For a pull request: what changed and why, what was checked (the actual command or test and its result, with numbers), what was not checked, and any decision left for the reader.
- For every reference to another issue, pull request, decision, or note: a few words on what it is or what it decided. "As agreed in #<n>" is not enough; "#<n> decided to keep the TRY trait records and drop duplicates" is.

Do not use:

- Plan codes, phase or stage numbers, work-package or checkpoint labels, or decision numbers (for example `G2`, `L5`, `a03`, `decision 10`) unless the same text says in plain words what they are.
- Private shorthand from a working session ("the rehearsal database", "the parity check", "the subset") without saying what it is.
- "As discussed", "per the plan", or "see above" in place of the content itself.

Keep it concise. Lead with the summary, use short headed sections (`Background`, `Changes`, `Checks`, `Open questions`) only where they help, and put long evidence in a collapsed `<details>` block or a linked file.

Before posting, reread the text as that newcomer would. If they could not tell what problem it addresses, what changed or is needed, and how it was or will be verified without opening any link, add the missing explanation.
