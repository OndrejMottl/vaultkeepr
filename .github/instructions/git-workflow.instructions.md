---
description: >
  Git workflow for the vaultkeepr package: version-branch strategy, PR
  management, feature branching, and release conventions.
---

# Git Workflow Instructions

## Branch Strategy Overview

The workflow is built around a **version branch** that acts as the integration
target for a release cycle:

```
main
 └── 0.0.7          ← version branch (PR open against main)
      ├── fix-bug-x      ← feature/fix branches (PR into version branch)
      └── refactor-fn-y
```

All work for a given release goes through the version branch before reaching
`main`.

---

## Step 1 — Start a New Release Cycle

Create a version branch from `main` and bump `DESCRIPTION`:

```powershell
# 1. Ensure main is up to date
git checkout main
git pull origin main

# 2. Create the version branch
git checkout -b 0.0.7    # use the next version number

# 3. Bump version in DESCRIPTION (edit manually)
# Version: 0.0.7

# 4. Commit the version bump
git add DESCRIPTION
git commit -m "chore: bump version to 0.0.7"
git push -u origin 0.0.7
```

Then **open a PR** from `0.0.7` → `main`. Do **not** merge it yet — it will
accumulate the release description throughout the cycle.

---

## Step 2 — Work on a Feature or Fix

Branch off the **version branch** (not `main`):

```powershell
# 1. Ensure the version branch is up to date
git checkout 0.0.7
git pull origin 0.0.7

# 2. Create the feature/fix branch
git checkout -b refactor-function-x

# 3. Verify
git branch
```

Branch name conventions:

- `kebab-case` with a short descriptive label
- Examples: `fix-dataset-query`, `add-trait-filter`, `refactor-open-vault`

---

## Step 3 — Commit Work on a Feature Branch

Follow the TDD cycle before each commit:

1. `devtools::document()` — regenerate `man/` if roxygen2 docs changed
2. `devtools::test()` — all tests pass
3. `devtools::check()` — no new errors or warnings

Use the commit message format from
[`../.github/commit-instructions.md`](../commit-instructions.md).

Only commit when the package is in a clean, passing state.

---

## Step 4 — Merge Feature Branch into Version Branch

Prefer **squash merges** to keep history clean:

```powershell
# 1. Switch to version branch and pull latest
git checkout 0.0.7
git pull origin 0.0.7

# 2. Squash-merge the feature branch
git merge --squash refactor-function-x
git commit -m "<descriptive message per commit-instructions.md>"
git push origin 0.0.7

# 3. Delete the feature branch
git branch -d refactor-function-x
git push origin --delete refactor-function-x
```

After merging, **update the open PR description** for the version branch:

- Summarise what the merged branch changed.
- Add `Closes #<issue_number>` for any issues resolved.

---

## Step 5 — Finalise the Release

Once all features and fixes are merged into the version branch:

1. **Update `NEWS.md`** — document all changes for the new version.
2. **Render the pkgdown website** — run `tools/build_pkgdown_site.R` or
   `pkgdown::build_site()`.
3. Commit these final changes to the version branch.
4. The PR (`0.0.7` → `main`) is now ready to be reviewed and merged.

---

## Key Rules

- **Version branch from `main`** — start each release cycle with a version
  branch (`0.0.7`, `0.1.0`, etc.) branched off `main`.
- **Feature branches from version branch** — never from `main` or another
  feature branch.
- **Squash merge** — keep history clean with one commit per feature on the
  version branch.
- **PR stays open until release is complete** — accumulate the description
  as feature branches are merged in.
- **`NEWS.md` and website are updated last** — only after all changes are
  merged into the version branch.
- **No direct commits to `main`** — all changes go through a feature branch
  and then the version branch PR.
