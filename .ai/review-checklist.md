# Review Checklist

Report findings first, ordered by severity, with file and line references.

Check:

- public API, documented return objects, and backward compatibility;
- `open_vault()` connection ownership and live-database safety;
- lazy `vault_pipe` behavior and accidental early collection;
- `return_raw_data` branches and empty/invalid inputs;
- `assertthat_cli()` diagnostics and `verbose` behavior;
- schema consistency between both local fixtures and the released external contract;
- tests that exercise the intended regression rather than implementation details;
- roxygen, generated documentation, NEWS, and pkgdown synchronization;
- exposure of credentials, private paths, licensed data, or ignored databases.

If no findings remain, say so and identify residual validation gaps.
