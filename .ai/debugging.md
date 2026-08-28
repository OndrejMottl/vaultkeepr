# Debugging

- Reproduce the problem in a clean R session with `devtools::load_all()`.
- Reduce failures to the smallest function, query, and in-memory database fixture that preserves the bug.
- Use `tempfile()` or `tempdir()` for disposable SQLite files and diagnostics. Do not write experiments into tracked package paths.
- Inspect lazy SQL and query classes before collecting large results.
- Check connection ownership, schema/version assumptions, column mappings, and `return_raw_data` branches explicitly.
- Convert a confirmed regression into a focused test before implementing the fix.

After the focused test passes, run `devtools::test()` and `devtools::check()`. Never debug against a live user database when the fixture can represent the issue.
