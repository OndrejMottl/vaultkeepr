# Database Contract

`vaultkeepr` is a client of a versioned VegVault SQLite schema. Queries, joins, readable-name mappings, object classes, and examples are downstream schema contracts.

## Connection and query rules

- `open_vault()` is the production connection entry point.
- Never embed personal paths or credentials. Tests use an in-memory disposable database.
- A function must not disconnect a connection supplied by its caller.
- Preserve lazy database work through `vault_pipe` until collection is part of the documented return contract.
- Check schema/version compatibility deliberately and produce actionable errors for unsupported databases.

## Schema synchronization

The package contains two schema fixtures that must remain synchronized:

- `tests/testthat/helper_make_database.R`
- `vignettes/helper_make_example_db.R`

Their external executable authority is the main VegVault repository's `Data/SQL/make_tables.sql`; `Data/SQL/database_structure.dbml` is that repository's conceptual mirror. A schema change requires coordinated updates to the external schema and version metadata, both package fixtures, affected tests, vignettes, reference documentation, and release notes.

This repository must remain usable without the sibling checkout. If the external repository is unavailable, work from the released schema contract represented by the local fixtures and explicitly flag any change that still needs upstream synchronization.

Never validate by overwriting a live database. Use an in-memory database or a disposable temporary SQLite copy.
