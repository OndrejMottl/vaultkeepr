# vaultkeepr 0.2.0

vaultkeepr 0.2.0 is released for VegVault 2.0.0. It reads both VegVault 1.0.0 and 2.0.0, and stops with an error for a database of any other major version. It opens the database read-only, adds the units of VegVault 2 trait values to the trait data, and makes several queries faster on large selections, without changing their results. One default changes: the geographic box of `select_dataset_by_geo()` now applies to every dataset type.

## Changed behaviour

* `select_dataset_by_geo()` applies the box to all four dataset types by default: `sel_dataset_type` now defaults to `vegetation_plot`, `fossil_pollen_archive`, `traits` and `gridpoints`. Before, trait datasets passed the box from anywhere in the world unless `traits` was named. Trait datasets outside the box, or without coordinates, are now dropped. To keep all datasets of a type, leave it out of `sel_dataset_type`; a message names the types in the data that the box does not filter.
* `select_dataset_by_geo()` stops with an error when `sel_dataset_type` names a type other than these four, or when the data lack the `dataset_type_id` column that `get_datasets()` adds.
* `get_age_uncertainty()` returns its rows in a fixed order: with `return_raw_data = TRUE` sorted by `sample_id` and `iteration`, and in the wide output one row per sample in the order of `sample_id`. Before, the order was whatever SQLite returned, which for larger selections was not by sample. The values are unchanged.
* `open_vault()` stops with an error for a VegVault version that vaultkeepr does not support, and closes the connection it opened. vaultkeepr 0.2.0 supports VegVault 1 and 2 (so far, the releases 1.0.0 and 2.0.0); a version that cannot be read as a version number, such as `2.0.0-dev`, is not supported either. A database without version rows is read as VegVault 1.0.0.
* `open_vault()` opens the database read-only. vaultkeepr only reads from it, and a read-only connection guarantees that no query changes the file. A statement that writes through the connection of a `vault_pipe`, such as `CREATE TABLE`, now fails with an error; temporary tables, for example from `dplyr::compute()`, still work.
* `dplyr`, `dbplyr` and `tidyr` are now installed with vaultkeepr. The package calls them for every query, but they were listed only as suggested packages, so vaultkeepr failed on a computer without them.
* vaultkeepr needs `dbplyr` 2.6.0 or newer. With `dbplyr` 2.5.0, queries that filter by the ids selected earlier in the pipe produce invalid SQL, and SQLite stops with `near "SELECT": syntax error`: for example `get_age_uncertainty()` on every call, and `get_taxa()` and `get_traits()` with `classify_to` other than `"original"`. The minimum version is now declared, so an older `dbplyr` is caught when vaultkeepr is installed or loaded, not at the first such query.

## VegVault 2 compatibility

* `open_vault()` detects the database version without changing the two-element `vault_pipe` structure. The version is read from the row of `version_control` with the highest `id`, because the VegVault build writes one row per version, in order. An unordered query is not enough: in VegVault 2.0.0, `version` is unique, and SQLite may return the rows in the text order of the versions, with `2.10.0` before `2.9.0`.
* `get_traits()` keeps VegVault 1 databases queryable and warns that their trait units may be unreliable.
* VegVault 2 trait queries expose canonical `trait_unit`, `conversion_rule_key`, and `derivation_rule_key` fields while remaining lazy.
* Packed trait data retains `trait_unit` but omits verbose record-level provenance.
* `get_readable_column_names()`, and with it `pack_data()`, works on trait data from VegVault 2. Both the trait values and `TraitsDomain` carry `trait_unit` there, and the join of `TraitsDomain` failed on the duplicated column; the column of the trait values is now kept.
* `select_traits_by_domain_name()` keeps one `trait_unit` column on trait data from VegVault 2. It joined the whole `TraitsDomain` table, so the unit that the trait values already carry was split into `trait_unit.x` and `trait_unit.y`, and `extract_data()` left the unit out of the packed trait data. The column of the trait values is now kept.

## Documentation

* The vignettes use an example database in the VegVault 2.0.0 layout. The article on functional traits (`a03_functional_traits`) shows the units of the trait values and their conversion rules, and no longer warns about VegVault 1 trait units.

## Query performance

* `extract_data()` checks for mandatory references with one query that stops at the first match, and only when `verbose = TRUE`, because the alert is the only effect of the check. It no longer prints the messages of `get_references()`.
* `get_references()` sends the selected ids to SQLite as a subquery instead of a literal list, and reads the reference details in the same query.
* `select_dataset_by_geo()` filters with range conditions on `coord_long` and `coord_lat` instead of a `CASE` expression, so SQLite can use the coordinate index of `Datasets`. It queries which dataset types are in the data only when some type is left out of `sel_dataset_type` and `verbose = TRUE`, to print the message.
* `get_age_uncertainty()` reads the uncertainty rows of the selected samples with `sample_id IN (SELECT DISTINCT sample_id ...)` instead of a correlated `EXISTS`. SQLite runs the selection once and finds the rows through the index or the primary key on `sample_id`, instead of scanning the whole `SampleUncertainty` table.
* `get_taxa()` and `get_traits()` with `classify_to` other than `"original"` classify only the taxa of the selected samples. The rows of `SampleTaxa` or `TraitsValue` are narrowed to the sample ids of the selection before the join with the classification, instead of classifying the taxa of the whole database first. The results are unchanged.

# vaultkeepr 0.1.0

## New functions

* `get_taxon_names()` - retrieve distinct taxon names from the Vault database
* `get_age_uncertainty()` - retrieve age uncertainty estimates for samples (wide and long format)
* `explore_vault()` - retrieve table names from a connected VegVault database
* `get_classification_table()` - retrieve classification data for inspecting and customising taxonomic classification

## Improvements

* `classify_taxa()` - add a `cli` warning when automatic classification is used (i.e., `to` is not `"original"`) to alert users about potential errors; add optional `classification_data` argument for user-supplied override tables
* `open_vault()` - add validation to ensure the supplied path points to a valid SQLite file

## Refactoring

* `assertthat_cli` helper
  * replace `assertthat::assert_that()` with a new `assertthat_cli()` helper using `{cli}` and `{stringr}` for clearer error and warning messages across all exported functions; `cli` and `stringr` moved to `Imports`

# vaultkeepr 0.0.6

* `get_readable_column_names()` - specify the link type for each table to improve the performance

* `classify_taxa()` - change the link type to "inner" to increase the performance.

## tests

* update tests for `extract_data()`

* update the "helper database" (for testing) -  make sure we do reference gridpoints in the `dataset_source_type` table

# vaultkeepr 0.0.5

* `get_references()` - It now (optionally) outputs the source of the referecnes (i.e., the table where the references are stored)

* `extract_data()` - perform a check via `get_references()` and output reminder message to the user to cite `mandatory` references

# vaultkeepr 0.0.4

* `extract_data()` - now returns human-readable column names (NAMES) by default (not IDs) and pack all data (samples) into several tibbles

* `get_references()` - the input is switched directly to a plan (no need to path and/or extracting data to get references)

# vaultkeepr 0.0.3

* `get_reference_data()` - new function to get the references present in the data compilation extracted from the database

# vaultkeepr 0.0.2

* `get_abiotic_data()`- now link with the distance data (i.e., needs to have both "gridpoints" samples and "non-gridpoints" samples to be present)
* removed `select_abiotic_data_by_distance()` as it is built within `get_abiotic_data()`
* Dummy database - add the `AbioticDataReference` data and adjust the creation of "gridpoints" data to have several random points around "non-gridpoints" data.
* Test - remove most hardcoded values and replace them with evaluations based on real size from the database
