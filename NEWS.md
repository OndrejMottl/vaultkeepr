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
