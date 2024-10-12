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
