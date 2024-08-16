# vaultkeepr 0.0.2

* `get_abiotic_data()`- now link with the distance data (i.e., needs to have both "gridpoints" samples and "non-gridpoints" samples to be present)
* removed `select_abiotic_data_by_distance()` as it is built within `get_abiotic_data()`
* Dummy database - add the `AbioticDataReference` data and adjust the creation of "gridpoints" data to have several random points around "non-gridpoints" data.
* Test - remove most hardcoded values and replace them with evaluations based on real size from the database
