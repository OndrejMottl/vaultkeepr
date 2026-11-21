#' @title Build the table of reference links
#' @description
#' Build the table that maps each reference type of VegVault to the id
#' column that carries it and to the link table between that column and
#' `References`.
#' @return
#' A `tibble` with one row per reference type and the columns
#' `reference_source` (the reference type, as in the `type` argument of
#' [get_references()]), `sel_column` (the id column) and `sel_table` (the
#' link table). The rows are in the order in which [get_references()]
#' reports the references.
#' @seealso [get_references()], [build_mandatory_reference_query()]
#' @keywords internal
build_reference_link_table <- function() {
  res <-
    tibble::tribble(
      ~reference_source, ~sel_column, ~sel_table,
      "Dataset", "dataset_id", "DatasetReferences",
      "DatasetSource", "data_source_id", "DatasetSourcesReference",
      "DatasetSourceType", "data_source_type_id", "DatasetSourceTypeReference",
      "SamplingMethod", "sampling_method_id", "SamplingMethodReference",
      "Sample", "sample_id", "SampleReference",
      "Taxon", "taxon_id", "TaxonReference",
      "Trait", "trait_id", "TraitsReference",
      "AbioticVariable", "abiotic_variable_id", "AbioticVariableReference"
    )

  return(res)
}
