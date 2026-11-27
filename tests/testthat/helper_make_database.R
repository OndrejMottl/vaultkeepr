# 0. setup -----
requireNamespace("tidyr", quietly = TRUE)
requireNamespace("stringr", quietly = TRUE)
requireNamespace("purrr", quietly = TRUE)
requireNamespace("magrittr", quietly = TRUE)

.data <- rlang::.data
`%>%` <- magrittr::`%>%`

if (
  !file.exists(
    paste(
      tempdir(),
      "example.sqlite",
      sep = "/"
    )
  )
) {
  # 1. Create an empty database -----

  # create a database conenction to a temporary file
  con_db <-
    DBI::dbConnect(
      RSQLite::SQLite(),
      paste(
        tempdir(),
        "example.sqlite",
        sep = "/"
      )
    )

  # The tables and indexes come from the VegVault schema file shared with
  #   the vignette database (inst/sql/), so that the two cannot drift apart
  build_schema_fixture(db_con = con_db)

  # 2. insert data into the database -----

  ## Datasets -----

  # DatasetType
  tibble::tibble(
    dataset_type = c(
      "vegetation_plot",
      "fossil_pollen_archive",
      "traits",
      "gridpoints"
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "DatasetTypeID",
      append = TRUE
    )

  # DatasetSourceType
  tibble::tibble(
    dataset_source_type = c(
      "alpha",
      "beta",
      "gamma",
      "gridpoints"
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "DatasetSourceTypeID",
      append = TRUE
    )

  # DatasetSources
  tibble::tibble(
    data_source_desc = c(
      "source1",
      "source2",
      "source3"
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "DatasetSourcesID",
      append = TRUE
    )

  # SamplingMethod
  tibble::tibble(
    sampling_method_details = c(
      "method1",
      "method2",
      "method3"
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "SamplingMethodID",
      append = TRUE
    )

  # Datasets
  # create a tibble with 486 datasets
  # 486 = 6 (continents) * 81 (datasets per continent)
  # 81 =  3 (dataset types) * 3 (data source types) * 3 (data sources) * 3 (sampling methods)

  data_dummy_coord <-
    tibble::tribble(
      ~coord_long, ~coord_lat,
      -115, 45,
      15, 45,
      115, 45,
      -60, -15,
      -15, -30,
      -135, -30
    )

  apply_fixture_seed()
  data_datasets_gridpoints <-
    purrr::map(
      .x = 1:5,
      .f = ~ data_dummy_coord %>%
        dplyr::mutate(
          coord_long = coord_long + runif(1, -0.05, 0.05),
          coord_lat = coord_lat + runif(1, -0.05, 0.05)
        )
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      data_source_id = 4,
      dataset_type_id = 4,
      data_source_type_id = 4,
      sampling_method_id = NA_integer_
    )

  data_datasets_vegetation <-
    tidyr::expand_grid(
      data_dummy_coord,
      data_source_id = 1:3,
      dataset_type_id = 1:3,
      data_source_type_id = 1:3,
      sampling_method_id = 1:3
    )

  data_datasets <-
    dplyr::bind_rows(
      data_datasets_vegetation,
      data_datasets_gridpoints
    ) %>%
    dplyr::mutate(,
      dataset_name = paste0("dataset_", dplyr::row_number())
    )

  dplyr::copy_to(
    con_db,
    data_datasets,
    name = "Datasets",
    append = TRUE
  )

  ### Dataset References -----

  # DatasetSourceReference
  build_reference_fixture(
    con_db,
    id_table_name = "DatasetSourcesID",
    ref_table_name = "DatasetSourcesReference",
    name_column_id = "data_source_id",
    dummy_ref_name = "data_source",
    n_refs = 3
  )

  # DatasetSourceTypeReference
  build_reference_fixture(
    con_db,
    id_table_name = "DatasetSourceTypeID",
    ref_table_name = "DatasetSourceTypeReference",
    name_column_id = "data_source_type_id",
    dummy_ref_name = "data_source_type",
    n_refs = 3,
    mandatory = TRUE
  )

  # SamplingMethodReference
  build_reference_fixture(
    con_db,
    id_table_name = "SamplingMethodID",
    ref_table_name = "SamplingMethodReference",
    name_column_id = "sampling_method_id",
    dummy_ref_name = "sampling_method",
    n_refs = 3
  )

  # DatasetReferences
  build_reference_fixture(
    con_db,
    id_table_name = "Datasets",
    ref_table_name = "DatasetReferences",
    name_column_id = "dataset_id",
    dummy_ref_name = "dataset",
    n_refs = 5
  )

  ## Samples -----
  tidyr::expand_grid(
    init = 1:50,
    sample_size_id = 1:10,
    age = seq(0, 8000, by = 500)
  ) %>%
    dplyr::mutate(
      sample_name = paste0("sample_", dplyr::row_number())
    ) %>%
    dplyr::select(-init) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "Samples",
      append = TRUE
    )

  ### Sample references -----
  build_reference_fixture(
    con_db,
    id_table_name = "Samples",
    ref_table_name = "SampleReference",
    name_column_id = "sample_id",
    dummy_ref_name = "sample",
    n_refs = 10
  )

  # DatasetSample
  data_dataset_sample <-
    tibble::tibble(
      dataset_id = rep(
        seq_len(nrow(data_datasets)),
        ceiling(8500 / nrow(data_datasets))
      )[1:8500],
      sample_id = 1:8500
    )

  dplyr::copy_to(
    con_db,
    data_dataset_sample,
    name = "DatasetSample",
    append = TRUE
  )

  ## Taxa -----
  tibble::tibble(
    taxon_name = paste0("taxon_", 1:57)
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "Taxa",
      append = TRUE
    )

  ### Taxon references -----
  build_reference_fixture(
    con_db,
    id_table_name = "Taxa",
    ref_table_name = "TaxonReference",
    name_column_id = "taxon_id",
    dummy_ref_name = "taxon",
    n_refs = 5
  )

  ### TaxonClassification -----
  # There will be 3 families
  #   each with 3 unique genera
  #   each with 5 unique species
  data_classification <-
    tibble::tibble(
      taxon_species = 1:45,
      taxon_genus = rep(46:54, each = 5),
      taxon_family = rep(55:57, each = 15),
    )

  data_classification_species <-
    data_classification %>%
    dplyr::mutate(
      taxon_id = taxon_species
    ) %>%
    dplyr::relocate(taxon_id)

  data_classification_genus <-
    data_classification %>%
    dplyr::distinct(taxon_genus, taxon_family) %>%
    dplyr::mutate(
      taxon_id = taxon_genus
    )

  dplyr::bind_rows(
    data_classification_species,
    data_classification_genus
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "TaxonClassification",
      append = TRUE
    )

  vec_vegetation_sample_id_db <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id %in% 1:2) %>%
    dplyr::collect() %>%
    dplyr::inner_join(
      data_dataset_sample,
      by = "dataset_id"
    ) %>%
    dplyr::select(sample_id) %>%
    dplyr::distinct() %>%
    purrr::chuck("sample_id")

  # SampleTaxa
  tibble::tibble(
    sample_id = rep_len(
      rep(vec_vegetation_sample_id_db, 7),
      length.out = 85e3
    ),
    taxon_id = rep_len(
      rep(1:45, each = 9),
      length.out = 85e3
    ),
    value = rep_len(
      c(1, 10, 100),
      length.out = 85e3
    )
  ) %>%
    dplyr::distinct(sample_id, taxon_id, .keep_all = TRUE) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "SampleTaxa",
      append = TRUE
    )

  ## AbioticData -----
  tibble::tibble(
    abiotic_variable_name = c(
      "temperature",
      "precipitation",
      "soil_moisture"
    ),
    abiotic_variable_unit = c(
      "C",
      "mm",
      "percent"
    ),
    measure_details = c(
      "daily average",
      "annual average",
      "monthly average"
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "AbioticVariable",
      append = TRUE
    )

  vec_abiotic_sample_id <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id == 4) %>%
    dplyr::inner_join(
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::distinct(sample_id) %>%
    dplyr::collect() %>%
    dplyr::pull(sample_id)

  tibble::tibble(
    sample_id = rep(
      vec_abiotic_sample_id,
      each = 3
    ),
    abiotic_variable_id = rep(
      1:3,
      length(vec_abiotic_sample_id)
    ),
    abiotic_value = rep(
      c(-5, 0, 5),
      length(vec_abiotic_sample_id)
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "AbioticData",
      append = TRUE
    )

  ### AbioticVariable references -----
  build_reference_fixture(
    con_db,
    id_table_name = "AbioticVariable",
    ref_table_name = "AbioticVariableReference",
    name_column_id = "abiotic_variable_id",
    dummy_ref_name = "abiotic_variable",
    mandatory = TRUE,
    n_refs = 3
  )

  ## AbioticDataReference -----
  # The links are computed per sample, then written once per position
  #   (VegVault 2.0.0); the view AbioticDataReference returns them per sample

  data_abiotic_geo <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id == 4) %>%
    dplyr::inner_join(
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "Samples"),
      by = "sample_id"
    ) %>%
    dplyr::select(
      sample_id,
      coord_lat,
      coord_long
    ) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::rename_with(
      ~ paste0(.x, "_abiotic")
    )

  data_vegetation_geo <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id %in% 1:2) %>%
    dplyr::inner_join(
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "Samples"),
      by = "sample_id"
    ) %>%
    dplyr::select(
      sample_id,
      coord_lat,
      coord_long
    ) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::rename_with(
      ~ paste0(.x, "_vegetation")
    )

  data_vegetation_geo_nest <-
    data_vegetation_geo %>%
    dplyr::group_by(
      coord_lat_vegetation, coord_long_vegetation
    ) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dummy_id = dplyr::row_number()
    )

  data_vegetation_geo_lookup <-
    data_vegetation_geo_nest %>%
    tidyr::unnest(data) %>%
    dplyr::distinct(
      dummy_id,
      sample_id_vegetation
    )

  data_to_estimate_distance <-
    tidyr::expand_grid(
      data_abiotic_geo,
      data_vegetation_geo_nest %>%
        dplyr::select(
          coord_lat_vegetation,
          coord_long_vegetation,
          dummy_id
        )
    )

  data_with_distance <-
    data_to_estimate_distance %>%
    dplyr::mutate(
      distance_in_m = purrr::pmap_dbl(
        .l = list(
          coord_long_abiotic,
          coord_lat_abiotic,
          coord_long_vegetation,
          coord_lat_vegetation
        ),
        .f = ~ geosphere::distGeo(
          p1 = c(..1, ..2),
          p2 = c(..3, ..4)
        )
      ),
      distance_in_km = distance_in_m / 1e3,
    ) %>%
    dplyr::filter(distance_in_km <= 50) %>%
    dplyr::select(-"distance_in_m")

  data_abiotic_reference <-
    data_with_distance %>%
    dplyr::select(
      !dplyr::starts_with("coord_")
    ) %>%
    dplyr::left_join(
      data_vegetation_geo_lookup,
      by = "dummy_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::select(-"dummy_id")

  data_gridpoints_ages <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id == 4) %>%
    dplyr::inner_join(
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "Samples"),
      by = "sample_id"
    ) %>%
    dplyr::distinct(
      .data$sample_id,
      .data$age
    ) %>%
    dplyr::collect()

  data_vegetation_ages <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id != 4) %>%
    dplyr::inner_join(
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "Samples"),
      by = "sample_id"
    ) %>%
    dplyr::distinct(
      .data$sample_id,
      .data$age
    ) %>%
    dplyr::collect()

  data_abiotic_reference %>%
    dplyr::left_join(
      data_gridpoints_ages,
      by = c("sample_id_abiotic" = "sample_id")
    ) %>%
    dplyr::rename(
      age_abiotic = "age"
    ) %>%
    dplyr::left_join(
      data_vegetation_ages,
      by = c("sample_id_vegetation" = "sample_id")
    ) %>%
    dplyr::rename(
      age_vegetation = "age"
    ) %>%
    dplyr::mutate(
      distance_in_years = abs(age_abiotic - age_vegetation)
    ) %>%
    dplyr::filter(distance_in_years <= 5e3) %>%
    dplyr::distinct(
      sample_id_vegetation,
      sample_id_abiotic,
      distance_in_km,
      distance_in_years
    ) %>%
    dplyr::rename(
      sample_id = sample_id_vegetation,
      sample_ref_id = sample_id_abiotic
    ) %>%
    build_position_link_fixture(
      con_db = con_db,
      data_sample_links = .
    )

  ## Traits -----

  tibble::tibble(
    trait_domain_name = c(
      "Stem specific density",
      "Diaspore mass",
      "Plant heigh"
    ),
    trait_domanin_description = c(
      "Density of the stem",
      "The mass of a single seed or fruit",
      "The height of the plant"
    ),
    trait_unit = c(
      "g/cm3",
      "mg",
      "m"
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "TraitsDomain",
      append = TRUE
    )

  tibble::tibble(
    trait_domain_id = rep(1:3, each = 3),
    trait_name = paste0("trait_", 1:9)
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "Traits",
      append = TRUE
    )

  build_trait_rule_fixture(con_db = con_db)

  data_trait_dataset_sample <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id == 3) %>%
    dplyr::inner_join(
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::select(sample_id, dataset_id) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  tidyr::expand_grid(
    trait_id = 1:9,
    data_trait_dataset_sample
  ) %>%
    dplyr::mutate(
      taxon_id = rep_len(1:45, length.out = nrow(.)),
      trait_value = rep_len(c(0, 5, 100), length.out = nrow(.))
    ) %>%
    # Each value takes the unit-conversion rule of the domain of its trait
    dplyr::left_join(
      dplyr::tbl(con_db, "Traits") %>%
        dplyr::select(trait_id, trait_unit_conversion_id = trait_domain_id) %>%
        dplyr::collect(),
      by = "trait_id"
    ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "TraitsValue",
      append = TRUE
    )

  ### Trait references -----
  build_reference_fixture(
    con_db,
    id_table_name = "Traits",
    ref_table_name = "TraitsReference",
    name_column_id = "trait_id",
    dummy_ref_name = "trait",
    n_refs = 5
  )

  ## SampleUncertainty -----
  # 5 age-model iterations for each sample
  tibble::tibble(
    sample_id = rep(1:8500, each = 5),
    iteration = rep(1:5, times = 8500),
    age = rep_len(
      seq(0, 8000, by = 500),
      length.out = 8500 * 5
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "SampleUncertainty",
      append = TRUE
    )

  ## version_control -----
  # The current version has the highest id, as in VegVault
  DBI::dbExecute(
    conn = con_db,
    statement = stringr::str_c(
      "INSERT INTO version_control (version) ",
      "VALUES ('1.0.0'), ('2.0.0')"
    )
  )

  # disconnect
  DBI::dbDisconnect(con_db)
}
