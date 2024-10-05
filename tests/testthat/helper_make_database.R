# 0. setup -----
requireNamespace("tidyr", quietly = TRUE)
requireNamespace("stringr", quietly = TRUE)
requireNamespace("purrr", quietly = TRUE)
requireNamespace("magrittr", quietly = TRUE)

.data <- rlang::.data
`%>%` <- magrittr::`%>%`

set_seed <- function(seed = c(0000 - 0002 - 9796 - 5081)) {
  seed %>%
    as.character() %>%
    set.seed()
}

process_references <- function(
    con_db,
    id_table_name, ref_table_name,
    name_column_id,
    dummy_ref_name,
    n_refs = 5) {
  db <-
    dplyr::tbl(con_db, id_table_name) %>%
    dplyr::distinct(!!dplyr::sym(name_column_id)) %>%
    dplyr::collect()

  set_seed()
  data_references <-
    db %>%
    dplyr::mutate(
      reference_dummy = sample(
        x = c(NA_integer_, 1:n_refs),
        size = nrow(.),
        replace = TRUE
      )
    ) %>%
    dplyr::filter(!is.na(.data$reference_dummy)) %>%
    dplyr::mutate(
      reference_detail = paste0(dummy_ref_name, "_", .data$reference_dummy)
    )

  data_references %>%
    dplyr::distinct(.data$reference_detail) %>%
    dplyr::arrange(.data$reference_detail) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "References",
      append = TRUE
    )

  data_references_db <-
    dplyr::tbl(con_db, "References") %>%
    dplyr::collect()

  data_references %>%
    dplyr::left_join(
      data_references_db,
      by = "reference_detail"
    ) %>%
    dplyr::distinct(!!dplyr::sym(name_column_id), .data$reference_id) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = ref_table_name,
      append = TRUE
    )
}

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

  # m make the SQL query to make all tables
  sql_query_full <-
    paste0(
      "CREATE TABLE 'Datasets' (
  'dataset_id' INTEGER PRIMARY KEY,
  'dataset_name' TEXT,
  'data_source_id' INTEGER,
  'dataset_type_id' INTEGER,
  'data_source_type_id' INTEGER,
  'coord_long' REAL,
  'coord_lat' REAL,
  'sampling_method_id' INTEGER,
  FOREIGN KEY ('dataset_type_id') REFERENCES 'DatasetTypeID' ('dataset_type_id'),
  FOREIGN KEY ('sampling_method_id') REFERENCES 'SamplingMethodID' ('sampling_method_id'),
  FOREIGN KEY ('data_source_id') REFERENCES 'DatasetSourcesID' ('data_source_id'),
  FOREIGN KEY ('data_source_type_id') REFERENCES 'DatasetSourceTypeID' ('data_source_type_id')
);
CREATE TABLE 'DatasetReferences' (
  'dataset_id' INTEGER,
  'reference_id' INTEGER,
  FOREIGN KEY ('dataset_id') REFERENCES 'Datasets' ('dataset_id'),
  FOREIGN KEY ('reference_id') REFERENCES 'References' ('reference_id')
);
CREATE TABLE 'DatasetTypeID' (
  'dataset_type_id' INTEGER PRIMARY KEY,
  'dataset_type' TEXT
);
CREATE TABLE 'DatasetSourceTypeID' (
  'data_source_type_id' INTEGER PRIMARY KEY,
  'dataset_source_type' TEXT
);
CREATE TABLE 'DatasetSourceTypeReference' (
  'data_source_type_id' INTEGER,
  'reference_id' INTEGER,
  FOREIGN KEY ('data_source_type_id') REFERENCES 'DatasetSourceTypeID' ('data_source_type_id'),
  FOREIGN KEY ('reference_id') REFERENCES 'References' ('reference_id')
);
CREATE TABLE 'DatasetSourcesID' (
  'data_source_id' INTEGER PRIMARY KEY,
  'data_source_desc' TEXT
);
CREATE TABLE 'DatasetSourcesReference' (
  'data_source_id' INTEGER,
  'reference_id' INTEGER,
  FOREIGN KEY ('data_source_id') REFERENCES 'DatasetSourcesID' ('data_source_id'),
  FOREIGN KEY ('reference_id') REFERENCES 'References' ('reference_id')
);
CREATE TABLE 'SamplingMethodID' (
  'sampling_method_id' INTEGER PRIMARY KEY,
  'sampling_method_details' TEXT
);
CREATE TABLE 'SamplingMethodReference' (
  'sampling_method_id' INTEGER,
  'reference_id' INTEGER,
  FOREIGN KEY ('sampling_method_id') REFERENCES 'SamplingMethodID' ('sampling_method_id'),
  FOREIGN KEY ('reference_id') REFERENCES 'References' ('reference_id')
);
CREATE TABLE 'Samples' (
  'sample_id' INTEGER PRIMARY KEY,
  'sample_name' TEXT,
  'sample_details' TEXT,
  'age' REAL,
  'sample_size_id' INTEGER,
  FOREIGN KEY ('sample_size_id') REFERENCES 'SampleSizeID' ('sample_size_id')
);
CREATE TABLE 'SampleReference' (
  'sample_id' INTEGER,
  'reference_id' INTEGER,
  FOREIGN KEY ('sample_id') REFERENCES 'Samples' ('sample_id'),
  FOREIGN KEY ('reference_id') REFERENCES 'References' ('reference_id')
);
CREATE TABLE 'DatasetSample' (
  'dataset_id' INTEGER,
  'sample_id' INTEGER,
  FOREIGN KEY ('dataset_id') REFERENCES 'Datasets' ('dataset_id'),
  FOREIGN KEY ('sample_id') REFERENCES 'Samples' ('sample_id')
);
CREATE TABLE 'SampleUncertainty' (
  'sample_id' INTEGER,
  'iteration' INTEGER,
  'age' INTEGER,
  FOREIGN KEY ('sample_id') REFERENCES 'Samples' ('sample_id')
);
CREATE TABLE 'SampleSizeID' (
  'sample_size_id' INTEGER PRIMARY KEY,
  'sample_size' REAL,
  'description' TEXT
);
CREATE TABLE 'SampleTaxa' (
  'sample_id' INTEGER,
  'taxon_id' INTEGER,
  'value' REAL,
  FOREIGN KEY ('sample_id') REFERENCES 'Samples' ('sample_id'),
  FOREIGN KEY ('taxon_id') REFERENCES 'Taxa' ('taxon_id')
);
CREATE TABLE 'Taxa' (
  'taxon_id' INTEGER PRIMARY KEY,
  'taxon_name' TEXT
);
CREATE TABLE 'TaxonClassification' (
  'taxon_id' INTEGER,
  'taxon_species' INTEGER,
  'taxon_genus' INTEGER,
  'taxon_family' INTEGER,
  FOREIGN KEY ('taxon_id') REFERENCES 'Taxa' ('taxon_id'),
  FOREIGN KEY ('taxon_species') REFERENCES 'Taxa' ('taxon_id'),
  FOREIGN KEY ('taxon_genus') REFERENCES 'Taxa' ('taxon_id'),
  FOREIGN KEY ('taxon_family') REFERENCES 'Taxa' ('taxon_id')
);
CREATE TABLE 'TaxonReference' (
  'taxon_id' INTEGER,
  'reference_id' INTEGER,
  FOREIGN KEY ('taxon_id') REFERENCES 'Taxa' ('taxon_id'),
  FOREIGN KEY ('reference_id') REFERENCES 'References' ('reference_id')
);
CREATE TABLE 'TraitsDomain' (
  'trait_domain_id' INTEGER PRIMARY KEY,
  'trait_domain_name' TEXT,
  'trait_domanin_description' TEXT
);
CREATE TABLE 'Traits' (
  'trait_id' INTEGER PRIMARY KEY,
  'trait_domain_id' INTEGER,
  'trait_name' TEXT,
  FOREIGN KEY ('trait_domain_id') REFERENCES 'TraitsDomain' ('trait_domain_id')
);
CREATE TABLE 'TraitsValue' (
  'trait_id' INTEGER,
  'dataset_id' INTEGER,
  'sample_id' INTEGER,
  'taxon_id' INTEGER,
  'trait_value' REAL,
  FOREIGN KEY ('trait_id') REFERENCES 'Traits' ('trait_id'),
  FOREIGN KEY ('dataset_id') REFERENCES 'Datasets' ('dataset_id'),
  FOREIGN KEY ('sample_id') REFERENCES 'Samples' ('sample_id'),
  FOREIGN KEY ('taxon_id') REFERENCES 'Taxa' ('taxon_id')
);
CREATE TABLE 'TraitsReference' (
  'trait_id' INTEGER,
  'reference_id' INTEGER,
  FOREIGN KEY ('trait_id') REFERENCES 'Traits' ('trait_id'),
  FOREIGN KEY ('reference_id') REFERENCES 'References' ('reference_id')
);
CREATE TABLE 'AbioticData' (
  'sample_id' INTEGER,
  'abiotic_variable_id' INTEGER,
  'abiotic_value' REAL,
  FOREIGN KEY ('sample_id') REFERENCES 'Samples' ('sample_id'),
  FOREIGN KEY ('abiotic_variable_id') REFERENCES 'AbioticVariable' ('abiotic_variable_id')
);

CREATE TABLE 'AbioticDataReference' (
  'sample_id' INTEGER,
  'sample_ref_id' INTEGER,
  'distance_in_km' INTEGER,
  'distance_in_years' INTEGER,
  FOREIGN KEY ('sample_id') REFERENCES 'Samples' ('sample_id'),
  FOREIGN KEY ('sample_ref_id') REFERENCES 'Samples' ('sample_id')
);

CREATE TABLE 'AbioticVariable' (
  'abiotic_variable_id' INTEGER PRIMARY KEY,
  'abiotic_variable_name' TEXT,
  'abiotic_variable_unit' TEXT,
  'measure_details' TEXT
);
CREATE TABLE 'AbioticVariableReference' (
  'abiotic_variable_id' INTEGER,
  'reference_id' INTEGER,
  FOREIGN KEY ('abiotic_variable_id') REFERENCES 'AbioticVariable' ('abiotic_variable_id'),
  FOREIGN KEY ('reference_id') REFERENCES 'References' ('reference_id')
);
CREATE TABLE 'References' (
  'reference_id' INTEGER PRIMARY KEY,
  'reference_detail' TEXT
);"
    )

  # split the query by semicolon
  sql_query_split <-
    paste(sql_query_full, collapse = "") %>%
    stringr::str_remove_all("\\n") %>%
    stringr::str_split(., pattern = "\\;") %>%
    unlist()

  # execute each query
  try(
    purrr::map(
      .x = sql_query_split,
      .f = ~ .x %>%
        stringr::str_remove_all("\\n") %>%
        DBI::dbExecute(
          conn = con_db,
          statement = .
        )
    ),
    silent = TRUE
  )

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
      "gamma"
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

  set_seed()
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
  process_references(
    con_db,
    id_table_name = "DatasetSourcesID",
    ref_table_name = "DatasetSourcesReference",
    name_column_id = "data_source_id",
    dummy_ref_name = "data_source",
    n_refs = 3
  )

  # DatasetSourceTypeReference
  process_references(
    con_db,
    id_table_name = "DatasetSourceTypeID",
    ref_table_name = "DatasetSourceTypeReference",
    name_column_id = "data_source_type_id",
    dummy_ref_name = "data_source_type",
    n_refs = 3
  )

  # SamplingMethodReference
  process_references(
    con_db,
    id_table_name = "SamplingMethodID",
    ref_table_name = "SamplingMethodReference",
    name_column_id = "sampling_method_id",
    dummy_ref_name = "sampling_method",
    n_refs = 3
  )

  # DatasetReferences
  process_references(
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
  process_references(
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
  process_references(
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
  process_references(
    con_db,
    id_table_name = "AbioticVariable",
    ref_table_name = "AbioticVariableReference",
    name_column_id = "abiotic_variable_id",
    dummy_ref_name = "abiotic_variable",
    n_refs = 3
  )

  ## AbioticDataReference -----

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
    dplyr::copy_to(
      con_db,
      df = .,
      name = "AbioticDataReference",
      append = TRUE
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
    dplyr::copy_to(
      con_db,
      df = .,
      name = "TraitsValue",
      append = TRUE
    )

  ### Trait references -----
  process_references(
    con_db,
    id_table_name = "Traits",
    ref_table_name = "TraitsReference",
    name_column_id = "trait_id",
    dummy_ref_name = "trait",
    n_refs = 5
  )

  # disconnect
  DBI::dbDisconnect(con_db)
}
