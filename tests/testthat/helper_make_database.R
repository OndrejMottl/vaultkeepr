# 0. setup -----
requireNamespace("tidyr", quietly = TRUE)
requireNamespace("stringr", quietly = TRUE)
requireNamespace("purrr", quietly = TRUE)
requireNamespace("magrittr", quietly = TRUE)

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

  # Datasets -----

  # DatasetType
  data_dataset_type <-
    tibble::tibble(
      dataset_type = c(
        "vegetation_plot",
        "fossil_pollen_archive",
        "traits",
        "gridpoints"
      )
    )

  dplyr::copy_to(
    con_db,
    data_dataset_type,
    name = "DatasetTypeID",
    append = TRUE
  )

  # DatasetSourceType
  data_dataset_source_type <-
    tibble::tibble(
      dataset_source_type = c(
        "alpha",
        "beta",
        "gamma"
      )
    )

  dplyr::copy_to(
    con_db,
    data_dataset_source_type,
    name = "DatasetSourceTypeID",
    append = TRUE
  )

  # DatasetSources
  data_dataset_sources <-
    tibble::tibble(
      data_source_desc = c(
        "source1",
        "source2",
        "source3"
      )
    )

  dplyr::copy_to(
    con_db,
    data_dataset_sources,
    name = "DatasetSourcesID",
    append = TRUE
  )

  # SamplingMethod
  data_sampling_method <-
    tibble::tibble(
      sampling_method_details = c(
        "method1",
        "method2",
        "method3"
      )
    )

  dplyr::copy_to(
    con_db,
    data_sampling_method,
    name = "SamplingMethodID",
    append = TRUE
  )

  # Datasets
  # create a tibble with 648 datasets
  # 648 = 6 (continents) * 108 (datasets per continent)
  # 108 =  3 (dataset types) * 4 (data source types) * 3 (data sources) * 3 (sampling methods)

  data_datasets <-
    tidyr::expand_grid(
      data_source_id = 1:3,
      dataset_type_id = 1:4,
      data_source_type_id = 1:3,
      sampling_method_id = 1:3,
      tibble::tribble(
        ~coord_long, ~coord_lat,
        -115, 45,
        15, 45,
        115, 45,
        -60, -15,
        -15, -30,
        -135, -30
      )
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

  # Samples -----
  data_samples <-
    tidyr::expand_grid(
      init = 1:50,
      sample_size_id = 1:10,
      age = seq(0, 8000, by = 500)
    ) %>%
    dplyr::mutate(
      sample_name = paste0("sample_", dplyr::row_number())
    ) %>%
    dplyr::select(-init)

  dplyr::copy_to(
    con_db,
    data_samples,
    name = "Samples",
    append = TRUE
  )

  # DatasetSample
  data_dataset_sample <-
    tibble::tibble(
      dataset_id = rep(
        1:648,
        ceiling(8500 / 648)
      )[1:8500],
      sample_id = 1:8500
    )

  dplyr::copy_to(
    con_db,
    data_dataset_sample,
    name = "DatasetSample",
    append = TRUE
  )

  # Taxa -----
  data_taxa <-
    tibble::tibble(
      taxon_name = paste0("taxon_", 1:57)
    )

  dplyr::copy_to(
    con_db,
    data_taxa,
    name = "Taxa",
    append = TRUE
  )

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

  data_classification_merged <-
    dplyr::bind_rows(
      data_classification_species,
      data_classification_genus
    )

  dplyr::copy_to(
    con_db,
    data_classification_merged,
    name = "TaxonClassification",
    append = TRUE
  )

  # SampleTaxa
  data_sample_taxa <-
    tibble::tibble(
      sample_id = rep_len(
        rep(1:8500, 7),
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
    )

  dplyr::copy_to(
    con_db,
    data_sample_taxa,
    name = "SampleTaxa",
    append = TRUE
  )

  # AbioticData -----

  data_abiotic_variable <-
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
    )

  dplyr::copy_to(
    con_db,
    data_abiotic_variable,
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

  data_abiotic <-
    tibble::tibble(
      sample_id = rep(
        vec_abiotic_sample_id,
        3
      ),
      abiotic_variable_id = rep(
        1:3,
        length(vec_abiotic_sample_id)
      ),
      abiotic_value = rep(
        c(-5, 0, 5),
        ceiling(3 * length(vec_abiotic_sample_id) / 3)
      )[(3 * length(vec_abiotic_sample_id))]
    )

  dplyr::copy_to(
    con_db,
    data_abiotic,
    name = "AbioticData",
    append = TRUE
  )

  # Traits -----

  data_trait_domain <-
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
    )

  dplyr::copy_to(
    con_db,
    data_trait_domain,
    name = "TraitsDomain",
    append = TRUE
  )

  data_trait <-
    tibble::tibble(
      trait_domain_id = rep(1:3, each = 3),
      trait_name = paste0("trait_", 1:9)
    )

  dplyr::copy_to(
    con_db,
    data_trait,
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

  data_traits_value <-
    tidyr::expand_grid(
      trait_id = 1:9,
      data_trait_dataset_sample
    ) %>%
    dplyr::mutate(
      taxon_id = rep_len(1:45, length.out = nrow(.)),
      trait_value = rep_len(c(0, 5, 100), length.out = nrow(.))
    )

  dplyr::copy_to(
    con_db,
    data_traits_value,
    name = "TraitsValue",
    append = TRUE
  )

  # disconnect
  DBI::dbDisconnect(con_db)
}
