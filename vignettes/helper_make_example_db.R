# Helper: build the vaultkeepr example SQLite database
#
# Creates a self-contained SQLite database at
# tempdir()/vaultkeepr_example.sqlite with the same schema and data
# scope as the test-suite database.  The file is created only once per
# R session; subsequent calls return the path immediately.
#
# Returns the path string so articles can call:
#   path_db <- source("helper_make_example_db.R")$value

requireNamespace("DBI", quietly = TRUE)
requireNamespace("RSQLite", quietly = TRUE)
requireNamespace("dplyr", quietly = TRUE)
requireNamespace("tidyr", quietly = TRUE)
requireNamespace("tibble", quietly = TRUE)
requireNamespace("purrr", quietly = TRUE)
requireNamespace("magrittr", quietly = TRUE)
requireNamespace("stringr", quietly = TRUE)
requireNamespace("geosphere", quietly = TRUE)

`%>%` <- magrittr::`%>%`
.data <- rlang::.data

# ── helpers ──────────────────────────────────────────────────────────────────

.set_seed <- function() {
  set.seed(as.character(0000 - 0002 - 9796 - 5081))
}

# Insert rows into a reference table for every entity in id_table_name.
# Creates rows in "References" then links them via ref_table_name.
.add_references <- function(
  con_db,
  id_table_name,
  ref_table_name,
  id_col,
  prefix,
  mandatory = FALSE,
  n_refs = 5
) {
  .set_seed()

  data_ids <-
    dplyr::tbl(con_db, id_table_name) %>%
    dplyr::distinct(!!dplyr::sym(id_col)) %>%
    dplyr::collect()

  data_refs <-
    data_ids %>%
    dplyr::mutate(
      ref_num = sample(
        x = c(NA_integer_, seq_len(n_refs)),
        size = nrow(.),
        replace = TRUE
      )
    ) %>%
    dplyr::filter(!is.na(.data$ref_num)) %>%
    dplyr::mutate(
      reference_detail = paste0(prefix, "_", .data$ref_num),
      mandatory = mandatory
    )

  data_refs %>%
    dplyr::distinct(.data$reference_detail, .data$mandatory) %>%
    dplyr::arrange(.data$reference_detail) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "References",
      append = TRUE
    )

  data_refs_db <-
    dplyr::tbl(con_db, "References") %>%
    dplyr::collect()

  data_refs %>%
    dplyr::left_join(data_refs_db, by = "reference_detail") %>%
    dplyr::distinct(!!dplyr::sym(id_col), .data$reference_id) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = ref_table_name,
      append = TRUE
    )
}

# ── SQL schema ────────────────────────────────────────────────────────────────

.sql_schema <- '
CREATE TABLE "version_control"
("id" INTEGER PRIMARY KEY,"version" TEXT,"update_timestamp" DATETIME DEFAULT CURRENT_TIMESTAMP,"changelog" TEXT);
CREATE UNIQUE INDEX idx_version_control_id ON version_control(id);

CREATE TABLE "Authors"
("author_id" INTEGER PRIMARY KEY,"author_fullname" TEXT,"author_email" TEXT,"author_orcid" TEXT);
CREATE UNIQUE INDEX idx_authors_author_id ON Authors(author_id);

CREATE TABLE "References"
("reference_id" INTEGER PRIMARY KEY,"reference_detail" TEXT,"mandatory" BOOLEAN NOT NULL DEFAULT FALSE);
CREATE UNIQUE INDEX idx_references_reference_id ON "References"(reference_id);

CREATE TABLE "DatasetTypeID"
("dataset_type_id" INTEGER PRIMARY KEY,"dataset_type" TEXT);
CREATE UNIQUE INDEX idx_datasettypeid_dataset_type_id ON DatasetTypeID(dataset_type_id);

CREATE TABLE "DatasetSourceTypeID"
("data_source_type_id" INTEGER PRIMARY KEY,"dataset_source_type" TEXT);
CREATE UNIQUE INDEX idx_datasetsourcetypeid_data_source_type_id ON DatasetSourceTypeID(data_source_type_id);

CREATE TABLE "DatasetSourceTypeReference"
("data_source_type_id" INTEGER,"reference_id" INTEGER,
 FOREIGN KEY ("data_source_type_id") REFERENCES "DatasetSourceTypeID" ("data_source_type_id"),
 FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id"));
CREATE INDEX idx_datasetsourcetypereference_data_source_type_id ON DatasetSourceTypeReference(data_source_type_id);
CREATE INDEX idx_datasetsourcetypereference_reference_id ON DatasetSourceTypeReference(reference_id);

CREATE TABLE "DatasetSourcesID"
("data_source_id" INTEGER PRIMARY KEY,"data_source_desc" TEXT);
CREATE UNIQUE INDEX idx_datasetsourcesid_data_source_id ON DatasetSourcesID(data_source_id);

CREATE TABLE "DatasetSourcesReference"
("data_source_id" INTEGER,"reference_id" INTEGER,
 FOREIGN KEY ("data_source_id") REFERENCES "DatasetSourcesID" ("data_source_id"),
 FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id"));
CREATE INDEX idx_datasetsourcesreference_data_source_id ON DatasetSourcesReference(data_source_id);
CREATE INDEX idx_datasetsourcesreference_reference_id ON DatasetSourcesReference(reference_id);

CREATE TABLE "SamplingMethodID"
("sampling_method_id" INTEGER PRIMARY KEY,"sampling_method_details" TEXT);
CREATE UNIQUE INDEX idx_samplingmethodid_sampling_method_id ON SamplingMethodID(sampling_method_id);

CREATE TABLE "SamplingMethodReference"
("sampling_method_id" INTEGER,"reference_id" INTEGER,
 FOREIGN KEY ("sampling_method_id") REFERENCES "SamplingMethodID" ("sampling_method_id"),
 FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id"));
CREATE INDEX idx_samplingmethodreference_sampling_method_id ON SamplingMethodReference(sampling_method_id);
CREATE INDEX idx_samplingmethodreference_reference_id ON SamplingMethodReference(reference_id);

CREATE TABLE "Datasets"
("dataset_id" INTEGER PRIMARY KEY,"dataset_name" TEXT,"data_source_id" INTEGER,"dataset_type_id" INTEGER,
 "data_source_type_id" INTEGER,"coord_long" REAL,"coord_lat" REAL,"sampling_method_id" INTEGER,
 FOREIGN KEY ("dataset_type_id") REFERENCES "DatasetTypeID" ("dataset_type_id"),
 FOREIGN KEY ("sampling_method_id") REFERENCES "SamplingMethodID" ("sampling_method_id"),
 FOREIGN KEY ("data_source_id") REFERENCES "DatasetSourcesID" ("data_source_id"),
 FOREIGN KEY ("data_source_type_id") REFERENCES "DatasetSourceTypeID" ("data_source_type_id"));
CREATE UNIQUE INDEX idx_datasets_dataset_id ON Datasets(dataset_id);
CREATE INDEX idx_datasets_dataset_type_id ON Datasets(dataset_type_id);
CREATE INDEX idx_datasets_data_source_id ON Datasets(data_source_id);
CREATE INDEX idx_datasets_data_source_type_id ON Datasets(data_source_type_id);
CREATE INDEX idx_datasets_coord_long_lat ON Datasets(coord_long, coord_lat);
CREATE INDEX idx_datasets_sampling_method_id ON Datasets(sampling_method_id);

CREATE TABLE "DatasetReferences"
("dataset_id" INTEGER,"reference_id" INTEGER,
 FOREIGN KEY ("dataset_id") REFERENCES "Datasets" ("dataset_id"),
 FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id"));
CREATE INDEX idx_datasetreferences_dataset_id ON DatasetReferences(dataset_id);
CREATE INDEX idx_datasetreferences_reference_id ON DatasetReferences(reference_id);

CREATE TABLE "SampleSizeID"
("sample_size_id" INTEGER PRIMARY KEY,"sample_size" REAL,"description" TEXT);
CREATE UNIQUE INDEX idx_samplesizeid_sample_size_id ON SampleSizeID(sample_size_id);

CREATE TABLE "Samples"
("sample_id" INTEGER PRIMARY KEY,"sample_name" TEXT,"sample_details" TEXT,"age" REAL,"sample_size_id" INTEGER,
 FOREIGN KEY ("sample_size_id") REFERENCES "SampleSizeID" ("sample_size_id"));
CREATE UNIQUE INDEX idx_samples_sample_id ON Samples(sample_id);
CREATE INDEX idx_samples_sample_size_id ON Samples(sample_size_id);

CREATE TABLE "SampleReference"
("sample_id" INTEGER,"reference_id" INTEGER,
 FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
 FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id"));
CREATE INDEX idx_samplereference_sample_id ON SampleReference(sample_id);
CREATE INDEX idx_samplereference_reference_id ON SampleReference(reference_id);

CREATE TABLE "DatasetSample"
("dataset_id" INTEGER,"sample_id" INTEGER,
 FOREIGN KEY ("dataset_id") REFERENCES "Datasets" ("dataset_id"),
 FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"));
CREATE INDEX idx_datasetsample_dataset_id ON DatasetSample(dataset_id);
CREATE INDEX idx_datasetsample_sample_id ON DatasetSample(sample_id);

CREATE TABLE "SampleUncertainty"
("sample_id" INTEGER,"iteration" INTEGER,"age" INTEGER,
 FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"));
CREATE INDEX idx_sampleuncertainty_sample_id ON SampleUncertainty(sample_id);

CREATE TABLE "Taxa"
("taxon_id" INTEGER PRIMARY KEY,"taxon_name" TEXT);
CREATE UNIQUE INDEX idx_taxa_taxon_id ON Taxa(taxon_id);

CREATE TABLE "TaxonClassification"
("taxon_id" INTEGER,"taxon_species" INTEGER,"taxon_genus" INTEGER,"taxon_family" INTEGER,
 FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id"),
 FOREIGN KEY ("taxon_species") REFERENCES "Taxa" ("taxon_id"),
 FOREIGN KEY ("taxon_genus") REFERENCES "Taxa" ("taxon_id"),
 FOREIGN KEY ("taxon_family") REFERENCES "Taxa" ("taxon_id"));
CREATE INDEX idx_taxonclassification_taxon_id ON TaxonClassification(taxon_id);
CREATE INDEX idx_taxonclassification_taxon_species ON TaxonClassification(taxon_species);
CREATE INDEX idx_taxonclassification_taxon_genus ON TaxonClassification(taxon_genus);
CREATE INDEX idx_taxonclassification_taxon_family ON TaxonClassification(taxon_family);

CREATE TABLE "TaxonReference"
("taxon_id" INTEGER,"reference_id" INTEGER,
 FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id"),
 FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id"));
CREATE INDEX idx_taxonreference_taxon_id ON TaxonReference(taxon_id);
CREATE INDEX idx_taxonreference_reference_id ON TaxonReference(reference_id);

CREATE TABLE "SampleTaxa"
("sample_id" INTEGER,"taxon_id" INTEGER,"value" REAL,
 FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
 FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id"));
CREATE INDEX idx_sampletaxa_sample_id ON SampleTaxa(sample_id);
CREATE INDEX idx_sampletaxa_taxon_id ON SampleTaxa(taxon_id);
CREATE INDEX idx_sampletaxa_sample_id_taxon_id ON SampleTaxa(sample_id, taxon_id);

CREATE TABLE "TraitsDomain"
("trait_domain_id" INTEGER PRIMARY KEY,"trait_domain_name" TEXT,"trait_domanin_description" TEXT);
CREATE UNIQUE INDEX idx_traitsdomain_trait_domain_id ON TraitsDomain(trait_domain_id);

CREATE TABLE "Traits"
("trait_id" INTEGER PRIMARY KEY,"trait_domain_id" INTEGER,"trait_name" TEXT,
 FOREIGN KEY ("trait_domain_id") REFERENCES "TraitsDomain" ("trait_domain_id"));
CREATE UNIQUE INDEX idx_traits_trait_id ON Traits(trait_id);
CREATE INDEX idx_traits_trait_domain_id ON Traits(trait_domain_id);

CREATE TABLE "TraitsValue"
("trait_id" INTEGER,"dataset_id" INTEGER,"sample_id" INTEGER,"taxon_id" INTEGER,"trait_value" REAL,
 FOREIGN KEY ("trait_id") REFERENCES "Traits" ("trait_id"),
 FOREIGN KEY ("dataset_id") REFERENCES "Datasets" ("dataset_id"),
 FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
 FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id"));
CREATE INDEX idx_traitsvalue_trait_id ON TraitsValue(trait_id);
CREATE INDEX idx_traitsvalue_dataset_id ON TraitsValue(dataset_id);
CREATE INDEX idx_traitsvalue_sample_id ON TraitsValue(sample_id);
CREATE INDEX idx_traitsvalue_taxon_id ON TraitsValue(taxon_id);
CREATE INDEX idx_traitsvalue_dataset_id_sample_id_taxon_id ON TraitsValue(dataset_id, sample_id, taxon_id);

CREATE TABLE "TraitsReference"
("trait_id" INTEGER,"reference_id" INTEGER,
 FOREIGN KEY ("trait_id") REFERENCES "Traits" ("trait_id"),
 FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id"));
CREATE INDEX idx_traitsreference_trait_id ON TraitsReference(trait_id);
CREATE INDEX idx_traitsreference_reference_id ON TraitsReference(reference_id);

CREATE TABLE "AbioticVariable"
("abiotic_variable_id" INTEGER PRIMARY KEY,"abiotic_variable_name" TEXT,
 "abiotic_variable_unit" TEXT,"measure_details" TEXT);
CREATE UNIQUE INDEX idx_abioticvariable_abiotic_variable_id ON AbioticVariable(abiotic_variable_id);

CREATE TABLE "AbioticVariableReference"
("abiotic_variable_id" INTEGER,"reference_id" INTEGER,
 FOREIGN KEY ("abiotic_variable_id") REFERENCES "AbioticVariable" ("abiotic_variable_id"),
 FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id"));
CREATE INDEX idx_abioticvariablereference_abiotic_variable_id ON AbioticVariableReference(abiotic_variable_id);
CREATE INDEX idx_abioticvariablereference_reference_id ON AbioticVariableReference(reference_id);

CREATE TABLE "AbioticData"
("sample_id" INTEGER,"abiotic_variable_id" INTEGER,"abiotic_value" REAL,
 FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
 FOREIGN KEY ("abiotic_variable_id") REFERENCES "AbioticVariable" ("abiotic_variable_id"));
CREATE INDEX idx_abioticdata_sample_id ON AbioticData(sample_id);
CREATE INDEX idx_abioticdata_abiotic_variable_id ON AbioticData(abiotic_variable_id);

CREATE TABLE "AbioticDataReference"
("sample_id" INTEGER,"sample_ref_id" INTEGER,"distance_in_km" INTEGER,"distance_in_years" INTEGER,
 FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
 FOREIGN KEY ("sample_ref_id") REFERENCES "Samples" ("sample_id"));
CREATE INDEX idx_abioticdatareference_sample_id ON AbioticDataReference(sample_id);
CREATE INDEX idx_abioticdatareference_sample_ref_id ON AbioticDataReference(sample_ref_id);
'

# ── main builder ─────────────────────────────────────────────────────────────

path_db <- paste(tempdir(), "vaultkeepr_example.sqlite", sep = "/")

if (!file.exists(path_db)) {
  con_db <-
    DBI::dbConnect(RSQLite::SQLite(), path_db)

  # 1. Create schema -----------------------------------------------------------
  .sql_schema %>%
    stringr::str_remove_all("\\n") %>%
    stringr::str_split(pattern = "\\;") %>%
    unlist() %>%
    purrr::walk(
      ~ try(
        DBI::dbExecute(conn = con_db, statement = .x),
        silent = TRUE
      )
    )

  # 2. Lookup tables -----------------------------------------------------------

  tibble::tibble(
    dataset_type = c(
      "vegetation_plot",
      "fossil_pollen_archive",
      "traits",
      "gridpoints"
    )
  ) %>%
    dplyr::copy_to(con_db, df = ., name = "DatasetTypeID", append = TRUE)

  tibble::tibble(
    dataset_source_type = c(
      "BIEN",
      "sPlotOpen",
      "Neotoma-FOSSILPOL",
      "gridpoints"
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "DatasetSourceTypeID",
      append = TRUE
    )

  tibble::tibble(
    data_source_desc = c(
      "EVA (European Vegetation Archive)",
      "Neotoma Paleoecology Database",
      "TRY Plant Trait Database"
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "DatasetSourcesID",
      append = TRUE
    )

  tibble::tibble(
    sampling_method_details = c(
      "Standardised vegetation plot survey (1–1000 m²)",
      "Sediment core – pollen analysis",
      "Botanical measurement (herbarium or garden)"
    )
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "SamplingMethodID",
      append = TRUE
    )

  # 3. Datasets ----------------------------------------------------------------
  # 6 coordinate clusters (one per continent)
  data_coord_clusters <-
    tibble::tribble(
      ~coord_long, ~coord_lat,
      -115, 45, # Western North America
      15, 45, # Europe
      115, 45, # East Asia
      -60, -15, # South America
      -15, -30, # Southern Africa
      -135, -30 # Pacific
    )

  # gridpoints: 5 slight jitters per cluster, dataset_type_id = 4
  .set_seed()
  data_datasets_gridpoints <-
    purrr::map(
      .x = seq_len(5),
      .f = ~ data_coord_clusters %>%
        dplyr::mutate(
          coord_long = coord_long + runif(1, -0.05, 0.05),
          coord_lat  = coord_lat + runif(1, -0.05, 0.05)
        )
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      data_source_id      = 4L,
      dataset_type_id     = 4L,
      data_source_type_id = 4L,
      sampling_method_id  = NA_integer_
    )

  # vegetation / pollen / traits datasets: all combinations
  data_datasets_vegetation <-
    tidyr::expand_grid(
      data_coord_clusters,
      data_source_id      = 1:3,
      dataset_type_id     = 1:3,
      data_source_type_id = 1:3,
      sampling_method_id  = 1:3
    )

  data_datasets <-
    dplyr::bind_rows(data_datasets_vegetation, data_datasets_gridpoints) %>%
    dplyr::mutate(dataset_name = paste0("dataset_", dplyr::row_number()))

  dplyr::copy_to(con_db, data_datasets, name = "Datasets", append = TRUE)

  # Dataset reference tables
  .add_references(
    con_db,
    "DatasetSourcesID", "DatasetSourcesReference",
    "data_source_id", "data_source",
    n_refs = 3
  )
  .add_references(
    con_db,
    "DatasetSourceTypeID", "DatasetSourceTypeReference",
    "data_source_type_id", "data_source_type",
    mandatory = TRUE, n_refs = 3
  )
  .add_references(
    con_db,
    "SamplingMethodID", "SamplingMethodReference",
    "sampling_method_id", "sampling_method",
    n_refs = 3
  )
  .add_references(
    con_db,
    "Datasets", "DatasetReferences",
    "dataset_id", "dataset",
    n_refs = 5
  )

  # 4. Samples -----------------------------------------------------------------
  # 50 replicates × 10 size classes × 17 age steps = 8500 samples
  tidyr::expand_grid(
    init = seq_len(50),
    sample_size_id = seq_len(10),
    age = seq(0, 8000, by = 500)
  ) %>%
    dplyr::mutate(sample_name = paste0("sample_", dplyr::row_number())) %>%
    dplyr::select(-"init") %>%
    dplyr::copy_to(con_db, df = ., name = "Samples", append = TRUE)

  .add_references(
    con_db,
    "Samples", "SampleReference",
    "sample_id", "sample",
    n_refs = 10
  )

  # Assign samples to datasets respecting dataset-type semantics:
  # - vegetation_plot (1) and traits (3) are contemporary: age = 0 only
  # - fossil_pollen_archive (2) and gridpoints (4) carry temporal data

  vec_dataset_ids_contemporary <-
    which(data_datasets$dataset_type_id %in% c(1L, 3L))

  vec_dataset_ids_temporal <-
    which(data_datasets$dataset_type_id %in% c(2L, 4L))

  vec_sample_ids_age0 <-
    dplyr::tbl(con_db, "Samples") %>%
    dplyr::filter(age == 0) %>%
    dplyr::pull(sample_id)

  vec_sample_ids_temporal <-
    dplyr::tbl(con_db, "Samples") %>%
    dplyr::filter(age > 0) %>%
    dplyr::pull(sample_id)

  data_dataset_sample <-
    dplyr::bind_rows(
      tibble::tibble(
        dataset_id = rep_len(
          vec_dataset_ids_contemporary,
          length(vec_sample_ids_age0)
        ),
        sample_id = vec_sample_ids_age0
      ),
      tibble::tibble(
        dataset_id = rep_len(
          vec_dataset_ids_temporal,
          length(vec_sample_ids_temporal)
        ),
        sample_id = vec_sample_ids_temporal
      )
    )

  dplyr::copy_to(
    con_db,
    data_dataset_sample,
    name = "DatasetSample",
    append = TRUE
  )

  # 5. Taxa & classification ---------------------------------------------------
  # 45 species (taxon_1–45), 9 genera (46–54), 3 families (55–57)
  tibble::tibble(
    taxon_name = c(
      # 45 species (5 per genus)
      # Poa (genus 46)
      "Poa annua", "Poa pratensis", "Poa trivialis",
      "Poa alpina", "Poa nemoralis",
      # Festuca (genus 47)
      "Festuca rubra", "Festuca ovina", "Festuca pratensis",
      "Festuca arundinacea", "Festuca valesiaca",
      # Bromus (genus 48)
      "Bromus erectus", "Bromus hordeaceus", "Bromus sterilis",
      "Bromus tectorum", "Bromus commutatus",
      # Artemisia (genus 49)
      "Artemisia vulgaris", "Artemisia absinthium",
      "Artemisia campestris", "Artemisia annua",
      "Artemisia maritima",
      # Senecio (genus 50)
      "Senecio vulgaris", "Senecio jacobaea",
      "Senecio sylvaticus", "Senecio viscosus",
      "Senecio erucifolius",
      # Helianthus (genus 51)
      "Helianthus annuus", "Helianthus tuberosus",
      "Helianthus petiolaris", "Helianthus debilis",
      "Helianthus argophyllus",
      # Betula (genus 52)
      "Betula pendula", "Betula pubescens", "Betula nana",
      "Betula humilis", "Betula platyphylla",
      # Alnus (genus 53)
      "Alnus glutinosa", "Alnus incana", "Alnus viridis",
      "Alnus cordata", "Alnus rhombifolia",
      # Corylus (genus 54)
      "Corylus avellana", "Corylus maxima", "Corylus colurna",
      "Corylus americana", "Corylus sieboldiana",
      # 9 genera (positions 46–54)
      "Poa", "Festuca", "Bromus",
      "Artemisia", "Senecio", "Helianthus",
      "Betula", "Alnus", "Corylus",
      # 3 families (positions 55–57)
      "Poaceae", "Asteraceae", "Betulaceae"
    )
  ) %>%
    dplyr::copy_to(con_db, df = ., name = "Taxa", append = TRUE)

  .add_references(
    con_db,
    "Taxa", "TaxonReference",
    "taxon_id", "taxon",
    n_refs = 5
  )

  data_classification <-
    tibble::tibble(
      taxon_species = seq_len(45),
      taxon_genus   = rep(46:54, each = 5),
      taxon_family  = rep(55:57, each = 15)
    )

  dplyr::bind_rows(
    data_classification %>%
      dplyr::mutate(taxon_id = taxon_species) %>%
      dplyr::relocate("taxon_id"),
    data_classification %>%
      dplyr::distinct(taxon_genus, taxon_family) %>%
      dplyr::mutate(taxon_id = taxon_genus)
  ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "TaxonClassification",
      append = TRUE
    )

  # SampleTaxa: abundance records for vegetation + pollen samples
  # Vegetation plots (type 1): all 9 genera always present.
  # Fossil pollen archives (type 2): staggered Holocene succession —
  # tree genera (Betula, Alnus, Corylus) appear progressively in older
  # strata; herb/grass genera (Poa–Helianthus) drop out — producing a
  # hump-shaped richness curve typical of Holocene pollen records.
  data_veg_sample_ages <-
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
    dplyr::distinct(
      .data$sample_id, .data$age, .data$dataset_type_id
    ) %>%
    dplyr::collect()

  .set_seed()
  data_veg_sample_ages %>%
    tidyr::expand_grid(taxon_id = seq_len(45L)) %>%
    dplyr::filter(
      # Vegetation plots (type 1): all genera present (contemporary record)
      .data$dataset_type_id == 1L |
        # Fossil pollen archives (type 2): temporal genus succession
        # Herb/grass genera absent in strata older than their threshold:
        (
          !(.data$taxon_id <= 5L & .data$age > 6000L) & # Poa
            !(.data$taxon_id >= 6L & .data$taxon_id <= 10L &
              .data$age > 5500L) & # Festuca
            !(.data$taxon_id >= 11L & .data$taxon_id <= 15L &
              .data$age > 5000L) & # Bromus
            !(.data$taxon_id >= 16L & .data$taxon_id <= 20L &
              .data$age > 4500L) & # Artemisia
            !(.data$taxon_id >= 21L & .data$taxon_id <= 25L &
              .data$age > 4000L) & # Senecio
            !(.data$taxon_id >= 26L & .data$taxon_id <= 30L &
              .data$age > 3500L) & # Helianthus
            # Tree genera absent in strata younger than their threshold:
            !(.data$taxon_id >= 31L & .data$taxon_id <= 35L &
              .data$age < 1000L) & # Betula
            !(.data$taxon_id >= 36L & .data$taxon_id <= 40L &
              .data$age < 2000L) & # Alnus
            !(.data$taxon_id >= 41L & .data$taxon_id <= 45L &
              .data$age < 3000L) # Corylus
        )
    ) %>%
    dplyr::mutate(
      age_norm = age / 8000,
      base_abund = dplyr::if_else(
        taxon_id >= 31L,
        50 * age_norm + 5,
        50 * (1 - age_norm) + 5
      ),
      value = round(
        base_abund * stats::runif(dplyr::n(), 0.6, 1.4),
        digits = 1L
      )
    ) %>%
    dplyr::select(sample_id, taxon_id, value) %>%
    dplyr::copy_to(con_db, df = ., name = "SampleTaxa", append = TRUE)

  # 6. Abiotic data ------------------------------------------------------------
  tibble::tibble(
    abiotic_variable_name = c("bio1", "bio12", "HWSD2"),
    abiotic_variable_unit = c(
      "\u00b0C",
      "kg m-2 year-1",
      "Unitless"
    ),
    measure_details = c(
      "mean annual air temperature",
      "annual precipitation amount",
      "SoilGrids-soil class"
    )
  ) %>%
    dplyr::copy_to(con_db, df = ., name = "AbioticVariable", append = TRUE)

  vec_abiotic_sample_ids <-
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
    sample_id           = rep(vec_abiotic_sample_ids, each = 3),
    abiotic_variable_id = rep(1:3, length(vec_abiotic_sample_ids)),
    abiotic_value       = rep(c(-5, 0, 5), length(vec_abiotic_sample_ids))
  ) %>%
    dplyr::copy_to(con_db, df = ., name = "AbioticData", append = TRUE)

  .add_references(
    con_db,
    "AbioticVariable", "AbioticVariableReference",
    "abiotic_variable_id", "abiotic_variable",
    mandatory = TRUE, n_refs = 3
  )

  # AbioticDataReference: link vegetation/pollen samples to nearby gridpoint
  # samples (within 50 km and 5000 years)
  data_abiotic_geo <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id == 4) %>%
    dplyr::inner_join(
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(dplyr::tbl(con_db, "Samples"), by = "sample_id") %>%
    dplyr::distinct(sample_id, coord_lat, coord_long) %>%
    dplyr::collect() %>%
    dplyr::rename_with(~ paste0(.x, "_abiotic"))

  data_vegetation_geo <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id %in% 1:2) %>%
    dplyr::inner_join(
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::left_join(dplyr::tbl(con_db, "Samples"), by = "sample_id") %>%
    dplyr::distinct(sample_id, coord_lat, coord_long) %>%
    dplyr::collect() %>%
    dplyr::rename_with(~ paste0(.x, "_vegetation"))

  data_vegetation_geo_nest <-
    data_vegetation_geo %>%
    dplyr::group_by(coord_lat_vegetation, coord_long_vegetation) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dummy_id = dplyr::row_number())

  data_vegetation_geo_lookup <-
    data_vegetation_geo_nest %>%
    tidyr::unnest(data) %>%
    dplyr::distinct(dummy_id, sample_id_vegetation)

  tidyr::expand_grid(
    data_abiotic_geo,
    data_vegetation_geo_nest %>%
      dplyr::select(coord_lat_vegetation, coord_long_vegetation, dummy_id)
  ) %>%
    dplyr::mutate(
      distance_in_km = purrr::pmap_dbl(
        .l = list(
          coord_long_abiotic, coord_lat_abiotic,
          coord_long_vegetation, coord_lat_vegetation
        ),
        .f = ~ geosphere::distGeo(
          p1 = c(..1, ..2),
          p2 = c(..3, ..4)
        ) / 1e3
      )
    ) %>%
    dplyr::filter(distance_in_km <= 50) %>%
    dplyr::select(!dplyr::starts_with("coord_")) %>%
    dplyr::left_join(
      data_vegetation_geo_lookup,
      by = "dummy_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::select(-"dummy_id") %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "Datasets") %>%
        dplyr::filter(dataset_type_id == 4) %>%
        dplyr::inner_join(
          dplyr::tbl(con_db, "DatasetSample"),
          by = "dataset_id"
        ) %>%
        dplyr::left_join(dplyr::tbl(con_db, "Samples"), by = "sample_id") %>%
        dplyr::distinct(sample_id, age) %>%
        dplyr::collect(),
      by = c("sample_id_abiotic" = "sample_id")
    ) %>%
    dplyr::rename(age_abiotic = "age") %>%
    dplyr::left_join(
      dplyr::tbl(con_db, "Datasets") %>%
        dplyr::filter(dataset_type_id != 4) %>%
        dplyr::inner_join(
          dplyr::tbl(con_db, "DatasetSample"),
          by = "dataset_id"
        ) %>%
        dplyr::left_join(dplyr::tbl(con_db, "Samples"), by = "sample_id") %>%
        dplyr::distinct(sample_id, age) %>%
        dplyr::collect(),
      by = c("sample_id_vegetation" = "sample_id")
    ) %>%
    dplyr::rename(age_vegetation = "age") %>%
    dplyr::mutate(
      distance_in_years = abs(age_abiotic - age_vegetation)
    ) %>%
    dplyr::filter(distance_in_years <= 5e3) %>%
    dplyr::distinct(
      sample_id_vegetation, sample_id_abiotic,
      distance_in_km, distance_in_years
    ) %>%
    dplyr::rename(
      sample_id     = sample_id_vegetation,
      sample_ref_id = sample_id_abiotic
    ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "AbioticDataReference",
      append = TRUE
    )

  # 7. Traits ------------------------------------------------------------------
  tibble::tibble(
    trait_domain_name = c(
      "Stem specific density",
      "Diaspore mass",
      "Plant heigh"
    ),
    trait_domanin_description = c(
      "Stem dry mass per stem fresh volume (wood density)",
      "Dispersal unit (diaspore) dry mass",
      "Maximum whole-plant vegetative height"
    )
  ) %>%
    dplyr::copy_to(con_db, df = ., name = "TraitsDomain", append = TRUE)

  tibble::tibble(
    trait_domain_id = rep(1:3, each = 3),
    trait_name = c(
      # Stem specific density (domain 1)
      "stem wood density",
      "Stem specific density (SSD, stem dry mass per stem fresh volume)",
      "wood density (dry/fresh volume)",
      # Diaspore mass (domain 2)
      "seed mass",
      "Seed dry mass",
      "dispersule dry mass with additional features",
      # Plant heigh (domain 3)
      "whole plant height",
      "Plant height vegetative",
      "height generative"
    )
  ) %>%
    dplyr::copy_to(con_db, df = ., name = "Traits", append = TRUE)

  data_trait_dataset_sample <-
    dplyr::tbl(con_db, "Datasets") %>%
    dplyr::filter(dataset_type_id == 3) %>%
    dplyr::inner_join(
      dplyr::tbl(con_db, "DatasetSample"),
      by = "dataset_id"
    ) %>%
    dplyr::distinct(sample_id, dataset_id) %>%
    dplyr::collect()

  # Per-species plant height values (metres) for the "Plant heigh" domain:
  # grasses 0.3-1.2 m, composites/tall-herbs 0.3-3.5 m,
  # trees/shrubs 3-30 m — ensures a clear CWM contrast between age bins.
  vec_plant_heights <- c(
    # Poa spp. (1-5)
    0.3, 0.4, 0.5, 0.6, 0.7,
    # Festuca spp. (6-10)
    0.4, 0.5, 0.6, 0.7, 0.8,
    # Bromus spp. (11-15)
    0.5, 0.7, 0.8, 1.0, 1.2,
    # Artemisia spp. (16-20)
    0.5, 0.7, 1.0, 1.3, 1.5,
    # Senecio spp. (21-25)
    0.3, 0.5, 0.7, 1.0, 1.2,
    # Helianthus spp. (26-30)
    1.0, 1.5, 2.0, 2.5, 3.5,
    # Betula spp. (31-35)
    8.0, 12.0, 18.0, 22.0, 25.0,
    # Alnus spp. (36-40)
    10.0, 15.0, 20.0, 25.0, 30.0,
    # Corylus spp. (41-45)
    3.0, 4.0, 5.0, 6.0, 8.0
  )

  dplyr::bind_rows(
    # Non-height traits (ids 1-6): simple non-zero values
    tidyr::expand_grid(
      trait_id = seq_len(6L),
      data_trait_dataset_sample
    ) %>%
      dplyr::mutate(
        taxon_id    = rep_len(seq_len(45L), length.out = nrow(.)),
        trait_value = rep_len(c(0.2, 0.5, 0.8), length.out = nrow(.))
      ),
    # Plant height traits (ids 7-9): realistic per-species heights
    tidyr::expand_grid(
      trait_id = 7L:9L,
      data_trait_dataset_sample
    ) %>%
      dplyr::mutate(
        taxon_id = rep_len(seq_len(45L), length.out = nrow(.)),
        trait_value = vec_plant_heights[taxon_id] *
          rep_len(c(0.95, 1.0, 1.05), length.out = nrow(.))
      )
  ) %>%
    dplyr::copy_to(con_db, df = ., name = "TraitsValue", append = TRUE)

  .add_references(
    con_db,
    "Traits", "TraitsReference",
    "trait_id", "trait",
    n_refs = 5
  )

  # 8. Sample uncertainty ------------------------------------------------------
  # 25 age-model iterations per temporal sample; SD baseline raised to 100
  # so that at 500 yr BP (SD=125) ~32 % of samples cross a 250-year bin
  # boundary, producing visible spread in the spaghetti richness plot.
  .set_seed()

  data_temporal_samples <-
    dplyr::tbl(con_db, "Samples") %>%
    dplyr::filter(age > 0) %>%
    dplyr::select("sample_id", "age") %>%
    dplyr::collect()

  tidyr::expand_grid(
    data_temporal_samples,
    iteration = seq_len(25L)
  ) %>%
    dplyr::mutate(
      age = as.integer(
        pmax(
          0L,
          age + round(
            stats::rnorm(
              dplyr::n(),
              mean = 0,
              sd   = age * 0.05 + 100
            )
          )
        )
      )
    ) %>%
    dplyr::copy_to(
      con_db,
      df = .,
      name = "SampleUncertainty",
      append = TRUE
    )

  DBI::dbDisconnect(con_db)
}

path_db
