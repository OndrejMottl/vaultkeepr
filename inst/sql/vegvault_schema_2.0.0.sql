-- VegVault 2.0.0 database schema, as shipped with vaultkeepr
--
-- Copied without changes, below this header, from Data/SQL/make_tables.sql
-- at commit f92caaa of the 2.0.0 branch of
-- https://github.com/OndrejMottl/VegVault. The functions and files named in
-- the original header below belong to that repository. The test database
-- (tests/testthat/helper_make_database.R) and the vignette database
-- (vignettes/helper_make_example_db.R) create their tables, view and
-- indexes from this file, statement by statement.

-- VegVault database schema, version 2.0.0
--
-- The executable schema of the VegVault SQLite database. It is built with
--   build_database_schema() (R/Functions/Database/), statement by statement,
--   and checked with validate_database_schema(). The conceptual mirror is
--   database_structure.dbml in this folder. Change both files together.
--   The decisions behind this version are recorded at the end of
--   Documentation/Database_design_2.0.0.md.
--
-- Layout of this file:
--   1. Tables and the view. Keys, UNIQUE, NOT NULL, CHECK and foreign keys
--      are declared in the tables and are active while rows are loaded.
--   2. Secondary indexes. Every CREATE INDEX statement is in section 2, so
--      that a bulk load can create the tables first and the indexes after
--      the rows (build_database_schema() with sel_objects "tables", then
--      "indexes").
--
-- Semicolons appear only at the end of statements: two test scripts still
--   split this file at every semicolon.


----------------------------------------------------------------------------
-- 1. Tables and the view
----------------------------------------------------------------------------

-- Versions
-- One row per database version, written in the order of `database:
--   versions` in config.yml, so the current version has the highest id.
--   The build columns are empty for the versions before 2.0.0:
--   build_profile is the config profile of the build, git_commit the
--   commit of the VegVault code and manifest_sha256 the SHA-256 of the
--   producer pin manifest (Data/Producers/producer_manifest.yml).
CREATE TABLE "version_control"
(
  "id" INTEGER PRIMARY KEY,
  "version" TEXT NOT NULL UNIQUE,
  "update_date" TEXT DEFAULT CURRENT_DATE,
  "changelog" TEXT,
  "build_profile" TEXT,
  "git_commit" TEXT,
  "manifest_sha256" TEXT
);

-- One row per producer pin of a build: producer is the artefact id of the
--   pin manifest, tag the producer release tag and commit_sha the commit
--   of that tag. tag and commit_sha are empty in a development build that
--   reads local producer files. The release validation requires them.
CREATE TABLE "BuildProducers"
(
  "version_id" INTEGER NOT NULL,
  "producer" TEXT NOT NULL,
  "tag" TEXT,
  "commit_sha" TEXT,
  PRIMARY KEY ("version_id", "producer"),
  FOREIGN KEY ("version_id") REFERENCES "version_control" ("id")
) WITHOUT ROWID;

CREATE TABLE "Authors"
(
  "author_id" INTEGER PRIMARY KEY,
  "author_fullname" TEXT,
  "author_email" TEXT,
  "author_orcid" TEXT
);

-- Datasets
-- Dataset names are the natural key. Coordinates may be missing (some
--   trait datasets have none) but never outside the globe.
CREATE TABLE "Datasets"
(
  "dataset_id" INTEGER PRIMARY KEY,
  "dataset_name" TEXT NOT NULL UNIQUE,
  "data_source_id" INTEGER,
  "dataset_type_id" INTEGER,
  "data_source_type_id" INTEGER,
  "coord_long" REAL CHECK ("coord_long" BETWEEN -180 AND 180),
  "coord_lat" REAL CHECK ("coord_lat" BETWEEN -90 AND 90),
  "sampling_method_id" INTEGER,
  FOREIGN KEY ("dataset_type_id") REFERENCES "DatasetTypeID" ("dataset_type_id"),
  FOREIGN KEY ("sampling_method_id") REFERENCES "SamplingMethodID" ("sampling_method_id"),
  FOREIGN KEY ("data_source_id") REFERENCES "DatasetSourcesID" ("data_source_id"),
  FOREIGN KEY ("data_source_type_id") REFERENCES "DatasetSourceTypeID" ("data_source_type_id")
);

-- Junction and value tables with a composite primary key are WITHOUT ROWID
--   tables: the table is stored as the index of its key, so rows load
--   fastest in key order.
CREATE TABLE "DatasetReferences"
(
  "dataset_id" INTEGER NOT NULL,
  "reference_id" INTEGER NOT NULL,
  PRIMARY KEY ("dataset_id", "reference_id"),
  FOREIGN KEY ("dataset_id") REFERENCES "Datasets" ("dataset_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
) WITHOUT ROWID;

CREATE TABLE "DatasetTypeID"
(
  "dataset_type_id" INTEGER PRIMARY KEY,
  "dataset_type" TEXT
);

CREATE TABLE "DatasetSourceTypeID"
(
  "data_source_type_id" INTEGER PRIMARY KEY,
  "dataset_source_type" TEXT
);

CREATE TABLE "DatasetSourceTypeReference"
(
  "data_source_type_id" INTEGER NOT NULL,
  "reference_id" INTEGER NOT NULL,
  PRIMARY KEY ("data_source_type_id", "reference_id"),
  FOREIGN KEY ("data_source_type_id") REFERENCES "DatasetSourceTypeID" ("data_source_type_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
) WITHOUT ROWID;

CREATE TABLE "DatasetSourcesID"
(
  "data_source_id" INTEGER PRIMARY KEY,
  "data_source_desc" TEXT
);

CREATE TABLE "DatasetSourcesReference"
(
  "data_source_id" INTEGER NOT NULL,
  "reference_id" INTEGER NOT NULL,
  PRIMARY KEY ("data_source_id", "reference_id"),
  FOREIGN KEY ("data_source_id") REFERENCES "DatasetSourcesID" ("data_source_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
) WITHOUT ROWID;

CREATE TABLE "SamplingMethodID"
(
  "sampling_method_id" INTEGER PRIMARY KEY,
  "sampling_method_details" TEXT
);

CREATE TABLE "SamplingMethodReference"
(
  "sampling_method_id" INTEGER NOT NULL,
  "reference_id" INTEGER NOT NULL,
  PRIMARY KEY ("sampling_method_id", "reference_id"),
  FOREIGN KEY ("sampling_method_id") REFERENCES "SamplingMethodID" ("sampling_method_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
) WITHOUT ROWID;

-- Samples
-- Sample names are the natural key.
CREATE TABLE "Samples"
(
  "sample_id" INTEGER PRIMARY KEY,
  "sample_name" TEXT NOT NULL UNIQUE,
  "sample_details" TEXT,
  "age" REAL,
  "sample_size_id" INTEGER,
  FOREIGN KEY ("sample_size_id") REFERENCES "SampleSizeID" ("sample_size_id")
);

CREATE TABLE "SampleReference"
(
  "sample_id" INTEGER NOT NULL,
  "reference_id" INTEGER NOT NULL,
  PRIMARY KEY ("sample_id", "reference_id"),
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
) WITHOUT ROWID;

CREATE TABLE "DatasetSample"
(
  "dataset_id" INTEGER NOT NULL,
  "sample_id" INTEGER NOT NULL,
  PRIMARY KEY ("dataset_id", "sample_id"),
  FOREIGN KEY ("dataset_id") REFERENCES "Datasets" ("dataset_id"),
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id")
) WITHOUT ROWID;

CREATE TABLE "SampleUncertainty"
(
  "sample_id" INTEGER NOT NULL,
  "iteration" INTEGER NOT NULL,
  "age" INTEGER,
  PRIMARY KEY ("sample_id", "iteration"),
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id")
) WITHOUT ROWID;

CREATE TABLE "SampleSizeID"
(
  "sample_size_id" INTEGER PRIMARY KEY,
  "sample_size" REAL,
  "description" TEXT
);

-- SampleTaxa keeps its rowid and has no primary key: v1.0.0 repeats some
--   (sample_id, taxon_id) pairs, which the release validation flags until
--   the vegetation producers resolve them.
CREATE TABLE "SampleTaxa"
(
  "sample_id" INTEGER,
  "taxon_id" INTEGER,
  "value" REAL,
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
  FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id")
);

-- Taxa
CREATE TABLE "Taxa"
(
  "taxon_id" INTEGER PRIMARY KEY,
  "taxon_name" TEXT
);

CREATE TABLE "TaxonClassification"
(
  "taxon_id" INTEGER PRIMARY KEY,
  "taxon_species" INTEGER,
  "taxon_genus" INTEGER,
  "taxon_family" INTEGER,
  FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id"),
  FOREIGN KEY ("taxon_species") REFERENCES "Taxa" ("taxon_id"),
  FOREIGN KEY ("taxon_genus") REFERENCES "Taxa" ("taxon_id"),
  FOREIGN KEY ("taxon_family") REFERENCES "Taxa" ("taxon_id")
);

CREATE TABLE "TaxonReference"
(
  "taxon_id" INTEGER NOT NULL,
  "reference_id" INTEGER NOT NULL,
  PRIMARY KEY ("taxon_id", "reference_id"),
  FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
) WITHOUT ROWID;

-- Traits
CREATE TABLE "TraitsDomain"
(
  "trait_domain_id" INTEGER PRIMARY KEY,
  "trait_domain_name" TEXT,
  "trait_unit" TEXT NOT NULL,
  "trait_domanin_description" TEXT
);

CREATE TABLE "TraitUnitConversion"
(
  "trait_unit_conversion_id" INTEGER PRIMARY KEY,
  "conversion_rule_key" TEXT NOT NULL UNIQUE,
  "rule_version" TEXT NOT NULL,
  "source_provider" TEXT NOT NULL,
  "source_trait_id" TEXT NOT NULL,
  "source_dataset_key" TEXT,
  "normalized_original_unit" TEXT NOT NULL,
  "standardized_unit" TEXT NOT NULL,
  "trait_dimension" TEXT NOT NULL,
  "operation" TEXT NOT NULL,
  "multiplier" REAL NOT NULL,
  "evidence" TEXT NOT NULL
);

CREATE TABLE "TraitDerivation"
(
  "trait_derivation_id" INTEGER PRIMARY KEY,
  "derivation_rule_key" TEXT NOT NULL UNIQUE,
  "rule_version" TEXT NOT NULL,
  "input_unit" TEXT NOT NULL,
  "output_unit" TEXT NOT NULL,
  "trait_dimension" TEXT NOT NULL,
  "operation" TEXT NOT NULL,
  "multiplier" REAL NOT NULL,
  "evidence" TEXT NOT NULL
);

CREATE TABLE "Traits"
(
  "trait_id" INTEGER PRIMARY KEY,
  "trait_domain_id" INTEGER,
  "trait_name" TEXT,
  FOREIGN KEY ("trait_domain_id") REFERENCES "TraitsDomain" ("trait_domain_id")
);

-- The key starts with the dataset and the sample, the order in which trait
--   values are read. Every value has its unit-conversion rule.
CREATE TABLE "TraitsValue"
(
  "trait_id" INTEGER NOT NULL,
  "dataset_id" INTEGER NOT NULL,
  "sample_id" INTEGER NOT NULL,
  "taxon_id" INTEGER NOT NULL,
  "trait_value" REAL,
  "trait_unit_conversion_id" INTEGER NOT NULL,
  "trait_derivation_id" INTEGER,
  PRIMARY KEY ("dataset_id", "sample_id", "trait_id", "taxon_id"),
  FOREIGN KEY ("trait_id") REFERENCES "Traits" ("trait_id"),
  FOREIGN KEY ("dataset_id") REFERENCES "Datasets" ("dataset_id"),
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
  FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id"),
  FOREIGN KEY ("trait_unit_conversion_id") REFERENCES "TraitUnitConversion" ("trait_unit_conversion_id"),
  FOREIGN KEY ("trait_derivation_id") REFERENCES "TraitDerivation" ("trait_derivation_id")
) WITHOUT ROWID;

CREATE TABLE "TraitsReference"
(
  "trait_id" INTEGER NOT NULL,
  "reference_id" INTEGER NOT NULL,
  PRIMARY KEY ("trait_id", "reference_id"),
  FOREIGN KEY ("trait_id") REFERENCES "Traits" ("trait_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
) WITHOUT ROWID;

-- Abiotic data
-- The values of the gridpoint samples: one per sample and variable.
CREATE TABLE "AbioticData"
(
  "sample_id" INTEGER NOT NULL,
  "abiotic_variable_id" INTEGER NOT NULL,
  "abiotic_value" REAL,
  PRIMARY KEY ("sample_id", "abiotic_variable_id"),
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
  FOREIGN KEY ("abiotic_variable_id") REFERENCES "AbioticVariable" ("abiotic_variable_id")
) WITHOUT ROWID;

CREATE TABLE "AbioticVariable"
(
  "abiotic_variable_id" INTEGER PRIMARY KEY,
  "abiotic_variable_name" TEXT,
  "abiotic_variable_unit" TEXT,
  "measure_details" TEXT
);

CREATE TABLE "AbioticVariableReference"
(
  "abiotic_variable_id" INTEGER NOT NULL,
  "reference_id" INTEGER NOT NULL,
  PRIMARY KEY ("abiotic_variable_id", "reference_id"),
  FOREIGN KEY ("abiotic_variable_id") REFERENCES "AbioticVariable" ("abiotic_variable_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
) WITHOUT ROWID;

-- Links between samples and gridpoint samples, stored once per position
-- A position is one distinct (coord_long, coord_lat, age) of the samples
--   that are not gridpoints. Every sample at a position has the same links,
--   so they are stored once per position.
CREATE TABLE "Positions"
(
  "position_id" INTEGER PRIMARY KEY,
  "coord_long" REAL NOT NULL CHECK ("coord_long" BETWEEN -180 AND 180),
  "coord_lat" REAL NOT NULL CHECK ("coord_lat" BETWEEN -90 AND 90),
  "age" REAL NOT NULL,
  UNIQUE ("coord_long", "coord_lat", "age")
);

-- The position of each sample that is not a gridpoint.
CREATE TABLE "SamplePosition"
(
  "sample_id" INTEGER PRIMARY KEY,
  "position_id" INTEGER NOT NULL,
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
  FOREIGN KEY ("position_id") REFERENCES "Positions" ("position_id")
);

-- The gridpoint samples linked to each position: at most 50 km apart
--   (geodesic distance, rounded down to whole km) and at most 5000 years.
CREATE TABLE "PositionLink"
(
  "position_id" INTEGER NOT NULL,
  "sample_ref_id" INTEGER NOT NULL,
  "distance_in_km" INTEGER NOT NULL CHECK ("distance_in_km" BETWEEN 0 AND 50),
  "distance_in_years" INTEGER NOT NULL CHECK ("distance_in_years" BETWEEN 0 AND 5000),
  PRIMARY KEY ("position_id", "sample_ref_id"),
  FOREIGN KEY ("position_id") REFERENCES "Positions" ("position_id"),
  FOREIGN KEY ("sample_ref_id") REFERENCES "Samples" ("sample_id")
) WITHOUT ROWID;

-- References
CREATE TABLE "References"
(
  "reference_id" INTEGER PRIMARY KEY,
  "reference_detail" TEXT,
  "mandatory" BOOLEAN NOT NULL DEFAULT FALSE CHECK ("mandatory" IN (0, 1))
);

-- The links of each sample, with the name and columns of the v1.0.0 table
--   AbioticDataReference, so that readers of v1.0.0 (vaultkeepr 0.1.0) work
--   unchanged. It is a view: rows are written to the three position tables.
CREATE VIEW "AbioticDataReference" AS
SELECT
  sp."sample_id" AS "sample_id",
  pl."sample_ref_id" AS "sample_ref_id",
  pl."distance_in_km" AS "distance_in_km",
  pl."distance_in_years" AS "distance_in_years"
FROM "SamplePosition" AS sp
JOIN "PositionLink" AS pl ON pl."position_id" = sp."position_id";


----------------------------------------------------------------------------
-- 2. Secondary indexes, created after the rows are loaded
----------------------------------------------------------------------------

-- Datasets
-- idx_datasets_dataset_id and idx_samples_sample_id repeat the primary
--   key. They stay because the reference check of vaultkeepr reads them
--   faster than the tables.
CREATE UNIQUE INDEX idx_datasets_dataset_id ON Datasets(dataset_id);
CREATE INDEX idx_datasets_dataset_type_id ON Datasets(dataset_type_id);
CREATE INDEX idx_datasets_data_source_id ON Datasets(data_source_id);
CREATE INDEX idx_datasets_data_source_type_id ON Datasets(data_source_type_id);
CREATE INDEX idx_datasets_coord_long_lat ON Datasets(coord_long, coord_lat);
CREATE INDEX idx_datasets_sampling_method_id ON Datasets(sampling_method_id);
CREATE INDEX idx_datasetreferences_reference_id ON DatasetReferences(reference_id);
CREATE INDEX idx_datasetsourcetypereference_reference_id ON DatasetSourceTypeReference(reference_id);
CREATE INDEX idx_datasetsourcesreference_reference_id ON DatasetSourcesReference(reference_id);
CREATE INDEX idx_samplingmethodreference_reference_id ON SamplingMethodReference(reference_id);

-- Samples
CREATE UNIQUE INDEX idx_samples_sample_id ON Samples(sample_id);
CREATE INDEX idx_samples_sample_size_id ON Samples(sample_size_id);
CREATE INDEX idx_samplereference_reference_id ON SampleReference(reference_id);
CREATE INDEX idx_datasetsample_sample_id ON DatasetSample(sample_id);
CREATE INDEX idx_sampletaxa_taxon_id ON SampleTaxa(taxon_id);
CREATE INDEX idx_sampletaxa_sample_id_taxon_id ON SampleTaxa(sample_id, taxon_id);

-- Taxa
CREATE INDEX idx_taxonclassification_taxon_species ON TaxonClassification(taxon_species);
CREATE INDEX idx_taxonclassification_taxon_genus ON TaxonClassification(taxon_genus);
CREATE INDEX idx_taxonclassification_taxon_family ON TaxonClassification(taxon_family);
CREATE INDEX idx_taxonreference_reference_id ON TaxonReference(reference_id);

-- Traits
CREATE UNIQUE INDEX idx_traitsdomain_trait_domain_name ON TraitsDomain(trait_domain_name);
CREATE INDEX idx_traits_trait_domain_id ON Traits(trait_domain_id);
CREATE INDEX idx_traitsvalue_trait_id ON TraitsValue(trait_id);
CREATE INDEX idx_traitsvalue_sample_id ON TraitsValue(sample_id);
CREATE INDEX idx_traitsvalue_taxon_id ON TraitsValue(taxon_id);
CREATE INDEX idx_traitsvalue_trait_unit_conversion_id ON TraitsValue(trait_unit_conversion_id);
CREATE INDEX idx_traitsvalue_trait_derivation_id ON TraitsValue(trait_derivation_id);
CREATE INDEX idx_traitsreference_reference_id ON TraitsReference(reference_id);

-- Abiotic data
CREATE INDEX idx_abioticdata_abiotic_variable_id ON AbioticData(abiotic_variable_id);
CREATE INDEX idx_abioticvariablereference_reference_id ON AbioticVariableReference(reference_id);
