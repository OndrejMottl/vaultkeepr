-- VegVault 1.0.0 database schema
--
-- Copied without changes from Data/SQL/make_tables.sql at tag
-- v1.0.0-zenodo (commit 72b518b) of https://github.com/OndrejMottl/VegVault.
-- The test database (tests/testthat/helper_make_database.R) and the
-- vignette database (vignettes/helper_make_example_db.R) both create their
-- tables and indexes from this file, statement by statement. Statements end
-- with a semicolon; comments are lines that start with two dashes.

CREATE TABLE "version_control"
(
  "id" INTEGER PRIMARY KEY,
  "version" TEXT,
  "update_date" TEXT DEFAULT CURRENT_DATE,
  "changelog" TEXT
);
CREATE UNIQUE INDEX idx_version_control_id ON version_control(id);

CREATE TABLE "Authors"
(
  "author_id" INTEGER PRIMARY KEY,
  "author_fullname" TEXT,
  "author_email" TEXT,
  "author_orcid" TEXT
);
CREATE UNIQUE INDEX idx_authors_author_id ON Authors(author_id);

CREATE TABLE "Datasets"
(
  "dataset_id" INTEGER PRIMARY KEY,
  "dataset_name" TEXT,
  "data_source_id" INTEGER,
  "dataset_type_id" INTEGER,
  "data_source_type_id" INTEGER,
  "coord_long" REAL,
  "coord_lat" REAL,
  "sampling_method_id" INTEGER,
  FOREIGN KEY ("dataset_type_id") REFERENCES "DatasetTypeID" ("dataset_type_id"),
  FOREIGN KEY ("sampling_method_id") REFERENCES "SamplingMethodID" ("sampling_method_id"),
  FOREIGN KEY ("data_source_id") REFERENCES "DatasetSourcesID" ("data_source_id"),
  FOREIGN KEY ("data_source_type_id") REFERENCES "DatasetSourceTypeID" ("data_source_type_id")
);
CREATE UNIQUE INDEX idx_datasets_dataset_id ON Datasets(dataset_id);
CREATE INDEX idx_datasets_dataset_type_id ON Datasets(dataset_type_id);
CREATE INDEX idx_datasets_data_source_id ON Datasets(data_source_id);
CREATE INDEX idx_datasets_data_source_type_id ON Datasets(data_source_type_id);
CREATE INDEX idx_datasets_coord_long_lat ON Datasets(coord_long, coord_lat);
CREATE INDEX idx_datasets_sampling_method_id ON Datasets(sampling_method_id);

CREATE TABLE "DatasetReferences"
(
  "dataset_id" INTEGER,
  "reference_id" INTEGER,
  FOREIGN KEY ("dataset_id") REFERENCES "Datasets" ("dataset_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
);
CREATE INDEX idx_datasetreferences_dataset_id ON DatasetReferences(dataset_id);
CREATE INDEX idx_datasetreferences_reference_id ON DatasetReferences(reference_id);

CREATE TABLE "DatasetTypeID"
(
  "dataset_type_id" INTEGER PRIMARY KEY,
  "dataset_type" TEXT
);
CREATE UNIQUE INDEX idx_datasettypeid_dataset_type_id ON DatasetTypeID(dataset_type_id);

CREATE TABLE "DatasetSourceTypeID"
(
  "data_source_type_id" INTEGER PRIMARY KEY,
  "dataset_source_type" TEXT
);
CREATE UNIQUE INDEX idx_datasetsourcetypeid_data_source_type_id ON DatasetSourceTypeID(data_source_type_id);

CREATE TABLE "DatasetSourceTypeReference"
(
  "data_source_type_id" INTEGER,
  "reference_id" INTEGER,
  FOREIGN KEY ("data_source_type_id") REFERENCES "DatasetSourceTypeID" ("data_source_type_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
);
CREATE INDEX idx_datasetsourcetypereference_data_source_type_id ON DatasetSourceTypeReference(data_source_type_id);
CREATE INDEX idx_datasetsourcetypereference_reference_id ON DatasetSourceTypeReference(reference_id);

CREATE TABLE "DatasetSourcesID"
(
  "data_source_id" INTEGER PRIMARY KEY,
  "data_source_desc" TEXT
);
CREATE UNIQUE INDEX idx_datasetsourcesid_data_source_id ON DatasetSourcesID(data_source_id);

CREATE TABLE "DatasetSourcesReference"
(
  "data_source_id" INTEGER,
  "reference_id" INTEGER,
  FOREIGN KEY ("data_source_id") REFERENCES "DatasetSourcesID" ("data_source_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
);
CREATE INDEX idx_datasetsourcesreference_data_source_id ON DatasetSourcesReference(data_source_id);
CREATE INDEX idx_datasetsourcesreference_reference_id ON DatasetSourcesReference(reference_id);

CREATE TABLE "SamplingMethodID"
(
  "sampling_method_id" INTEGER PRIMARY KEY,
  "sampling_method_details" TEXT
);
CREATE UNIQUE INDEX idx_samplingmethodid_sampling_method_id ON SamplingMethodID(sampling_method_id);

CREATE TABLE "SamplingMethodReference"
(
  "sampling_method_id" INTEGER,
  "reference_id" INTEGER,
  FOREIGN KEY ("sampling_method_id") REFERENCES "SamplingMethodID" ("sampling_method_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
);
CREATE INDEX idx_samplingmethodreference_sampling_method_id ON SamplingMethodReference(sampling_method_id);
CREATE INDEX idx_samplingmethodreference_reference_id ON SamplingMethodReference(reference_id);

CREATE TABLE "Samples"
(
  "sample_id" INTEGER PRIMARY KEY,
  "sample_name" TEXT,
  "sample_details" TEXT,
  "age" REAL,
  "sample_size_id" INTEGER,
  FOREIGN KEY ("sample_size_id") REFERENCES "SampleSizeID" ("sample_size_id")
);
CREATE UNIQUE INDEX idx_samples_sample_id ON Samples(sample_id);
CREATE INDEX idx_samples_sample_size_id ON Samples(sample_size_id);

CREATE TABLE "SampleReference"
(
  "sample_id" INTEGER,
  "reference_id" INTEGER,
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
);
CREATE INDEX idx_samplereference_sample_id ON SampleReference(sample_id);
CREATE INDEX idx_samplereference_reference_id ON SampleReference(reference_id);

CREATE TABLE "DatasetSample"
(
  "dataset_id" INTEGER,
  "sample_id" INTEGER,
  FOREIGN KEY ("dataset_id") REFERENCES "Datasets" ("dataset_id"),
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id")
);
CREATE INDEX idx_datasetsample_dataset_id ON DatasetSample(dataset_id);
CREATE INDEX idx_datasetsample_sample_id ON DatasetSample(sample_id);

CREATE TABLE "SampleUncertainty"
(
  "sample_id" INTEGER,
  "iteration" INTEGER,
  "age" INTEGER,
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id")
);
CREATE INDEX idx_sampleuncertainty_sample_id ON SampleUncertainty(sample_id);

CREATE TABLE "SampleSizeID"
(
  "sample_size_id" INTEGER PRIMARY KEY,
  "sample_size" REAL,
  "description" TEXT
);
CREATE UNIQUE INDEX idx_samplesizeid_sample_size_id ON SampleSizeID(sample_size_id);

CREATE TABLE "SampleTaxa"
(
  "sample_id" INTEGER,
  "taxon_id" INTEGER,
  "value" REAL,
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
  FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id")
);
CREATE INDEX idx_sampletaxa_sample_id ON SampleTaxa(sample_id);
CREATE INDEX idx_sampletaxa_taxon_id ON SampleTaxa(taxon_id);
CREATE INDEX idx_sampletaxa_sample_id_taxon_id ON SampleTaxa(sample_id, taxon_id);

CREATE TABLE "Taxa"
(
  "taxon_id" INTEGER PRIMARY KEY,
  "taxon_name" TEXT
);
CREATE UNIQUE INDEX idx_taxa_taxon_id ON Taxa(taxon_id);

CREATE TABLE "TaxonClassification"
(
  "taxon_id" INTEGER,
  "taxon_species" INTEGER,
  "taxon_genus" INTEGER,
  "taxon_family" INTEGER,
  FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id"),
  FOREIGN KEY ("taxon_species") REFERENCES "Taxa" ("taxon_id"),
  FOREIGN KEY ("taxon_genus") REFERENCES "Taxa" ("taxon_id"),
  FOREIGN KEY ("taxon_family") REFERENCES "Taxa" ("taxon_id")
);
CREATE INDEX idx_taxonclassification_taxon_id ON TaxonClassification(taxon_id);
CREATE INDEX idx_taxonclassification_taxon_species ON TaxonClassification(taxon_species);
CREATE INDEX idx_taxonclassification_taxon_genus ON TaxonClassification(taxon_genus);
CREATE INDEX idx_taxonclassification_taxon_family ON TaxonClassification(taxon_family);

CREATE TABLE "TaxonReference"
(
  "taxon_id" INTEGER,
  "reference_id" INTEGER,
  FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
);
CREATE INDEX idx_taxonreference_taxon_id ON TaxonReference(taxon_id);
CREATE INDEX idx_taxonreference_reference_id ON TaxonReference(reference_id);

CREATE TABLE "TraitsDomain"
(
  "trait_domain_id" INTEGER PRIMARY KEY,
  "trait_domain_name" TEXT,
  "trait_domanin_description" TEXT
);
CREATE UNIQUE INDEX idx_traitsdomain_trait_domain_id ON TraitsDomain(trait_domain_id);

CREATE TABLE "Traits"
(
  "trait_id" INTEGER PRIMARY KEY,
  "trait_domain_id" INTEGER,
  "trait_name" TEXT,
  FOREIGN KEY ("trait_domain_id") REFERENCES "TraitsDomain" ("trait_domain_id")
);
CREATE UNIQUE INDEX idx_traits_trait_id ON Traits(trait_id);
CREATE INDEX idx_traits_trait_domain_id ON Traits(trait_domain_id);

CREATE TABLE "TraitsValue"
(
  "trait_id" INTEGER,
  "dataset_id" INTEGER,
  "sample_id" INTEGER,
  "taxon_id" INTEGER,
  "trait_value" REAL,
  FOREIGN KEY ("trait_id") REFERENCES "Traits" ("trait_id"),
  FOREIGN KEY ("dataset_id") REFERENCES "Datasets" ("dataset_id"),
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
  FOREIGN KEY ("taxon_id") REFERENCES "Taxa" ("taxon_id")
);
CREATE INDEX idx_traitsvalue_trait_id ON TraitsValue(trait_id);
CREATE INDEX idx_traitsvalue_dataset_id ON TraitsValue(dataset_id);
CREATE INDEX idx_traitsvalue_sample_id ON TraitsValue(sample_id);
CREATE INDEX idx_traitsvalue_taxon_id ON TraitsValue(taxon_id);
CREATE INDEX idx_traitsvalue_dataset_id_sample_id_taxon_id ON TraitsValue(dataset_id, sample_id, taxon_id);

CREATE TABLE "TraitsReference"
(
  "trait_id" INTEGER,
  "reference_id" INTEGER,
  FOREIGN KEY ("trait_id") REFERENCES "Traits" ("trait_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
);
CREATE INDEX idx_traitsreference_trait_id ON TraitsReference(trait_id);
CREATE INDEX idx_traitsreference_reference_id ON TraitsReference(reference_id);

CREATE TABLE "AbioticData"
(
  "sample_id" INTEGER,
  "abiotic_variable_id" INTEGER,
  "abiotic_value" REAL,
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
  FOREIGN KEY ("abiotic_variable_id") REFERENCES "AbioticVariable" ("abiotic_variable_id")
);
CREATE INDEX idx_abioticdata_sample_id ON AbioticData(sample_id);
CREATE INDEX idx_abioticdata_abiotic_variable_id ON AbioticData(abiotic_variable_id);

CREATE TABLE "AbioticDataReference"
(
  "sample_id" INTEGER,
  "sample_ref_id" INTEGER,
  "distance_in_km" INTEGER,
  "distance_in_years" INTEGER,
  FOREIGN KEY ("sample_id") REFERENCES "Samples" ("sample_id"),
  FOREIGN KEY ("sample_ref_id") REFERENCES "Samples" ("sample_id")
);
CREATE INDEX idx_abioticdatareference_sample_id ON AbioticDataReference(sample_id);
CREATE INDEX idx_abioticdatareference_sample_ref_id ON AbioticDataReference(sample_ref_id);

CREATE TABLE "AbioticVariable"
(
  "abiotic_variable_id" INTEGER PRIMARY KEY,
  "abiotic_variable_name" TEXT,
  "abiotic_variable_unit" TEXT,
  "measure_details" TEXT
);
CREATE UNIQUE INDEX idx_abioticvariable_abiotic_variable_id ON AbioticVariable(abiotic_variable_id);

CREATE TABLE "AbioticVariableReference"
(
  "abiotic_variable_id" INTEGER,
  "reference_id" INTEGER,
  FOREIGN KEY ("abiotic_variable_id") REFERENCES "AbioticVariable" ("abiotic_variable_id"),
  FOREIGN KEY ("reference_id") REFERENCES "References" ("reference_id")
);
CREATE INDEX idx_abioticvariablereference_abiotic_variable_id ON AbioticVariableReference(abiotic_variable_id);
CREATE INDEX idx_abioticvariablereference_reference_id ON AbioticVariableReference(reference_id);

CREATE TABLE "References"
(
  "reference_id" INTEGER PRIMARY KEY,
  "reference_detail" TEXT,
  "mandatory" BOOLEAN NOT NULL DEFAULT FALSE
);
CREATE UNIQUE INDEX idx_references_reference_id ON "References"(reference_id);
