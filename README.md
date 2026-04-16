

<!-- README.md is generated from README.qmd. Please edit that file -->

# vaultkeepr <img src="man/figures/vaultkeeper_logo.png" align="right" width="200" />

<!-- badges: start -->

[![CRAN-status](https://www.r-pkg.org/badges/version/vaultkeepr.png)](https://CRAN.R-project.org/package=vaultkeepr)
[![R-CMD-check](https://github.com/OndrejMottl/vaultkeepr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OndrejMottl/vaultkeepr/actions/workflows/R-CMD-check.yaml)
[![Codecov-test-coverage](https://codecov.io/gh/OndrejMottl/vaultkeepr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/OndrejMottl/vaultkeepr?branch=main)
<!-- badges: end -->

The goal of vaultkeepr is to providing the interface to access the
[**VegVault** database](https://ondrejmottl.github.io/VegVault/).

## Installation

You can install the development version of vaultkeepr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("OndrejMottl/vaultkeepr")
```

## Overview

vaultkeepr provides a pipe-friendly interface to the
[VegVault](https://ondrejmottl.github.io/VegVault/) database. Functions
are designed to be chained together to build a lazy query plan that is
executed in full with a single call to `extract_data()`.

### Connection & exploration

| Function          | Description                                    |
|-------------------|------------------------------------------------|
| `open_vault()`    | Open a connection to a VegVault SQLite file    |
| `explore_vault()` | List all table names in the connected database |

### Building a query plan

| Function | Description |
|----|----|
| `get_datasets()` | Add dataset metadata to the plan |
| `select_dataset_by_type()` | Filter by dataset type |
| `select_dataset_by_geo()` | Filter by geographic bounding box |
| `get_samples()` | Add sample metadata |
| `select_samples_by_age()` | Filter samples by age range |
| `get_age_uncertainty()` | Retrieve age uncertainty estimates for samples |
| `get_taxa()` | Add taxon occurrence data |
| `select_taxa_by_name()` | Filter taxa by name |
| `select_taxa_by_id()` | Filter taxa by ID |
| `get_taxon_names()` | Retrieve distinct taxon names from the current plan |
| `get_classification_table()` | Retrieve the taxonomic classification table |
| `get_traits()` | Add functional trait data |
| `select_traits_by_domain_name()` | Filter traits by domain |
| `get_abiotic_data()` | Add spatially matched abiotic (climate/soil) data |
| `select_abiotic_var_by_name()` | Filter abiotic variables by name |
| `select_abiotic_var_by_id()` | Filter abiotic variables by ID |

### Executing the plan

| Function           | Description                                |
|--------------------|--------------------------------------------|
| `extract_data()`   | Execute the query plan and return a tibble |
| `get_references()` | Retrieve data-source references            |

## Usage

``` r
library(vaultkeepr)

# Open the database
con <-
  open_vault(path = "path/to/VegVault.sqlite")

# Inspect available tables
explore_vault(con = con)

# Build and execute a query plan
data_betula <-
  con |>
  get_datasets() |>
  select_dataset_by_type(
    sel_dataset_type = c("vegetation_plot", "fossil_pollen_archive")
  ) |>
  get_samples() |>
  get_taxa() |>
  select_taxa_by_name(sel_taxa = "Betula") |>
  extract_data()
```

For detailed worked examples, see the [package
website](https://hope-uib-bio.github.io/vaultkeepr/articles/).
