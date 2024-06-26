% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_taxa_by_id.R
\name{select_taxa_by_id}
\alias{select_taxa_by_id}
\title{Only select taxa by ID}
\usage{
select_taxa_by_id(con = NULL, sel_id = NULL)
}
\arguments{
\item{con}{A connection to the Vault database}

\item{sel_id}{A integer vector of taxa ids}
}
\value{
A vault_pipe object
}
\description{
Filter the Samples in dataset by taxon id
}
