% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_level.R
\name{add_level}
\alias{add_level}
\title{Add taxonomic level}
\usage{
add_level(physeq, level, level_name, look_up_table)
}
\arguments{
\item{physeq}{a phyloseq object.}

\item{level}{the level to target/bin into a new overall level.}

\item{level_name}{name of the new overall level.}

\item{look_up_table}{a look up table to categories the target level. Example: kingdom_to_domain <- c("Bacteria" = "Prokaryotes", "SAR" = "Eukaryotes")}
}
\value{
an updated phyloseq object with the user-specified taxonomic level.
}
\description{
Add user-specified taxonomic level, e.g., domain.
}
