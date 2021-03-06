% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getGeneRegulation.R
\name{getGeneRegulation}
\alias{getGeneRegulation}
\title{getGeneRegulation}
\usage{
getGeneRegulation(database, genes, format)
}
\arguments{
\item{database}{RSQLite object imported previously to R}

\item{genes}{genes of interest}

\item{format}{onerow, multirow, table}
}
\description{
This function gives the regulators of input genes and whether their effect is possitive or negative
}
\examples{
regulon <- importDB("~/Documents/regulondb_sqlite3.db")
getRegulatedGenesByTF(regulon, "Ara")
 dbDisconnect(regulon)
}
