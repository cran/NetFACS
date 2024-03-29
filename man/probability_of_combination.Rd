% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netfacs-helpers.R
\name{probability_of_combination}
\alias{probability_of_combination}
\title{Calculate probabilities of single elements and combinations occurring}
\usage{
probability_of_combination(elements, maxlen, sep = "_")
}
\arguments{
\item{elements}{A vector with all elements observed together at an event. Or
a list of vectors or a binary matrix with elements as \code{colnames()}}

\item{maxlen}{maximum size of combinations to be considered}

\item{sep}{String. Separator used for showing combinations of elements}
}
\value{
Function returns a dataframe with observed probabilities for each
  combination in the dataset
}
\description{
Calculate probabilities of single elements and combinations occurring
}
