% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_syntnet.R
\name{create_syntnet}
\alias{create_syntnet}
\title{Create a Syntactic Network}
\usage{
create_syntnet(
  syntrel,
  basis = "lemma",
  keep_pos = c("NOUN", "PROPN", "ADJ", "VERB", "ADV"),
  multiplex = FALSE,
  min_count = 1
)
}
\arguments{
\item{syntrel}{Tibble. Output of the function create_syntrel.}

\item{basis}{Character string. Whether the network should be based on "word" or "lemma".}

\item{keep_pos}{Character vector. Parts of speech which should be kept to build the network.}

\item{multiplex}{Logical. Whether the kind of relationship should be kept.}

\item{min_count}{Numeric. Minimum number of occurrences for a relationship to be kept.}
}
\value{
A tidygraph of syntactic relationships between words of lemmas
}
\description{
Make an tidygraph from a list of syntactic relationships as returned by create_syntrel.
}
\seealso{
\code{\link{create_syntrel}}
}
\author{
Nicolas Mangin
}
