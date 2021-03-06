% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim.R
\name{generate_data}
\alias{generate_data}
\title{Simulate data for LDA}
\usage{
generate_data(N, theta, beta)
}
\arguments{
\item{N}{[int] The number of words per document. We assume this is fixed,
though in general LDA doesn't require this.}

\item{theta}{[D x K matrix] A matrix of mixture components across documents.
Rows correspond to documents, columns to topics.}

\item{beta}{[V x K matrix] A matrix of topic probabilities. Rows correspond
to terms, columns to topics.}
}
\value{
X [D x V matrix] A matrix of word counts across documents. Documents
  are rows, terms are columns.
}
\description{
Given the mixture and topic probabilities, simulate word counts across
documents.
}
