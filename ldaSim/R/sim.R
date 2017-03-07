#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## These are utilities for generating data from LDA, useful in the simulation.

#' Simulate from a Dirichlet Distribution
#'
#' @param n [int] The number of dirichlet samples to draw
#' @param alpha [numeric vector] The alpha location / concentration parameter
#'   for the Dirichlet.
#' @return x [matrix] A matrix whose rows are samples from a Dirichlet(alpha).
#' @export
rdirichlet <- function(n, alpha) {
  gammas <- sapply(alpha, function(x) rgamma(n, x))
  gammas <- as.matrix(gammas, nrow = n)
  gammas / rowSums(gammas)
}

#' Simulate parameters for LDA
#' @param D [int] The number of documents.
#' @param K [int] The number of topics.
#' @param V [int] The number of terms.
#' @param alpha0 [numeric vector] The location / concentration for the mixture
#'   variables theta_{i}.
#' @param gamma0 [numeric vector] The location / concentration for the topic
#'   variables beta_{k}.
#' @return x [list] A list with components $theta and $beta containing the
#'   simulated mixture components and topic matrices, respectively. For theta,
#'   the rows are individual documents, for beta, the rows are terms. Both
#'   matrices have K columns.
#' @export
generate_params <- function(D, K, V, alpha0, gamma0) {
  list(
    "theta" = rdirichlet(D, alpha0 * rep(1, K)),
    "beta" = t(rdirichlet(K, gamma0 * rep(1, V)))
  )
}

#' Simulate data for LDA
#'
#' Given the mixture and topic probabilities, simulate word counts across
#' documents.
#' @param N [int] The number of words per document. We assume this is fixed,
#'   though in general LDA doesn't require this.
#' @param theta [D x K matrix] A matrix of mixture components across documents.
#'   Rows correspond to documents, columns to topics.
#' @param beta [V x K matrix] A matrix of topic probabilities. Rows correspond
#'   to terms, columns to topics.
#' @return X [D x V matrix] A matrix of word counts across documents. Documents
#'   are rows, terms are columns.
#' @export
generate_data <- function(N, theta, beta) {
  X <- matrix(nrow = nrow(theta), ncol = nrow(beta))
  for (i in seq_len(nrow(theta))) {
    X[i, ] <- rmultinom(1, N, beta %*% theta[i, ])
  }
  X
}
