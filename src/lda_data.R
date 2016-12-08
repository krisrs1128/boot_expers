#! /usr/bin/env Rscript

# File description -------------------------------------------------------------
# These are utilities for generating data from LDA

rdirichlet <- function(n, alpha) {
  gammas <- sapply(alpha, function(x) rgamma(n, x))
  gammas <- as.matrix(gammas, nrow = n)
  gammas / rowSums(gammas)
}

generate_params <- function(D, K, V, alpha0, gamma0) {
  list(
    "theta" = rdirichlet(D, alpha0 * rep(1, K)),
    "beta" = rdirichlet(K, gamma0 * rep(1, V))
  )
}

generate_data <- function(N, theta, beta) {
  matrix(
    rmultinom(1, N, as.numeric(theta %*% beta)),
    nrow = nrow(theta)
  )
}
