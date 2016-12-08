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


#' Fit an LDA model using ADVI STAN
#'
#' This is a wrapper for rstan::vb() and rstan::extract() which runs
#' and extracts summary information from an LDA model.
#'
#' @param X {matrix} The data to fit an LDA model to.
#' @param stan_file {string} The path to the STAN file describing the
#'   LDA model.
#' @param keep_samples {bool} Whether to keep samples from the model
#'   posterior. Otherwise only returns (sampled) posterior mean.
#' @return result {list of data.table} A list of data.tables giving
#'   means for beta, theta, and (optionally) samples for beta and
#'   theta.
#' @importFrom reshape2 melt dcast
#' @importFrom rstan stan_model vb extract
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom data.table data.table
fit_model <- function(stan_data, stan_file, keep_samples = FALSE) {
  library("plyr")
  library("dplyr")
  library("reshape2")
  library("rstan")
  library("data.table")

  # fit model
  m <- stan_model(file = stan_file)
  stan_fit <- vb(m, stan_data)

  # get samples
  samples <- rstan::extract(stan_fit)

  beta_hat <- samples$beta %>%
    melt() %>%
    group_by(Var2, Var3) %>%
    summarise(mean = mean(value)) %>%
    dcast(Var2 ~ Var3) %>%
    data.table()

  theta_hat <- samples$theta %>%
    melt() %>%
    group_by(Var2, Var3) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    data.table()
  setnames(theta_hat, c("n", "k", "theta"))

  result <- list(
    "theta_hat" = theta_hat,
    "beta_hat" = beta_hat
  )

  if (keep_samples) {
    result$samples <- samples
  }

  result
}
