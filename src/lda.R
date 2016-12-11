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
  X <- matrix(nrow = nrow(theta), ncol = ncol(beta))
  for (i in seq_len(nrow(theta))) {
    X[i, ] <- rmultinom(1, N, t(beta) %*% theta[i, ])
  }
  X
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
#' @param use_vb {bool} Should we use variational bayes? If false,
#'   uses gibbs sampling.
#' @return result {list of data.table} A list of data.tables giving
#'   means for beta, theta, and (optionally) samples for beta and
#'   theta.
#' @importFrom reshape2 melt dcast
#' @importFrom rstan stan_model vb extract
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise
#' @importFrom data.table data.table
fit_model <- function(stan_data, stan_file, keep_samples = FALSE,
                      use_vb = TRUE) {
  # fit model
  if (use_vb) {
    m <- stan_model(file = stan_file)
    stan_fit <- vb(m, stan_data)
  } else {
    stan_fit <- stan(stan_file, data = stan_data)
  }

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
