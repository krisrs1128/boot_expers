#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## These are utilities for fitting LDA using STAN.

#' Fit an LDA model using STAN
#'
#' This is a wrapper for rstan::vb() and rstan::extract() which runs
#' and extracts summary information from an LDA model.
#'
#' @param stan_data [list] A list giving required values for the data field in
#'   the stan file.
#' @param stan_file [string] The path to the STAN file describing the
#'   LDA model.
#' @param keep_samples [bool] Whether to keep samples from the model
#'   posterior. Otherwise only returns (sampled) posterior mean.
#' @param use_vb [bool] Should we use variational bayes? If false,
#'   uses gibbs sampling.
#' @return result [list of data.table] A list of data.tables giving
#'   means for beta, theta, and (optionally) samples for beta and
#'   theta.
#' @importFrom reshape2 melt dcast
#' @importFrom rstan stan_model vb extract
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise rename
#' @importFrom data.table data.table
#' @export
fit_model <- function(stan_data, stan_file, keep_samples = FALSE,
                      use_vb = TRUE, ...) {
  ## fit model
  if (use_vb) {
    m <- stan_model(file = stan_file)
    stan_fit <- vb(m, stan_data, ...)
  } else {
    stan_fit <- stan(stan_file, data = stan_data, ...)
  }

  ## get samples
  samples <- extract(stan_fit)

  beta_hat <- samples$beta %>%
    melt() %>%
    group_by(Var2, Var3) %>%
    summarise(mean = mean(value)) %>%
    dcast(Var2 ~ Var3) %>%
    data.table() %>%
    rename(v = Var2, k = Var3, beta = value)

  theta_hat <- samples$theta %>%
    melt() %>%
    group_by(Var2, Var3) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    data.table() %>%
    rename(n = Var2, k = Var3, theta = value)

  result <- list(
    "theta_hat" = theta_hat,
    "beta_hat" = beta_hat
  )

  if (keep_samples) {
    result$samples <- samples
  }

  result
}
