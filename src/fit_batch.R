#! /usr/bin/env Rscript

# File description -------------------------------------------------------------

## ---- parse-args ----
args <- commandArgs(trailingOnly=TRUE)
start_ix <- as.integer(args[[1]])
end_ix <- as.integer(args[[2]])
src_dir <- args[[3]]
N <- as.integer(args[[4]])
alpha0 <- as.integer(args[[5]])
fitted_theta <- args[[6]]
fitted_beta <- args[[7]]
output_dir <- args[[8]]

## ---- setup-src ----
library("rstan")
library("feather")
library("reshape2")
library("plyr")
library("dplyr")
library("data.table")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

for (f in list.files(src_dir, ".R$", full.names = TRUE)) {
  if (basename(f) != "fit_batch.R") {
    source(f)
  }
}

stan_file <- list.files(src_dir, ".stan$", full.names = TRUE)

## ---- run-parametric-bootstrap ----
for (i in seq(start_ix, end_ix)) {
  theta <- read_feather(fitted_theta) %>%
    dcast(n ~ k) %>%
    select(-n) %>%
    as.matrix()

  beta <- read_feather(fitted_beta) %>%
    select(-Var2) %>%
    as.matrix()

  X <- generate_data(N, theta, beta)
  stan_data <- list(
    n = X,
    K = nrow(beta),
    V = ncol(beta),
    D = nrow(theta),
    alpha = alpha0 * rep(1, nrow(beta))
  )

  fit <- fit_model(stan_data, stan_file, output_samples = 4000)
  write_feather(fit$beta_hat, output_path(output_dir)(paste0("beta_boot_", i, ".feather")))
  write_feather(fit$theta_hat, output_path(output_dir)(paste0("theta_boot_", i, ".feather")))
}
