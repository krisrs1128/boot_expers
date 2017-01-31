#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Fit an LDA Model using STAN.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- args[[1]]
gen_id <- args[[2]]
data_path <- args[[3]]
fit_method <- args[[4]]
n_samples <- as.integer(args[[5]])
K <- as.integer(args[[6]])
alpha <- as.numeric(args[[7]])
gamma <- as.numeric(args[[8]])
save(list = ls(all.names = TRUE), file = "~/input.rda")
load("~/input.rda")

## ---- libraries ----
library("rstan")
library("feather")
library("plyr")
library("dplyr")
library("data.table")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## ---- get-data ----
n <- read_feather(data_path) %>%
    dcast(i ~ v) %>%
    select(-i) %>%
    as.matrix()

stan_data <- list(
    "K" = K,
    "V" = ncol(n),
    "D" = nrow(n),
    "n" = n,
    "alpha" = rep(alpha, K),
    "gamma" = rep(gamma, ncol(n))
)

## ---- fit-model ----
if (tolower(fit_method) == "vb") {
    fit <- vb(
      stan_model("../src/lda.stan"),
      data = stan_data,
      output_samples = n_samples
    )
} else if (tolower(fit_method) == "gibbs") {
    fit <- stan(
      "../src/lda.stan",
      data = stan_data,
      chains = 1,
      iter = 2 * n_samples ## half are warmup
    )
} else {
    stop("fit_method must be either 'gibbs' or 'vb'")
}

message(n_samples)
message(dim(extract(fit)$beta))

## ---- save ----
output_path <- file.path(
  output_dir,
  paste0(fit_method, "-", gen_id, ".RData")
)
save(fit, file = output_path)

## ---- update-metadata ----
metadata <- data.frame(
  "file" = output_path,
  "D" = nrow(n),
  "V" = ncol(n),
  "N" = NA,
  "K" = K,
  "alpha0" = NA,
  "gamma0" = NA,
  "alpha_fit" = alpha,
  "gamma_fit" = gamma,
  "n_replicates" = NA,
  "batch_id" = NA,
  "n_samples" = n_samples,
  "method" = fit_method
)

metadata_path <- file.path(output_dir, "metadata.csv")
write.table(
  metadata,
  file = metadata_path,
  append = TRUE,
  sep = ",",
  row.names = FALSE,
  col.names = !file.exists(metadata_path)
)
