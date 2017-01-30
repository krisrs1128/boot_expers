#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Fit an LDA Model using STAN.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- args[[1]]
gen_id <- args[[2]]
data_path <- args[[3]]
fit_method <- args[[4]]
n_samples <- as.integer(args[[5]])
keep_fit <- as.logical(args[[6]])
K <- args[[7]]
alpha <- args[[8]]
gamma <- args[[9]]

## ---- libraries ----
library("rstan")
library("feather")
library("plyr")
library("dplyr")
library("data.table")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## ---- get-data ----
N <- read_feather(data_path) %>%
    dcast(i ~ v) %>%
    select(-i) %>%
    as.matrix()

stan_data <- list(
    "K" = K,
    "V" = ncol(N),
    "D" = nrow(N),
    "N" = N,
    "alpha" = rep(alpha, K),
    "gamma" = rep(gamma, ncol(N))
)

## ---- fit-model ----
if (tolower(fit_method) == "vb") {
    fit <- vb(
        stan_model("../src/lda.stan"),
        data = stan_data,
        iterations = n_samples
    )
} else if (tolower(fit_method) == "gibbs") {
    fit <- stan(
        "../src/lda.stan",
        data = stan_data,
        chains = 1,
        iter = n_samples
    )
} else {
    stop("fit_method must be either 'gibbs' or 'vb'")
}

## ---- save ----
output_path <- file.path(output_dir, paste0(fit_method, "-", gen_id))
if (keep_fit) {
    save(fit, file.path = paste0(output_path, ".RData"))
} else {
    samples <- extract(fit)
    samples$beta %>%
        melt(varnames = c("iterations", "v", "k")) %>%
        write_feather(paste0(output_path, "-beta_hat.feather"))
    samples$theta %>%
        melt(varnames = c("iterations", "i", "k")) %>%
        write_feather(paste0(output_path, "-theta_hat.feather"))
}
