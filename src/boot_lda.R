#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Simulate parametric bootstrap samples from a fitted LDA model

args <- commandArgs(trailingOnly = TRUE)
output_dir <- args[[1]]
batch_id <- args[[2]]
fit_id <- args[[3]]
input_path <- args[[4]]
N <- as.integer(args[[5]])
alpha <- as.numeric(args[[6]])
gamma <- as.numeric(args[[7]])
n_replicates <- as.integer(args[[8]])
n_samples <- as.integer(args[[9]])

## ---- libraries ----
library("rstan")
library("feather")
library("plyr")
library("dplyr")
library("data.table")
source("../src/lda.R")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
dir.create(output_dir)

## ---- utils ----
posterior_mean <- function(samples, dims) {
    samples %>%
        melt(varnames = c("iteration", rev(dims))) %>%
        group_by_(.dots = dims) %>%
        summarise(value = mean(value)) %>%
        dcast(formula(paste(dims, collapse = "~"))) %>%
        select_(paste0("-", dims[1])) %>%
        as.matrix()
}

## --- get-fit ---
fit <- get(load(input_path))
samples <- extract(fit)

beta_hat <- posterior_mean(samples$beta, c("v", "k"))
theta_hat <- posterior_mean(aperm(samples$theta, c(1, 3, 2)), c("i", "k"))

## ---- bootstrap-replicaates ----
for (i in seq_len(n_replicates)) {
    cur_data <- generate_data(N, theta_hat, beta_hat)
    stan_data <- list(
       "n" = cur_data,
       "K" = ncol(theta_hat),
       "V" = nrow(beta_hat),
       "D" = nrow(theta_hat),
       "alpha" = rep(alpha, ncol(theta_hat)),
       "gamma" = rep(gamma, nrow(beta_hat))
    )

    vb_fit <- vb(
        stan_model("../src/lda.stan"),
        stan_data,
        iter = n_samples
    ) %>%
        extract()

    beta_boot <- posterior_mean(vb_fit$beta, c("v", "k")) %>%
        melt(varnames = c("v", "k"), value.name = "beta")
    theta_boot <- posterior_mean(vb_fit$theta, c("i", "k")) %>%
        melt(varnames = c("i", "k"), value.name = "theta")

    write_feather(beta_boot, file.path(output_dir, paste0("beta-boot-", fit_id, batch_id, i, ".feather")))
    write_feather(theta_boot, file.path(output_dir, paste0("theta-boot-", fit_id, batch_id, i, ".feather")))
}
