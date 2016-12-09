#! /usr/bin/env Rscript

# File description -------------------------------------------------------------
# This is the master script for one bootstrapping experiment. This work is very
# preliminary.
#
# author: kriss1@stanford.edu

## ---- setup ----
library("rstan")
library("plyr")
library("dplyr")
library("jsonlite")
library("data.table")
library("feather")
for (f in list.files("src", ".R", full.names = TRUE)) {
  if (basename(f) == "fit_batch.R") next
  source(f)
}
exper <- fromJSON("exper.json")

## ---- setup-directories ----
paths <- exper$paths
path_output <- output_path(file.path(paths$base, paths$output_dir))
path_params <- output_path(file.path(paths$base, paths$params_dir))
path_data <- output_path(file.path(paths$base, paths$data_dir))

dir.create(paths$base)
dir.create(file.path(paths$base, paths$params_dir))
dir.create(file.path(paths$base, paths$data_dir))
dir.create(file.path(paths$base, paths$output_dir))
dir.create(file.path(paths$base, paths$tmp_dir))

## ---- generate-data ----
model <- exper$model
params <- generate_params(model$D, model$K, model$V, model$alpha0, model$gamma0)
X <- generate_data(model$N, params$theta, params$beta)
write_feather(data.table(params$theta), path_params("theta.feather"))
write_feather(data.table(params$beta), path_params("beta.feather"))
write_feather(data.table(X), path_data("X.feather"))

## ---- fit-top-model ----
stan_data <- list(
  n = X,
  K = model$K,
  V = model$V,
  D = model$D,
  alpha = model$alpha0 * rep(1, model$K)
)

fit <- fit_model(stan_data, "src/lda.stan", keep_samples = TRUE)
write_feather(fit$beta_hat, path_output("beta_hat_master.feather"))
write_feather(fit$theta_hat, path_output("theta_hat_master.feather"))
write_feather(data.table(melt(fit$samples$beta)), path_output("beta_master_samples.feather"))
write_feather(data.table(melt(fit$samples$theta)), path_output("theta_master_samples.feather"))

## ---- send-replicates ----
parallel <- exper$parallel
batches <- rep(seq_len(parallel$batches), length.out = parallel$replicates) %>%
  sort()

run_opts <- c(
  file.path(getwd(), "src"),
  model$N,
  model$alpha0,
  path_output("theta_hat_master.feather"),
  path_output("beta_hat_master.feather"),
  file.path(paths$base, paths$output_dir)
)

for (i in seq_len(parallel$batches)) {
  jobname <- paste0(parallel$basename, i)
  cur_opts <- c(
    min(which(batches == i)),
    max(which(batches == i)),
    run_opts
  )

  rscript_cmd <- paste0(
    c("Rscript", file.path(getwd(), "/src/fit_batch.R"), cur_opts),
    collapse = " "
  )

  create_job(
    file.path(paths$base, paths$tmp_dir, jobname),
    jobname,
    rscript_cmd,
    list(
      partitions=parallel$partitions,
      mem_alloc = parallel$mem,
      time_alloc = parallel$time_alloc
    )
  )

  system(paste0("sbatch ", file.path(paths$base, paths$tmp_dir, jobname)))
}
