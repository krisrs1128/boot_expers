#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Simulate parameters according to an LDA model, using command line arguments
args <- commandArgs(trailingOnly=TRUE)
output_dir <- args[[1]]
output_id <- args[[2]]
D <- as.integer(args[[3]])
V <- as.integer(args[[4]])
K <- as.integer(args[[5]])
alpha0 <- as.numeric(args[[6]])
gamma0 <- as.numeric(args[[7]])

## ---- libraries ----
source("../src/lda.R")
library("feather")
library("reshape2")

## ---- simulate ----
params <- generate_params(D, V, K, alpha0, gamma0)
dir.create(output_dir)
write_feather(
  melt(
    params$beta,
    varnames = c("v", "k"),
    value.name = "beta"
  ),
  file.path(output_dir, paste0("beta-", output_id, ".feather"))
)

write_feather(
  melt(
    params$theta,
    varnames = c("i", "k"),
    value.name = "theta"
  ),
  file.path(output_dir, paste0("theta-", output_id, ".feather"))
)
