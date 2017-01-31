#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Simulate parameters according to an LDA model, using command line arguments

args <- commandArgs(trailingOnly=TRUE)
output_dir <- args[[1]]
output_id <- args[[2]]
N <- as.integer(args[[3]])
beta_path <- args[[4]]
theta_path <- args[[5]]

## ---- libraries ----
source("../src/lda.R")
library("feather")
library("plyr")
library("dplyr")
library("data.table")

## ---- simulate ----
beta <- read_feather(beta_path) %>%
  dcast(v ~ k) %>%
  select(-v) %>%
  as.matrix()

theta <- read_feather(theta_path) %>%
  dcast(i ~ k) %>%
  select(-i) %>%
  as.matrix()

N <- generate_data(N, beta, theta) %>%
  melt(varnames = c("i", "v"), value.name = "n")

output_path <- file.path(output_dir, paste0("n-", output_id, ".feather"))
write_feather(
  data.table(N),
  output_path
)

## ---- update-metadata ----
metadata <- data.frame(
  "file" = output_path,
  "D" = nrow(N),
  "V" = ncol(N),
  "N" = N,
  "K" = ncol(beta),
  "alpha0" = NA,
  "gamma0" = NA,
  "alpha_fit" = NA,
  "gamma_fit" = NA,
  "n_replicates" = NA,
  "batch_id" = NA,
  "n_samples" = NA
)

write.table(
  metadata,
  file = file.path(output_dir, "metadata.csv"),
  append = TRUE,
  sep = ",",
  row.names = FALSE,
  col.names = !file.exists(file.path(output_dir, "metadata.csv"))
)
