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

write_feather(
  data.table(N),
  file.path(output_dir, paste0("n-", output_id, ".feather"))
)
