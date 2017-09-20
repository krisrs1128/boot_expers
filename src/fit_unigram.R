#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
##
## Fit a dynamic unigram model using STAN.
##
## author: sankaran.kris@gmail.com
## date: 09/19/2017

###############################################################################
## arguments and libraries
###############################################################################
library("feather")
library("tidyverse")
library("rstan")

args <- commandArgs(trailingOnly = TRUE)
output_dir <- args[[1]]
fit_id <- args[[2]]
data_path <- args[[3]]
n_samples <- as.integer(args[[4]])
a0 <- as.numeric(args[[5]])
b0 <- as.numeric(args[[6]])

###############################################################################
## prepare model input
###############################################################################
X <- read_feather(data_path) %>%
  spread(v, x) %>%
  select(-i) %>%
  as.matrix()

stan_data <- list(
  "sigma0" = sigma0,
  "V" = ncol(X),
  "T" = nrow(X),
  "N" = nrow(X),
  "a0" = a0,
  "b0" = b0,
  "times" = seq_len(nrow(X)),
  "times_mapping" = seq_len(nrow(X)),
  "x" = X
)
