#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
##
## Simulate data according to the dynamic unigrams model, given simulated
## parameters $\mu_{t}$.
##
## author: sankaran.kris@gmail.com
## date: 09/19/2017

###############################################################################
## setup arguments + functions
###############################################################################
library("feather")
library("tidyverse")
library("reshape2")

args <- commandArgs(trailingOnly = TRUE)
argv <- list()
argv$output_dir <- args[[1]]
argv$gen_id <- args[[2]]
argv$N <- as.numeric(args[[3]])
argv$mu_path <- args[[4]]

softmax <- function(x) {
  exp(x) / log(sum(exp(x)))
}

#' Simulate from a unigram model
unigram_data <- function(N, mu) {
  mu_mat <- mu %>%
    spread(v, mu) %>%
    select(-i)
  X <- matrix(nrow = nrow(mu_mat), ncol = ncol(mu_mat))

  for (i in seq_len(nrow(mu_mat))) {
    X[i, ] <- rmultinom(1, N, prob = softmax(mu_mat[i, ]))[, 1]
  }

  X
}

###############################################################################
## Simulate and write results to file
###############################################################################
mu <- read_feather(argv$mu_path)
X <- unigram_data(argv$N, mu)

dir.create(argv$output_dir)
x_path <- file.path(argv$output_dir, sprintf("x-%s.feather", argv$gen_id))

write_feather(
  melt(
    X,
    varnames = c("i", "v"),
    value.name = "x"
  ),
  x_path
)
