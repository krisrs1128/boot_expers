#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
##
## Simulate parameters according to the dynamic unigrams model, using command
## line arguments.
##
## author: sankaran.kris@gmail.com
## date: 09/19/2017

###############################################################################
## setup arguments + functions
###############################################################################
library("argparser")
library("reshape2")
library("feather")

parser <- arg_parser("Simulate parameters according to a dynamic unigram model")
parser <- add_argument(
  parser,
  "--output_dir"
  help = "Directory to save the output"
)
parser <- add_argument(
  parser,
  "--gen_id",
  help = "Unique identifier for the output object of this script"
)
parser <- add_argument(
  parser,
  "--D",
  help = "The total number of samples / timepoints in this simulation"
)
parser <- add_argument(
  parser,
  "--V",
  help = "The number of terms at each timepoint"
)
parser <- add_argument(
  parser,
  "--sigma0",
  help = "The random walk diffusion parameter"
)
argv <- parse_args(parser)

#' Simulate unigram parameters
unigram_params <- function(D, V, sigma0) {
  mu <- matrix(nrow = D, ncol = V)
  mu[1, ] <- rnorm(V, 0, sigma0)
  for (i in seq_len(D - 1)) {
    mu[i + 1, ] <- rnorm(V, mu[i, ], sigma0)
  }

  mu
}

###############################################################################
## simulate and write results to file
###############################################################################
mu <- unigram_params(argv$D, argv$V, argv$sigma0)
dir.create(output_dir)
mu_path <- file.path(output_dir, sprintf("mu-%s.feather", argv$gen_id))

write_feather(
  melt(
    mu,
    varnames = c("i", "v"),
    value.name = "mu"
  ),
  mu_path
)

###############################################################################
## update the metaata
###############################################################################
metadata <- data.frame(
  "file" = mu_path,
  "D" = argv$D,
  "V" = argv$V,
  "N" = NA,
  "sigma0" = argv$sigma0,
  "sigma_fit" = NA,
  "n_samples" = NA,
  "method" = NA,
  "iteration" = NA
)

write.table(
  metadata,
  file = file.path(output_dir, "metadata.csv"),
  append = TRUE,
  sep = ",",
  row.names = FALSE,
  colnames = !file.exists(file.path(output_dir, "metadata.csv"))
)
