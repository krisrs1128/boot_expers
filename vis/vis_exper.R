#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Script for visualizing output from LDA simulation and evaluation pipeline.
## Three main types of views: Boxplots of proportions estimates, across
## configurations, scatterplots of pairs of proportions estimates, and
## histograms of errors.
##
## author: kriss1@stanford.edu

## ---- libraries ----
library("feather")
library("data.table")
library("plyr")
library("dplyr")
library("tidyr")
library("rstan")
library("ggplot2")
library("ggscaffold")
source("./vis_utils.R")

## ---- paths ----
output_path <- "/scratch/users/kriss1/output/boot_expers"
metadata <- fread(file.path(output_path, "metadata.csv")) %>%
  unique()

## ---- get-truth ----
truth_paths <- metadata %>%
  filter(
    is.na(method),
    grepl("beta", file)
  ) %>%
  select(file) %>%
  unlist()

truth_data <- feather_from_paths(truth_paths) %>%
  mutate(k = paste0("truth_", k)) %>%
  dcast(file + v ~ k) %>%
  left_join(metadata) %>%
  select(v, starts_with("truth"), D, V, K, alpha0, gamma0)

## ---- posterior-samples ----
samples_paths <- metadata %>%
  filter(
    method %in% c("vb", "gibbs")
  ) %>%
  select(file) %>%
  unlist()

samples <- rdata_from_paths(samples_paths, "beta", c("iteration", "k", "v")) %>%
  mutate(k = paste0("value_", k)) %>%
  dcast(file + iteration + v ~ k) %>%
  left_join(metadata, by = "file") %>%
  mutate(iteration = pmin(iteration.x, iteration.y, na.rm = TRUE)) %>%
  select(-file, -iteration.x, -iteration.y, -starts_with("alpha"), -starts_with("gamma"))

## ---- bootstrap-samples ----
bootstrap_paths <- metadata %>%
  filter(
    method == "bootstrap",
    grepl("beta", file)
  ) %>%
  select(file) %>%
  unlist()

bootstraps <- feather_from_paths(bootstrap_paths) %>%
  mutate(k = paste0("value_", k)) %>%
  dcast(file + v ~ k) %>%
  left_join(metadata) %>%
  select(-file, -starts_with("alpha"), -starts_with("gamma"))

## ---- combined-data ----
combined <- samples %>%
  full_join(bootstraps) %>%
  left_join(truth_data)

## ---- manual-alignment ----
swap_ix <- c(
  which(combined$V == 10 & combined$D == 20 & combined$method == "gibbs"),
  which(combined$V == 10 & combined$D == 40 & combined$method == "gibbs"),
  which(combined$V == 15 & combined$D == 30 & combined$method == "vb"),
  which(combined$V == 10 & combined$D == 30 & combined$method == "vb"),
  which(combined$V == 10 & combined$D == 20 & combined$method == "vb"),
  which(combined$V == 15 & combined$D == 20 & combined$method == "vb")
)
tmp <- combined$value_1[swap_ix]
combined$value_1[swap_ix] <- combined$value_2[swap_ix]
combined$value_2[swap_ix] <- tmp
mcombined <- melt_reshaped_samples(combined)

## ---- boxplots ----
plot_opts <- list(
  "x" = "v",
  "y" = "sqrt(estimate)",
  "fill" = "method",
  "linetype" = "as.factor(N)"
)

p <- ggboxplot(mcombined, plot_opts) +
  geom_hline(
    data = mcombined,
    aes(yintercept = sqrt(truth)),
    alpha = 0.5, size = 0.5
  ) +
  scale_x_discrete(drop = F) +
  facet_grid(D + dimension ~ V + v, scales = "free_x", space = "free_x")

## ---- contours ----
plot_opts <- list(
  "x" = "sqrt(value_1)",
  "y" = "sqrt(value_2)",
  "group" = "v",
  "fill_type" = "gradient",
  "h" = 0.05
)
p <- ggcontours(combined, plot_opts) +
  geom_text(
    aes(x = sqrt(truth_1), y = sqrt(truth_2), label = v),
    size = 2
  ) +
  geom_text(
    data = combined %>%
      group_by(v, D, V, N, K, method) %>%
      summarise(value_mean_1 = mean(value_1), value_mean_2 = mean(value_2)),
    aes(x = sqrt(value_mean_1), y = sqrt(value_mean_2), label = v),
    size = 2, col = "#fc8d62"
  ) +
  facet_grid(method + N ~ V + D)

## ---- histograms ----
p <- error_histograms(mcombined, c("method + N", "V + D"))
