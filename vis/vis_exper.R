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
beta <- get_truth_data(metadata, "beta")

## ---- get-samples ----
combined <- get_samples(metadata, "beta") %>%
  full_join(get_bootstraps(metadata, "beta")) %>%
  left_join(beta)

## ---- beta-alignment ----
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
experiment_boxplots(mcombined)

## ---- contours ----
experiment_contours(combined)

## ---- histograms ----
error_histograms(mcombined, c("method + N", "V + D"))
