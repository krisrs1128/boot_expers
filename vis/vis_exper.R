#! /usr/bin/env Rscript

# File description -------------------------------------------------------------

## ---- setup ----
library("jsonlite")
library("feather")
library("plyr")
library("dplyr")

setwd("/scratch/users/kriss1/working/boot_expers")
source("vis/vis_utils.R")
exper <- fromJSON("exper.json")

## ---- read-output ----
paths <- exper$paths
beta <- list.files(file.path(paths$base, paths$output_dir), "beta_hat_[0-9]+", full.names = TRUE) %>%
  combine_replicates()
colnames(beta)[3] <- "k"
theta <- list.files(file.path(paths$base, paths$output_dir), "theta_hat_[0-9]+", full.names = TRUE) %>%
  combine_replicates()

## ---- vis-theta ----
