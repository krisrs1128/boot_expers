#! /usr/bin/env Rscript

# File description -------------------------------------------------------------

## ---- setup ----
library("jsonlite")
library("feather")
library("plyr")
library("dplyr")
library("ggplot2")
library("reshape2")

# minimal theme for ggplots
theme_set(theme_bw())
min_theme <- theme_update(
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.ticks = element_blank(),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 6),
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 8),
  strip.background = element_blank(),
  strip.text = element_text(size = 8),
  legend.key = element_blank()
)

setwd("/scratch/users/kriss1/working/boot_expers")
source("vis/vis_utils.R")
exper <- fromJSON("exper.json")

## ---- read-output ----
paths <- exper$paths
output_dir <- file.path(paths$base, paths$output_dir)
beta <- output_dir %>%
  list.files("beta_hat_[0-9]+", full.names = TRUE) %>%
  combine_replicates()
colnames(beta)[3] <- "k"
theta <- output_dir %>%
  list.files("theta_hat_[0-9]+", full.names = TRUE) %>%
  combine_replicates()

## ---- vis-theta ----
theta_truth <- file.path(paths$base, paths$params, "theta.feather") %>%
  read_feather()
theta_truth$n <- seq_len(nrow(theta_truth))
theta_truth <- theta_truth %>%
  melt(measure.vars = c("V1", "V2"), variable.name = "k")

theta_master <- output_dir %>%
  list.files("theta_hat_master", full.names = TRUE) %>%
  read_feather()

ggplot() +
  geom_histogram(data = theta, aes(x = theta), binwidth = .03) +
  geom_vline(data = theta_truth, aes(xintercept = value), col = "#8CADE1") +
  geom_vline(data = theta_master, aes(xintercept = theta), col = "#E39B5C") +
  facet_wrap(~n) +
  theme(
    panel.border = element_rect(fill = "transparent"),
    panel.margin = unit(0, "line")
  )
