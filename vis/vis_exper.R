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

## ---- theta-benchmarks ----
theta_truth <- file.path(paths$base, paths$params, "theta.feather") %>%
  read_feather()
theta_truth$n <- seq_len(nrow(theta_truth))
theta_truth <- theta_truth %>%
  melt(measure.vars = c("V1", "V2"), variable.name = "k")

theta_master <- output_dir %>%
  list.files("theta_hat_master", full.names = TRUE) %>%
  read_feather()

## ---- beta-benchmarks ----
beta_truth <- file.path(paths$base, paths$params, "beta.feather") %>%
  read_feather()
beta_truth$k <- seq_len(nrow(beta_truth))
mbeta_truth <- beta_truth %>%
  melt(id.vars = "k", variable.name = "v")
mbeta_truth$v <- gsub("V", "", mbeta_truth$v)

beta_master <- output_dir %>%
  list.files("beta_hat_master", full.names = TRUE) %>%
  read_feather()
colnames(beta_master)[1] <- "k"
beta_master <- beta_master %>%
  melt(id.vars = "k", variable.name = "v")

## ---- vis-theta ----
ggplot() +
  geom_histogram(data = theta, aes(x = theta), binwidth = 0.03) +
  geom_vline(data = theta_truth, aes(xintercept = value), col = "#8CADE1") +
  geom_vline(data = theta_master, aes(xintercept = theta), col = "#E39B5C") +
  facet_wrap(~n) +
  theme(
    panel.border = element_rect(fill = "transparent"),
    panel.margin = unit(0, "line")
  )

## ---- vis-beta ----
mbeta <- beta %>%
  melt(
    id.vars = c("file", "rep", "k"),
    variable.name = "v"
  )

ggplot() +
  geom_histogram(data = mbeta, aes(x = value), binwidth = 0.003) +
  geom_vline(data = mbeta_truth, aes(xintercept = value), col = "#8CADE1") +
  geom_vline(data = beta_master, aes(xintercept = value), col = "#E39B5C") +
  facet_wrap(~v)
