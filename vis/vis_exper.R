#! /usr/bin/env Rscript

# File description -------------------------------------------------------------

## ---- setup ----
library("jsonlite")
library("feather")
library("plyr")
library("dplyr")
library("ggplot2")
library("ggtern")
library("reshape2")
library("FactoMineR")

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
exper <- fromJSON("expers/exper_d100_n100.json")

## ---- read-output ----
paths <- exper$paths
output_dir <- file.path(paths$base, paths$output_dir)
beta <- output_dir %>%
  list.files("beta_boot_[0-9]+", full.names = TRUE) %>%
  combine_replicates()
colnames(beta)[3] <- "k"
theta <- output_dir %>%
  list.files("theta_boot_[0-9]+", full.names = TRUE) %>%
  combine_replicates()

## ---- theta-benchmarks ----
theta_truth <- file.path(paths$base, paths$params, "theta.feather") %>%
  read_feather()
theta_truth$n <- seq_len(nrow(theta_truth))
theta_truth <- theta_truth %>%
  melt(measure.vars = c("V1", "V2"), variable.name = "k")

theta_master <- output_dir %>%
  list.files("theta_hat_vb", full.names = TRUE) %>%
  read_feather()

## ---- beta-benchmarks ----
beta_truth <- file.path(paths$base, paths$params, "beta.feather") %>%
  read_feather()
beta_truth$k <- seq_len(nrow(beta_truth))
mbeta_truth <- beta_truth %>%
  melt(id.vars = "k", variable.name = "v")
mbeta_truth$v <- gsub("V", "", mbeta_truth$v)

beta_master <- output_dir %>%
  list.files("beta_hat_vb", full.names = TRUE) %>%
  read_feather()
colnames(beta_master)[1] <- "k"
beta_master <- beta_master %>%
  melt(id.vars = "k", variable.name = "v")

## ---- vis-theta ----
ggplot() +
  geom_histogram(data = theta, aes(x = theta), binwidth = 0.01) +
  geom_vline(data = theta_truth, aes(xintercept = value), linetype = 1, col = "#696969") +
  geom_vline(data = theta_master, aes(xintercept = theta), linetype = 2, col = "#696969") +
  facet_wrap(~n) +
  theme(
    panel.border = element_rect(fill = "transparent", size = 0.4),
    panel.spacing = unit(0, "line")
  )

## ---- vis-beta ----
mbeta <- beta %>%
  melt(
    id.vars = c("file", "rep", "k"),
    variable.name = "v"
  )

v_order <- mbeta %>%
  group_by(v) %>%
  summarise(mean = max(value)) %>%
  arrange(desc(mean)) %>%
  select(v) %>%
  unlist()

mbeta$v <- factor(mbeta$v, levels = v_order)
mbeta_truth$v <- factor(mbeta_truth$v, levels = v_order)
beta_master$v <- factor(beta_master$v, levels = v_order)

ggplot() +
  geom_hline(yintercept = 0, size = 0.1, col = "#696969") +
  geom_histogram(data = mbeta, aes(x = value), binwidth = 0.003) +
  geom_vline(data = mbeta_truth, aes(xintercept = value, y = 0), col = "#696969", size = 0.5, linetype = 1) +
  geom_vline(data = beta_master, aes(xintercept = value, y = 0), col = "#696969", size = 0.5, linetype = 2) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  facet_grid(. ~ v) +
  theme(
    panel.spacing = unit(0, "line")
  )

## ---- tours ----
projs <- combn(exper$model$V, 3)

p_beta <- mbeta %>%
  select(-file) %>%
  dcast(k + rep ~ v) %>%
  select(-k, -rep) %>%
  as.matrix()

for (i in seq_len(10)) {
  simplex_proj(p_beta, projs[, i]) %>%
    print()
}

## ---- correspondence-analysis ----
p_beta_master <- beta_master %>%
  dcast(k ~ v) %>%
  select(-k) %>%
  as.matrix()

p_beta_truth <- mbeta_truth %>%
  dcast(k ~ v) %>%
  select(-k) %>%
  as.matrix()

p_beta <- rbind(
  data.frame(type = "bootstrap", p_beta),
  data.frame(type = "master", p_beta_master),
  data.frame(type = "truth", p_beta_truth)
)

ca_beta <- CA(p_beta %>% select(-type))
beta_row_coords <- data.frame(
  type = p_beta$type,
  ca_beta$row$coord
)

ggplot() +
  geom_point(data = beta_row_coords %>% filter(type == "bootstrap"),
             aes(x = Dim.1, y = Dim.2, col = type), size = .5, alpha = 0.5) +
  geom_point(data = beta_row_coords %>% filter(type != "bootstrap"),
             aes(x = Dim.1, y = Dim.2, col = type)) +
  scale_color_brewer(palette = "Set2") +
  coord_fixed()

## ---- alignment-approach ----
R <- max(mbeta$rep)
beta_master_mat <- beta_master %>%
  dcast(k ~ v) %>%
  select(-k) %>%
  as.matrix()

old_names <- colnames(mbeta)
colnames(mbeta) <- c("file", "rep", "row", "col", "value")
mbeta <- match_matrices(mbeta, beta_master_mat)
colnames(mbeta) <- old_names

mbeta_truth$k <- rep(c(2, 1), 10)

## ---- visualize-aligned ----
ggplot() +
  geom_vline(data = mbeta_truth, aes(xintercept = value, y = 0, col = as.factor(k)), size = 0.5, linetype = 1) +
  geom_vline(data = beta_master, aes(xintercept = value, y = 0, col = as.factor(k)), size = 0.5, linetype = 2) +
  geom_histogram(data = mbeta,
                 aes(x = value, fill = as.factor(k)),
                 binwidth = .003, alpha = 0.8, position = "identity") +
  geom_hline(yintercept = 0, size = 0.1, col = "#696969") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  facet_grid(. ~ v) +
  theme(
    panel.spacing = unit(0, "line")
  )

## ---- align-theta ----
theta_master_mat <- theta_master %>%
  dcast(k ~ n) %>%
  select(-k) %>%
  as.matrix()

old_names <- colnames(theta)
colnames(theta) <- c("file", "rep", "col", "row", "value")
theta <- match_matrices(theta, theta_master_mat)
colnames(theta) <- old_names

ggplot() +
  geom_histogram(data = theta, aes(x = theta, fill = as.factor(k)),
                 binwidth = 0.01, alpha = 0.8, position = "identity") +
  geom_vline(data = theta_truth, aes(xintercept = value), linetype = 1, col = "#696969") +
  geom_vline(data = theta_master, aes(xintercept = theta), linetype = 2, col = "#696969") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 150) , oob = rescale_none) +
  facet_wrap(~n) +
  theme(
    panel.border = element_rect(fill = "transparent", size = 0.4),
    panel.spacing = unit(0, "line")
  )

## ---- vb-samples-theta ----
theta_samples <- file.path(output_dir, "theta_samples_vb.feather") %>%
  read_feather()
colnames(theta_samples) <- c("rep", "n", "k", "theta")

ggplot() +
  geom_histogram(data = theta_samples, aes(x = theta, fill = as.factor(k)),
                 binwidth = 0.01, alpha = 0.8, position = "identity") +
  geom_vline(data = theta_truth, aes(xintercept = value), linetype = 1, col = "#696969") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 150) , oob = scales::rescale_none) +
  facet_wrap(~n) +
  theme(
    panel.border = element_rect(fill = "transparent", size = 0.4),
    panel.spacing = unit(0, "line")
  )

## ---- vb-samples-beta ----
beta_samples <- file.path(output_dir, "beta_samples_vb.feather") %>%
  read_feather()
colnames(beta_samples) <- c("rep", "k", "v", "value")

beta_samples$v <- factor(beta_samples$v, levels = v_order)

ggplot() +
  geom_vline(data = mbeta_truth, aes(xintercept = value, y = 0, col = as.factor(k)), size = 0.5, linetype = 1) +
  geom_histogram(data = beta_samples,
                 aes(x = value, fill = as.factor(k)),
                 binwidth = .003, alpha = 0.8, position = "identity") +
  geom_hline(yintercept = 0, size = 0.1, col = "#696969") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  facet_grid(. ~ v) +
  theme(
    panel.spacing = unit(0, "line")
  )

## ---- gibbs-samples-beta ----
beta_samples <- file.path(output_dir, "beta_samples_gibbs.feather") %>%
  read_feather()
colnames(beta_samples) <- c("rep", "row", "col", "value")
beta_samples <- match_matrices(beta_samples, beta_master_mat)
colnames(beta_samples) <- c("rep", "k", "v", "value")
beta_samples$v <- factor(beta_samples$v, levels = v_order)

ggplot() +
  geom_vline(data = mbeta_truth, aes(xintercept = value, y = 0, col = as.factor(k)), size = 0.5, linetype = 1) +
  geom_histogram(data = beta_samples,
                 aes(x = value, fill = as.factor(k)),
                 binwidth = .003, alpha = 0.8, position = "identity") +
  geom_hline(yintercept = 0, size = 0.1, col = "#696969") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  facet_grid(. ~ v) +
  theme(
    panel.spacing = unit(0, "line")
  )
