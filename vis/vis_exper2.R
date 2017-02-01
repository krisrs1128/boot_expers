
## ---- libraries ----
library("feather")
library("data.table")
library("plyr")
library("dplyr")
library("rstan")
library("ggplot2")
library("ggscaffold")

## ---- utils ----
cbind_list <- function(data_list, cbind_vals, cbind_name) {
  ## bind together, not losing tracks of filename
  for (i in seq_along(data_list)) {
    if (nrow(data_list[[i]]) == 0) next
    data_list[[i]] <- data.table(
      cbind_vals[[i]],
      data_list[[i]]
    )
    colnames(data_list[[i]])[1] <- cbind_name
  }
  data_list
}

rdata_from_paths <- function(paths, param, var_names = NULL) {
  data <- paths %>%
    lapply(function(x) {
      res <- extract(get(load(x)))
      melt(res[[param]], varnames = var_names, value.name = param)
    }) %>%
    cbind_list(paths, "file")
  rbindlist(data)
}

feather_from_paths <- function(paths) {
  ## read data into list
  data <- paths %>%
    lapply(read_feather) %>%
    cbind_list(paths, "file")

  rbindlist(data, fill = TRUE)
}

#' Errors histogram
#'
#' Plot the histograms of errors associated with the scatterplots from the NMF fits.
#'
#' @param plot_data [data.frame] The data used to plot the error between truth
#'   vs. estimate across all dimensions. See the output of
#'   melt_reshaped_samples().
#' @param facet_terms [character vector] The columns on which to facet_grid the
#'   plot.
#' @param n_bins [int] The number of bins in each histogram panel. Defaults to 75.
#' @param alpha [numeric] The alpha transparency for the different factors.
#' @param colors [character vector] The colors to use for each factor.
#' @return hist_plot [ggplot] The ggplot object showing error histograms across
#'   factors and simulation configurations.
error_histograms <- function(plot_data,
                             facet_terms = NULL,
                             n_bins = 75,
                             alpha = 0.7,
                             colors = c("#d95f02", "#7570b3")) {
  ggplot(plot_data) +
    geom_histogram(
      aes(x = sqrt(estimate) - sqrt(truth), fill = dimension, y = ..density..),
      position = "identity", alpha = alpha, bins = n_bins
    ) +
    facet_grid(formula(paste(facet_terms, collapse = "~"))) +
    scale_y_continuous(breaks = scales::pretty_breaks(3)) +
    scale_fill_manual(values = colors) +
    min_theme() +
    theme(
      legend.position = "bottom"
    )
}

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
  mutate(k = paste0("beta_truth_", k)) %>%
  dcast(file + v ~ k) %>%
  left_join(metadata) %>%
  select(v, starts_with("beta_truth"), D, V, K, alpha0, gamma0)

## ---- posterior-samples ----
samples_paths <- metadata %>%
  filter(
    method %in% c("vb", "gibbs")
  ) %>%
  select(file) %>%
  unlist()

samples <- rdata_from_paths(samples_paths, "beta", c("iteration", "k", "v")) %>%
  mutate(k = paste0("beta_", k)) %>%
  dcast(file + iteration + v ~ k) %>%
  left_join(metadata) %>%
  select(-iteration, -file, -alpha0, -gamma0, -n_replicates, -batch_id)

## ---- bootstrap-samples ----
bootstrap_paths <- metadata %>%
  filter(
    method == "bootstrap",
    grepl("beta", file)
  ) %>%
  select(file) %>%
  unlist()

bootstraps <- feather_from_paths(bootstrap_paths) %>%
  mutate(k = paste0("beta_", k)) %>%
  dcast(file + v ~ k) %>%
  left_join(metadata) %>%
  select(-file, -alpha0, -gamma0, -n_replicates, -batch_id)

combined <- samples %>%
  left_join(bootstraps) %>%
  left_join(truth_data)

## ---- visualize ----
plot_opts <- list(
  "x" = "v",
  "y" = "beta",
  "fill" = "method",
  "linetype" = "as.factor(N)",
  "color" = "D"
)

p <- ggboxplot(combined %>% top_n(1000), plot_opts) +
  geom_hline(
    aes(yintercept = beta_truth),
    alpha = 0.5, size = 0.5
  ) +
  facet_grid(D + k ~ V + v, scales = "free_x", space = "free_x")

plot_opts <- list(
  "x" = "sqrt(beta_1)",
  "y" = "sqrt(beta_2)",
  "group" = "v",
  "fill_type" = "gradient",
  "h" = 0.05
)
p <- ggcontours(combined, plot_opts) +
  geom_text(
    aes(x = sqrt(beta_truth_1), y = sqrt(beta_truth_2), label = v),
    size = 2
  ) +
  geom_text(
    data = combined %>%
      group_by(v, D, V, N, K, method) %>%
      summarise(beta_mean_1 = mean(beta_1), beta_mean_2 = mean(beta_2)),
    aes(x = sqrt(beta_mean_1), y = sqrt(beta_mean_2), label = v),
    size = 2, col = "#fc8d62"
  ) +
  facet_grid(method + N ~ V + D)

## histograms of errors ##
mcombined <- combined %>%
  melt(
    measure.vars = c("beta_1", "beta_2"),
    variable.name = "dimension",
    value.name = "estimate"
  ) %>%
  melt(
    measure.vars = c("beta_truth_1", "beta_truth_2"),
    variable.name = "dimension2",
    value.name = "truth"
  ) %>%
  select(-dimension2)

error_histograms(mcombined, c("method + N", "V + D"))
