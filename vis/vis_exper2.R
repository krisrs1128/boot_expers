
## ---- libraries ----
library("feather")
library("data.table")
library("plyr")
library("dplyr")
library("tidyr")
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
      res <- rstan::extract(get(load(x)))
      melt(res[[param]], varnames = var_names, value.name = "value")
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

#' Melt reshaped samples
#'
#' While for the scatter / contours figures, it's useful to have the dimensions
#' as separate columns, we'll also want to the melted data when computing
#' explicit errors. This takes the output of reshaped_... and melts it so that
#' it's appropriate for histogramming the errors.
#'
#' @param samples [data.frame] The wide samples data, usually the output of
#'   reshape_all_samples.
#' @return melted_samples [data.frame] The same data as samples, but with
#'   different factor dimensions all stacked.
#' @export
melt_reshaped_samples <- function(samples) {
  melted_samples <- samples %>%
    gather(type, val, starts_with("value"), starts_with("truth")) %>%
    separate("type", c("estimate_type", "dimension"), "\\_") %>%
    spread(estimate_type, val) %>%
    rename(estimate = value)

  melted_samples$dimension <- paste0("k=", melted_samples$dimension)
  melted_samples
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

## ---- visualize ----
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
error_histograms(mcombined, c("method + N", "V + D"))
