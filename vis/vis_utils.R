#! /usr/bin/env Rscript

## File description ------------------------------------------------------------
## Utilities for visualizing output from lda simulation pipeline.
##
## author: kriss1@stanford.edu

## ---- data-reading ----
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

## ---- reshaping ----
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

## ---- plots ----
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

