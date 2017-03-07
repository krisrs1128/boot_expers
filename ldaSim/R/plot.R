#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Helpers for plotting output from LDA experiments.
##
## author: kriss1@stanford.edu

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
#' @importFrom ggplot2 ggplot geom_histogram aes facet_grid scale_y_continuous
#'   scale_fill_manual theme
#' @importFrom ggscaffold min_theme
#' @importFrom scales pretty_breaks
#' @export
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
    scale_y_continuous(breaks = pretty_breaks(3)) +
    scale_fill_manual(values = colors) +
    min_theme() +
    theme(
      legend.position = "bottom"
    )
}

#' Boxplots across experiment configurations
#'
#' @param mcombined [data.frame] The melted data frame including true and
#'   sampled values across experimental configurations.
#' @return p [ggplot] The ggplot object containing the sampled values across
#'   configurations.
#' @importFrom ggplot2 geom_hline scale_x_discrete facet_grid theme
#'   element_blank
#' @importFrom ggscaffold ggboxplot
#' @importFrom grid unit
#' @export
experiment_boxplots <- function(mcombined) {
  plot_opts <- list(
    "x" = "variable",
    "y" = "sqrt(estimate)",
    "fill" = "method",
    "linetype" = "as.factor(N)",
    "theme_opts" = list("border_size" = 0)
  )

  ggboxplot(mcombined, plot_opts) +
    geom_hline(
      data = mcombined,
      aes(yintercept = sqrt(truth)),
      alpha = 0.5, size = 0.5
    ) +
    geom_hline(
      aes(yintercept = 0),
      alpha = 0.0, size = 0.3
    ) +
    scale_x_discrete(drop = F) +
    facet_grid(
      D + N + dimension ~ variable,
      scale = "free_x"
    ) +
    theme(
      panel.border = element_blank(),
      panel.margin = unit(0, "line")
    )
}

#' Contour plots across topic components
#'
#' @param combined [data.frmae] The unmelted data.frame including true and
#'   sampled values across experimental configurations
#' @param return p [ggplot] The ggplot object containing samples across
#'   configurations.
#' @importFrom ggplot2 geom_text aes facet_grid
#' @importFrom ggscaffold ggcontours
#' @importFrom dplyr filter group_by summarise
#' @importFrom magrittr %>%
#' @export
experiment_contours <- function(combined) {
  plot_opts <- list(
    "x" = "sqrt(estimate_1)",
    "y" = "sqrt(estimate_2)",
    "group" = "variable",
    "fill_type" = "gradient",
    "h" = 0.05
  )

  ggcontours(combined, plot_opts) +
    geom_text(
      data = combined %>% filter(iteration == 1),
      aes(x = sqrt(truth_1), y = sqrt(truth_2), label = variable),
      size = 2
    ) +
    geom_text(
      data = combined %>%
        group_by(variable, D, V, N, K, method) %>%
        summarise(estimate_mean_1 = mean(estimate_1), estimate_mean_2 = mean(estimate_2)),
      aes(x = sqrt(estimate_mean_1), y = sqrt(estimate_mean_2), label = variable),
      size = 2, col = "#fc8d62" ) +
  facet_grid(method ~ D + N)
}
