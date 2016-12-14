#! /usr/bin/env Rscript

# File description -------------------------------------------------------------

#' Read bootstrap replicates into a single data.frame
#'
#' @param files [character vector] A vector of paths pointing to the
#'   bootstrap replicates to read in and combine.
#' @return data [data.frame] A data.frame with the combined data,
#'   along with the file paths and bootstrap replicates associated
#'   with each row.
combine_replicates <- function(files) {
  sapply(files, read_feather)

  data <- list()
  for (i in seq_along(files)) {
    print_skip(i)

    data[[i]] <- cbind(
      file = files[i],
      rep = i,
      read_feather(files[i])
    )
  }

  do.call(rbind, data)
}

#' Plot points on a face of a high dimensional simplex
#'
#' @param p [matrix] An n x V matrix, whose rows are point on a V
#'   dimensional simplex.
#' @param coords [vector] A length 3 vector specifying the dimensions
#'   to project the points onto.
#' @return ggtern [ggtern object] A ggtern plot object of the points
#'   on the specified
simplex_proj <- function(p, coords) {
  coords <- paste0("dim_", coords)
  colnames(p) <- paste0("dim_", colnames(p))

  ggtern(p) +
    geom_point(aes_string(x = coords[1], y = coords[2], z = coords[3], col = "as.factor(dim_k)"),
               size = .5, alpha = 0.5) +
    scale_color_brewer(palette = "Set2") +
    labs(col = "k") +
    theme_nogrid()
}

#' Identify a permutation that aligns rows of two matrices
#'
#' One way to get confidence intervals for mixtures on the simplex is
#' to first specify the cluster labels for each component, and then
#' study each of these histograms on their own. This is different
#' from, say, smoothing the mixture distribution and identifying
#' modes.
#'
#' The approach taken here is to find the two rows with maximal
#' correlation and put that in the required permutation. Then, remove
#' those rows and repeat.
#'
#' @param X [numeric matrix] A matrix whose rows we want to align with
#'   X.
#' @param Z [numeric matrix] A matrix whose rows we want to align with
#'   Z.
#' @return pi_result [vector] A permutation such that X[pi_result, ] = Z
#' (ideally).
#' @examples
#' X <- matrix(rnorm(100, mean = 4) ^ 2, 20, 5)
#' pi <- sample(1:20)
#' Z <- X[pi, ] + matrix(rnorm(100), 20, 5)
#' pi_hat <- match_matrix(X, Z)
#' cbind(pi_hat, pi)
match_matrix <- function(X, Z) {
  n <- nrow(X)
  X_tilde <- X
  Z_tilde <- Z

  rownames(X_tilde) <- seq_len(n)
  rownames(Z_tilde) <- seq_len(n)
  pi_result <- rep(0, n)

  for (i in seq_len(n)) {
    # get maximal correlation in remainding rows
    rho <- cor(t(X_tilde), t(Z_tilde))
    max_ix0 <- which(rho == max(rho), arr.ind = TRUE)

    max_ix <- c(
      as.integer(rownames(X_tilde)[max_ix0[1]]),
      as.integer(rownames(Z_tilde)[max_ix0[2]])
    )

    # input to resulting permutation, and update rows
    pi_result[max_ix[2]] <- max_ix[1]
    X_tilde <- X_tilde[-max_ix0[1],, drop = F]
    Z_tilde <- Z_tilde[-max_ix0[2],, drop = F]
  }

  pi_result
}

#' Identify permutations to match multiple matrices with a preset one
#'
#' This is a wrapper of match_matrix that aligns a large collection of
#' melted matrices (Xs) to one preset one (Z).
#'
#' @param Xs [data.frame] A collection of melted matrices. The
#'   required columns are,
#'     rep: which matrix replicate is it?
#'     row: What row in the current matrix is it? This is what we will
#'       permute.  col: What column in the current matrix is it?
#' @param Z [matrix] This is the matrix to which we want to align the
#'   X matrices with.
#' @return pi_all [data.frame] The data.frame that specifies how to
#'   permutate rows within each replicate, so that the rows match
#'  as well as possible.
match_matrices <- function(Xs, Z) {
  colnames(Xs) <- c("rep", "row", "col", "value")
  R <- max(Xs$rep)

  for (i in seq_len(R)) {
    print_skip(i)

    cur_ix <- which(Xs$rep == i)
    cur_x <- Xs[cur_ix, ] %>%
      dcast(row ~ col, value.var = "value") %>%
      select(-row)

    pi <- match_matrix(cur_x, Z)
    Xs[cur_ix, "pi_ix"] <- cur_ix[pi]
    Xs[cur_ix, "pi_row"] <- Xs[cur_ix[pi], "row"]
  }
  unique(Xs[, c("rep", "row", "pi_row", "pi_ix")])
}

#' Plot many samples from theta
#'
#' Whether we are looking at bootstrap, gibbs, or variational bayes
#' posterior samples, we need to be able to plot the different sampled
#' thetas by document x cluster. This abstracts away that plot.
#'
#' @param plot_data [list] A list of data.frames containing
#'   information to plot. Contains the following slots:
#'     $samples [data.frame] Data frame of all the sampled thetas, across
#'       replicates. These will be displayed as a histogram, for each
#'       document / cluster combination.
#'     $truth [data.frame] The true theta values to display, as a
#'       reference.
#'     $fit [data.frame] The posterior mean, as a reference.
#' @param aligned [bool] If the data have been aligned, we will shade in
#'   colors accoridng to the cluster component. Otherwise, we will leave
#'   black.
#' @return p [ggplot] The ggplot object used to compare the sampled
#'   and true thetas.
theta_plot <- function(plot_data, aligned = FALSE) {
  p <- ggplot() +
    geom_histogram(data = plot_data$samples, aes(x = theta, fill = as.factor(k)),
                   binwidth = 0.01, position = "identity", alpha = 0.8) +
    geom_vline(data = plot_data$truth,
               aes(xintercept = value, col = as.factor(k)),
               linetype = 1) +
    facet_wrap(~n) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2") +
    theme(
      panel.border = element_rect(fill = "transparent", size = 0.4),
      panel.spacing = unit(0, "line"),
      axis.text = element_blank()
    )

  if (!is.null(plot_data$fit)) {
    p <- p + geom_vline(data = plot_data$fit,
                        aes(xintercept = theta, col = as.factor(k)),
                        linetype = 2)
  }
  p + labs(fill = "k", col = "k")
}

#' Plot many samples from beta
#'
#' Whether we are looking at bootstrap, gibbs, or variational bayes
#' posterior samples, we need to be able to plot the different sampled
#' betas by document x cluster. This abstracts away that plot.
#'
#' @param plot_data [list] A list of data.frames containing
#'   information to plot. Contains the following slots:
#'     $samples [data.frame] Data frame of all the sampled betas, across
#'       replicates. These will be displayed as a histogram, for each
#'       document / cluster combination.
#'     $truth [data.frame] The true beta values to display, as a
#'       reference.
#'     $fit [data.frame] The posterior mean, as a reference.
#' @param aligned [bool] If the data have been aligned, we will shade in
#'   colors accoridng to the cluster component. Otherwise, we will leave
#'   black.
#' @return p [ggplot] The ggplot object used to compare the sampled
#'   and true betas.
beta_plot <- function(plot_data, aligned = FALSE) {
  p <- ggplot() +
    geom_histogram(data = plot_data$samples, aes(x = value, fill = as.factor(k)),
                   binwidth = 0.003, position = "identity", alpha = 0.8) +
    geom_hline(yintercept = 0, size = 0.1, col = "#696969") +
    geom_vline(data = plot_data$truth, aes(xintercept = value, col = as.factor(k)),
             size = 0.5, linetype = 1) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    facet_grid(. ~ v) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2") +
    theme(
      panel.spacing = unit(0, "line"),
      axis.text.x = element_blank()
    )

  if (!is.null(plot_data$fit)) {
    p <- p + geom_vline(data = plot_data$fit, aes(xintercept = value, col = as.factor(k)),
                        size = 0.5, linetype = 2)
  }
  p + labs(fill = "k", col = "k")
}

#' Helper function, to print every p^th iteration
#'
#' This is just so we can track our progress in slow loops
#'
#' @param i [integer] The current iteration
#' @param p [integer] If p divides i, then we print i
#' @return NULL
print_skip <- function(i, p = 50) {
  if (i %% p == 0) {
    cat(sprintf("processing replicate %d\n", i))
  }
}
