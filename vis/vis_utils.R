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
  mp <- p[, coords] %>%
    melt() %>%
    dcast(Var1 ~ Var2) %>%
    select(-Var1)
  colnames(mp) <- paste0("dim_", colnames(mp))
  dims <- colnames(mp)

  ggtern(mp) +
    geom_point(aes_string(x = dims[1], y = dims[2], z = dims[3])) +
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
#'    rep: which matrix replicate is it?
#'    row: What row in the current matrix is it? This is what we will
#'     permutate.
#'    col: What column in the current matrix is it?
#' @param Z [matrix] This is the matrix to which we want to align
#'   the X matrices with.
#' @return Xs [data.frame] A version of X with the "row" column
#'   permuted so that rows align with the rows of Z
match_matrices <- function(Xs, Z) {
  R <- max(Xs$rep)

  for (i in seq_len(R)) {
    if (i %% 50 == 0) {
      cat(sprintf("aligning replicate %d\n", i))
    }

    cur_ix <- which(Xs$rep == i)
    cur_x <- Xs[cur_ix, ] %>%
      dcast(row ~ col, value.var = "value") %>%
      select(-row)

    pi <- match_matrix(cur_x, Z)
    Xs[cur_ix, "row"] <- Xs[cur_ix[pi], "row"]
  }
  Xs
}

theta_plot <- function(plot_data, aligned = FALSE) {
  if (aligned) {
    hist_aes <- aes(x = theta, fill = as.factor(k))
  } else {
    hist_aes <- aes(x = theta)
  }

  p <- ggplot() +
    geom_histogram(data = plot_data$samples, hist_aes, binwidth = 0.01,
                   position = "identity", alpha = 0.8) +
    geom_vline(data = plot_data$truth, aes(xintercept = value), linetype = 1, col = "#696969") +
    facet_wrap(~n) +
    scale_fill_brewer(palette = "Set2") +
    theme(
      panel.border = element_rect(fill = "transparent", size = 0.4),
      panel.spacing = unit(0, "line")
    )

  if (!is.null(plot_data$fit)) {
    p <- p + geom_vline(data = plot_data$fit, aes(xintercept = theta), linetype = 2, col = "#696969")
  }

  p
}



beta_plot <- function(plot_data, aligned = FALSE) {
  if (aligned) {
    hist_aes <- aes(x = value, fill = as.factor(k))
  } else {
    hist_aes <- aes(x = value)
  }

  p <- ggplot() +
    geom_histogram(data = plot_data$samples, hist_aes, binwidth = 0.003,
                   position = "identity", alpha = 0.8) +
    geom_hline(yintercept = 0, size = 0.1, col = "#696969") +
    geom_vline(data = plot_data$truth, aes(xintercept = value), col = "#696969", size = 0.5, linetype = 1) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    facet_grid(. ~ v) +
    scale_fill_brewer(palette = "Set2") +
    theme(panel.spacing = unit(0, "line"))

  if (!is.null(plot_data$fit)) {
    p <- p + geom_vline(data = plot_data$fit, aes(xintercept = value),
                        size = 0.5, linetype = 2, col = "#696969")
  }
  p
}
