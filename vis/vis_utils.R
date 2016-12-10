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
