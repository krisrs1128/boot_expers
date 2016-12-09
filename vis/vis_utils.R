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
