#! /usr/bin/env Rscript

# File description -------------------------------------------------------------

#' Read bootstrap replicates into a single data.frame
#'
#' @param files {character vector} A vector of paths pointing to the
#'   bootstrap replicates to read in and combine.
#' @return data {data.frame} A data.frame with the combined data,
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
