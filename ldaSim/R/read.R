#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Read in the results from the LDA experiment
##
## author: kriss1@stanford.edu

#' Bind together lists, not losing track of filenames
#' @importFrom data.table data.table
#' @export
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

#' Read RData from a vector of paths
#' @importFrom rstan extract
#' @importFrom reshape2 melt
#' @importFrom data.table rbindlist
#' @importFrom magrittr %>%
#' @export
rdata_from_paths <- function(paths, param, var_names = NULL) {
  data <- paths %>%
    lapply(function(x) {
      res <- extract(get(load(x)))
      melt(res[[param]], varnames = var_names, value.name = "value")
    }) %>%
    cbind_list(paths, "file")
  rbindlist(data)
}

#' Read feathers from a vector of paths
#' @importFrom data.table rbindlist
#' @importFrom magrittr %>%
#' @export
feather_from_paths <- function(paths) {
  ## read data into list
  data <- paths %>%
    lapply(read_feather) %>%
    cbind_list(paths, "file")

  rbindlist(data, fill = TRUE)
}
