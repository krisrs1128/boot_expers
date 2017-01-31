
## ---- libraries ----
library("feather")
library("data.table")
library("plyr")
library("dplyr")
library("rstan")
library("ggplot2")

## ---- paths ----
output_path <- "/scratch/users/kriss1/output/boot_expers"
metadata <- fread(file.path(output_path, "metadata.csv"))

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

## ---- get-truth ----
truth_paths <- metadata %>%
  filter(
    is.na(method),
    grepl("beta", file)
  ) %>%
  select(file) %>%
  unlist()

truth_data <- feather_from_paths(truth_paths) %>%
  left_join(metadata)

## ---- posterior-samples ----
samples_paths <- metadata %>%
  filter(
    method %in% c("vb", "gibbs"),
    is.na(N)
  ) %>%
  select(file) %>%
  unlist()

samples <- rdata_from_paths(samples_paths, "beta", c("iteration", "v", "k")) %>%
  left_join(metadata)

## ---- bootstrap-samples ----
bootstrap_paths <- metadata %>%
  filter(
    method == "bootstrap",
    grepl("beta", file)
  ) %>%
  select(file) %>%
  unlist()

bootstraps <- data_from_paths(bootstrap_paths) %>%
  left_join(metadata)
bootstraps$iteration <- NA
samples <- rbind(samples, bootstraps)

## ---- visualize ----
p <- ggplot(samples) +
  geom_boxplot(aes(x = v, y = beta)) +
  facet_grid(N ~ V ~ k, scale = "free")
