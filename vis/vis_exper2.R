
## ---- libraries ----
library("feather")
library("data.table")
library("plyr")
library("dplyr")
library("ggplot2")

## ---- paths ----
output_path <- "/scratch/users/kriss1/output/boot_expers"
metadata <- fread(file.path(output_path, "metadata.csv"))

data_from_paths <- function(paths, feather = TRUE, param = "beta", var_names = NULL, value_name = "value") {
  ## read data into list
  if (feather) {
    data <- paths %>%
      lapply(read_feather)
  } else {
    data <- paths %>%
      lapply(function(x) {
        res <- extract(get(load(x)))
        melt(res[[param]], varnames = var_names, value.name = value_name)
      })
  }

  ## bind together, not losing tracks of filename
  for (i in seq_along(data)) {
    if (nrow(data[[i]]) == 0) next
    data[[i]] <- cbind(
      "file" = paths[[i]],
      data[[i]]
    )
  }
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

truth_data <- data_from_paths(truth_paths) %>%
  left_join(metadata)

## ---- posterior-samples ----
samples_paths <- metadata %>%
  filter(
    method %in% c("vb", "gibbs"),
    is.na(N)
  ) %>%
  select(file) %>%
  unlist()

samples <- data_from_paths(samples_paths, FALSE, "beta", c("iteration", "v", "k"), "beta") %>%
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
