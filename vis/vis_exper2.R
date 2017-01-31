
## ---- libraries ----
library("feather")
library("data.table")
library("plyr")
library("dplyr")
library("rstan")
library("ggplot2")

## ---- utils ----
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

## ---- paths ----
output_path <- "/scratch/users/kriss1/output/boot_expers"
metadata <- fread(file.path(output_path, "metadata.csv")) %>%
  unique()

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

samples <- rdata_from_paths(samples_paths, "beta", c("iteration", "k", "v")) %>%
  left_join(metadata)

## ---- bootstrap-samples ----
bootstrap_paths <- metadata %>%
  filter(
    method == "bootstrap",
    grepl("beta", file)
  ) %>%
  select(file) %>%
  unlist()

bootstraps <- feather_from_paths(bootstrap_paths) %>%
  left_join(metadata)
bootstraps$iteration <- NA
samples <- rbind(samples, bootstraps)

## ---- visualize ----
library("ggscaffold")
plot_opts <- list(
  "x" = "v",
  "y" = "beta",
  "fill" = "method",
  "color" = "D"
)

merge_boxplot_opts(plot_opts)

cur_samples <- samples
v_order <- truth_data %>%
  group_by(v) %>%
  summarise(m_beta = max(beta)) %>%
  arrange(desc(m_beta)) %>%
  select(v) %>%
  unlist()

cur_samples$v <- factor(cur_samples$v)
truth_data$v <- factor(truth_data$v)
p <- ggboxplot(cur_samples, plot_opts) +
  geom_hline(
    data = truth_data,
    aes(yintercept = beta),
    alpha = 0.5, size = 0.5
  ) +
  facet_grid(facet_vals, scales = "free_x", space = "free_x")
p

facet_vals <- paste0(c("D + V + k", "v + N"), collapse = " ~ ")

cast_samples <- cur_samples %>%
  dcast(file + iteration + v + D + V + N + K + alpha0 + gamma0 + alpha_fit + gamma_fit + n_replicates + batch_id + n_samples + method ~ k, value.var = "beta")

colnames(cast_samples) <- make.names(colnames(cast_samples))

plot_opts <- list(
  "x" = "X1",
  "y" = "X2",
  "group" = "v",
  "fill_type" = "gradient",
  "h" = 0.1
)
ggcontours(cast_samples, plot_opts) +
  facet_grid(method ~ V + N + D)

ggsave("~/test.png", p)
