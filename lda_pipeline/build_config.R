library("jsonlite")
df = expand.grid(D = c(20, 100), V = c(750, 1500), K = 2, N = c(3750, 7500, 15000), alpha0 = 1, gamma0 = 1)
df$id <- 1:nrow(df)
cat(toJSON(df, auto_unbox = TRUE), file = "experiment.json")
