library("jsonlite")
df = expand.grid(D = c(20, 100), V = c(10, 50), K = 2, N = c(20, 50, 100), a0 = 1, b0 = 1, sigma0 = 1)
df$id <- 1:nrow(df)
cat(toJSON(df, auto_unbox = TRUE), file = "experiment.json")
