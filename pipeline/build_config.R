library("jsonlite")
df = expand.grid(D = c(20, 30, 40), V = c(10, 15), K = 2, N = 30, alpha0 = 1, gamma0 = 1)
df$id <- 1:nrow(df)
cat(toJSON(df, auto_unbox = TRUE), file = "../conf/experiment.json")
