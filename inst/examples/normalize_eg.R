# Generate data samples
set.seed(1)
df1 <- data.frame(X1 = rnorm(100, 0, 5),
                  X2 = runif(100, 1, 10),
                  X3 = rpois(100, 10))
set.seed(1)
df2 <- data.frame(X1 = rnorm(300, 0, 5),
                  X2 = runif(300, 1, 10),
                  X3 = rpois(300, 10),
                  time = rep(c(2020:2022), rep(100,3)))

# Min-max normalization
df1.mm <- normalize(inds = df1,
                    ind.pol = c("pos", "neg", "pos"))
summary(df1.mm)
df2.mm <- normalize(inds = df2[, 1:3],
                    ind.pol = c("pos", "neg", "pos"),
                    time = df2[, 4], ref.time = 2020)
summary(df2.mm)

# Goalpost normalization
df1.gp <- normalize(inds = df1, method = "goalpost",
                    ind.pol = c("pos", "neg", "pos"))
summary(df1.gp)
df2.gp <- normalize(inds = df2[, 1:3], method = "goalpost",
                    ind.pol = c("pos", "neg", "pos"),
                    time = df2[, 4], ref.time = 2020)
summary(df2.gp)
