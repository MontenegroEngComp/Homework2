n <- 50
p <- 0.08
x <- 0:50

pmf <- dbinom(x, size = n, prob = p)
cdf <- pbinom(x, size = n, prob = p)

cat("Soma da PMF =", sum(pmf), "\n")
cat("F(50) =", cdf[length(cdf)], "\n")
