n <- 50
p <- 0.08
x <- 0:50

pmf <- dbinom(x, size = n, prob = p)
cdf <- pbinom(x, size = n, prob = p)

cat("Soma da PMF =", sum(pmf), "\n")
cat("F(50) =", cdf[length(cdf)], "\n")

plot(x, pmf, type = "h",
     main = "PMF da Binomial",
     xlab = "Número de requisições (x)",
     ylab = "P(X = x)")
points(x, pmf, pch = 19)
abline(h = 0)

plot(x, cdf, type = "s", ylim = c(0, 1),
     main = "CDF da Binomial",
     xlab = "Número de requisições (x)",
     ylab = "P(X <= x)")
abline(h = c(0, 1), lty = 2)
