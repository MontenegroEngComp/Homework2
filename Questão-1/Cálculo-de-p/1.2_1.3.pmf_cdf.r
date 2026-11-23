n <- 50
p <- 0.08
x <- 0:50

pmf <- dbinom(x, size = n, prob = p)
cdf <- pbinom(x, size = n, prob = p)

cat("Soma da PMF =", sum(pmf), "\n")
cat("F(50) =", cdf[length(cdf)], "\n")

plot(x, pmf, type = "h",
     main = "PMF - Binomial(50, 0,08)",
     xlab = "Número de requisições com processamento adicional (x)",
     ylab = "P(X = x)")
points(x, pmf, pch = 19)
abline(h = 0)
