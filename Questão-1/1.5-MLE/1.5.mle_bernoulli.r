set.seed(581706)
n <- 50
p_teorico <- 0.08

amostra <- rbinom(n, size = 1, prob = p_teorico)
print(amostra)

k <- sum(amostra)
p_hat <- mean(amostra)

cat("k =", k, "\n")
cat("p_hat =", p_hat, "\n")
