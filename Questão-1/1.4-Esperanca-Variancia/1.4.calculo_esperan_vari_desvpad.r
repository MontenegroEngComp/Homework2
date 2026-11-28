
n <- 50
p <- 0.08

valor_esperado <- n * p
variancia <- n * p * (1 - p)
desvio_padrao <- sqrt(variancia)

cat("E[X] =", valor_esperado, "\n")
cat("Var(X) =", variancia, "\n")
cat("DP(X) =", desvio_padrao, "\n")
