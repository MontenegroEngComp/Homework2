set.seed(581706)
n <- 50
p_teorico <- 0.08

amostra <- rbinom(n, size = 1, prob = p_teorico)
print(amostra)

k <- sum(amostra)
p_hat <- mean(amostra)

cat("k =", k, "\n")
cat("p_hat =", p_hat, "\n")

diferenca <- p_hat - p_teorico
erro_abs <- abs(diferenca)
erro_rel <- (erro_abs / p_teorico) * 100
erro_padrao <- sqrt(p_teorico * (1 - p_teorico) / n)

cat("p_teorico =", p_teorico, "\n")
cat("diferenca =", diferenca, "\n")
cat("erro_relativo (%) =", erro_rel, "\n")
cat("erro_padrao (n=50) =", round(erro_padrao, 4), "\n\n")

tamanhos <- c(50, 500, 5000, 50000)
comparacao_n <- data.frame(
  n = tamanhos,
  k = NA_integer_,
  p_hat = NA_real_,
  erro_absoluto = NA_real_,
  erro_padrao_teorico = sqrt(p_teorico * (1 - p_teorico) / tamanhos)
)

for (i in seq_along(tamanhos)) {
  ni <- tamanhos[i]
  amostra_i <- if (ni == 50) amostra else rbinom(ni, size = 1, prob = p_teorico)
  comparacao_n$k[i] <- sum(amostra_i)
  comparacao_n$p_hat[i] <- mean(amostra_i)
  comparacao_n$erro_absoluto[i] <- abs(comparacao_n$p_hat[i] - p_teorico)
}

print(comparacao_n, digits = 5, row.names = FALSE)
