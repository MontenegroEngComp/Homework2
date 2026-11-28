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
cat("erro_padrao (n=50) =", round(erro_padrao, 4), "\n")
