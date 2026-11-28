set.seed(581706)
n <- 50
p <- 0.08

amostra <- rbinom(n, size = 1, prob = p)
p_hat <- mean(amostra)

cat("k =", sum(amostra), "\n")
cat("p_hat =", p_hat, "\n")
cat("diferenca =", p_hat - p, "\n")

tamanhos <- c(50, 500, 5000, 50000)
p_hats <- c(p_hat, sapply(tamanhos[-1], function(ni) mean(rbinom(ni, 1, p))))
print(data.frame(n = tamanhos, p_hat = p_hats, erro = abs(p_hats - p)))

desenhar <- function() {
  plot(tamanhos, p_hats, type = "b", log = "x", pch = 19, ylim = c(0.06, 0.11),
       main = "Convergência do MLE", xlab = "Tamanho da amostra (n)", ylab = "Estimativa (p_hat)")
  abline(h = p, col = "red", lty = 2)
}

png("Questão-1/imagens/mle_convergencia.png", width = 1600, height = 1000, res = 200)
desenhar()
dev.off()

if (interactive()) desenhar()
