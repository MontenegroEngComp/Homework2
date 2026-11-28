# 1. definição dos parâmetros da equipe
n <- 50
p <- 0.08
E_X <- n * p  # esperança E[X] = 4

# 2. cálculo exato das probabilidades em R

# (a) P(X >= 20) = P(X > 19)
prob_a <- pbinom(19, size = n, prob = p, lower.tail = FALSE)
prob_a_soma <- sum(dbinom(20:50, size = n, prob = p))
stopifnot(isTRUE(all.equal(prob_a, prob_a_soma)))

# (b) P(30 < X < 43) = P(31 <= X <= 42)
prob_b <- sum(dbinom(31:42, size = n, prob = p))
prob_b_tail <- pbinom(30, size = n, prob = p, lower.tail = FALSE) -
  pbinom(42, size = n, prob = p, lower.tail = FALSE)
stopifnot(isTRUE(all.equal(prob_b, prob_b_tail)))

# (c) P(X = 31)
prob_c <- dbinom(31, size = n, prob = p)

# extra para a Questão 3: r = P(X >= E[X]) = P(X >= 4)
lim_inf <- ceiling(round(E_X, 10)) - 1
r_val <- pbinom(lim_inf, size = n, prob = p, lower.tail = FALSE)
r_val_comp <- 1 - pbinom(lim_inf, size = n, prob = p)
stopifnot(isTRUE(all.equal(r_val, r_val_comp)))

# 3. validação do método manual (logaritmos + recorrência)
# função auxiliar para somar k_ini até k_ini + m termos via recorrência
soma_recorrencia <- function(k_ini, m_termos, n, p) {
  ln_p0 <- lchoose(n, k_ini) + k_ini * log(p) + (n - k_ini) * log(1 - p)
  termos <- numeric(m_termos + 1)
  termos[1] <- exp(ln_p0)
  for (i in seq_len(m_termos)) {
    k <- k_ini + i - 1
    q_k <- ((n - k) / (k + 1)) * (p / (1 - p))
    termos[i + 1] <- termos[i] * q_k
  }
  list(ln_p0 = ln_p0, p0 = termos[1], soma = sum(termos))
}

man_a <- soma_recorrencia(20, 4, n, p) # P(20) até P(24)
man_b <- soma_recorrencia(31, 3, n, p) # P(31) até P(34)
man_c <- man_b$p0                      # P(31) via logaritmo
man_r <- 1 - sum(round(dbinom(0:3, n, p), 6)) 

# conferência dos erros relativos
vals_r   <- c(prob_a, prob_b, prob_c, r_val)
vals_man <- c(man_a$soma, man_b$soma, man_c, man_r)
erro_rel <- abs(vals_man - vals_r) / vals_r * 100

# 4. tabela resumo dos resultados
resultados <- data.frame(
  Evento = c(
    "P(X >= 20)",
    "P(30 < X < 43)",
    "P(X = 31)",
    paste0("r = P(X >= ", E_X, ")")
  ),
  Valor_Manual = vals_man,
  Valor_R = vals_r,
  Erro_Relativo_Pct = erro_rel
)

print(resultados, row.names = FALSE)
cat("\nValor de r com alta precisão:\n")
print(r_val, digits = 10)

# 5. exportação de r_val para a Questão 3
saveRDS(r_val, file = "r_val.rds")