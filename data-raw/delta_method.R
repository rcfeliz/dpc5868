# Delta method: passo a passo para Delta_ativo = p2 - p1
#
# Modelo: Y ~ ativo * passivo (logit)
# Delta_ativo = P(Y=1 | ativo=1, passivo=0) - P(Y=1 | ativo=0, passivo=0)
#             = p2 - p1

# preparacao -------------------------------------------------------------

base_raw <- dpc5868::base_final |>
  dplyr::mutate(
    tipologia = stringr::str_to_lower(tipologia),
    ativo = stringr::str_extract(tipologia, "^[a-z]+"),
    ativo = dplyr::if_else(ativo == "lh", 1, 0),
    passivo = stringr::str_extract(tipologia, "[a-z]+$"),
    passivo = dplyr::if_else(passivo == "lh", 1, 0)
  )

base_extinto <- base_raw |>
  dplyr::mutate(extinto = dplyr::if_else(resultado == "Extinção", 1L, 0L))

modelo <- glm(extinto ~ ativo * passivo, data = base_extinto, family = binomial)

# passo 1: coeficientes estimados ----------------------------------------

b <- coef(modelo)
# b[1] = beta_0 (intercepto)
# b[2] = beta_1 (ativo)
# b[3] = beta_2 (passivo)
# b[4] = beta_3 (ativo:passivo)

# passo 2: matriz de covariancia dos coeficientes ------------------------

V <- vcov(modelo)

# passo 3: probabilidades preditas nos dois perfis -----------------------

# p1: ativo=0, passivo=0 -> logit = beta_0
# p2: ativo=1, passivo=0 -> logit = beta_0 + beta_1

logit_p1 <- b["(Intercept)"]
logit_p2 <- b["(Intercept)"] + b["ativo"]

p1 <- plogis(logit_p1)
p2 <- plogis(logit_p2)

# passo 4: efeito marginal -----------------------------------------------

delta_ativo <- p2 - p1

# passo 5: gradiente de delta em relacao a beta --------------------------
#
# p = plogis(x) => dp/dx = p * (1 - p)
#
# d(p2)/d(beta_0) = p2 * (1 - p2)   [x2 = beta_0 + beta_1, dx/d(beta_0) = 1]
# d(p2)/d(beta_1) = p2 * (1 - p2)   [dx/d(beta_1) = 1]
# d(p2)/d(beta_2) = 0                [x2 nao depende de beta_2]
# d(p2)/d(beta_3) = 0                [x2 nao depende de beta_3]
#
# d(p1)/d(beta_0) = p1 * (1 - p1)
# d(p1)/d(beta_1) = 0
# d(p1)/d(beta_2) = 0
# d(p1)/d(beta_3) = 0
#
# gradiente de delta = d(p2)/d(beta) - d(p1)/d(beta)

grad <- c(
  p2 * (1 - p2) - p1 * (1 - p1), # d(delta)/d(beta_0)
  p2 * (1 - p2), # d(delta)/d(beta_1)
  0, # d(delta)/d(beta_2)
  0 # d(delta)/d(beta_3)
)

# passo 6: variancia do delta (formula do delta method) ------------------
#
# Var(delta) = grad' * V * grad

var_delta <- as.numeric(t(grad) %*% V %*% grad)

# passo 7: erro padrao ---------------------------------------------------

se_delta <- sqrt(var_delta)

# passo 8: intervalo de confianca (95%) ----------------------------------

ic_inf <- delta_ativo - 1.96 * se_delta
ic_sup <- delta_ativo + 1.96 * se_delta

# passo 9: teste de wald -------------------------------------------------

z <- delta_ativo / se_delta
p_valor <- 2 * pnorm(-abs(z))

# resultado --------------------------------------------------------------

cat("Delta (ativo):", round(delta_ativo, 4), "\n")
cat("Erro padrão:  ", round(se_delta, 4), "\n")
cat("IC 95%:       [", round(ic_inf, 4), ";", round(ic_sup, 4), "]\n")
cat("z:            ", round(z, 4), "\n")
cat("p-valor:      ", round(p_valor, 4), "\n")
