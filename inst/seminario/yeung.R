# fabricando a base ------------------------------------------------------

df <- tibble::tibble(
  union = c(
    rep(1, 7311), # 1 1
    rep(1, 1422), # 1 0
    rep(0, 6190), # 0 1
    rep(0, 3181) # 0 0
  ),
  dismiss = c(
    rep(1, 7311), # 1 1
    rep(0, 1422), # 1 0
    rep(1, 6190), # 0 1
    rep(0, 3181) # 0 0
  ),
  lawsuit = c(
    rep(1, 5829), # 1 1 1
    rep(0, 7311 - 5829), # 1 1 0
    rep(1, 917), # 1 0 1
    rep(0, 1422 - 917), # 1 0 0
    rep(1, 3885), # 0 1 1
    rep(0, 6190 - 3885), # 0 1 0
    rep(1, 786), # 0 0 1
    rep(0, 3181 - 786) # 0 0 0
  )
) |>
  dplyr::mutate(id = dplyr::row_number())

# modelo -----------------------------------------------------------------

modelo <- glm(
  lawsuit ~ union + dismiss + union * dismiss,
  data = df,
  family = 'binomial'
)

# extraindo os coeficientes
betas <- coef(modelo)
Y00 <- betas[1] # intercepto
Y10 <- betas[2] # coeficiente de union
Y01 <- betas[3] # coeficiente de dismiss
Y11 <- betas[4] # coeficiente da interação

# efeitos marginais ------------------------------------------------------

# calculando os efeitos
delta_union <- Y10 - Y00 # efeito puro de union
delta_dismiss <- Y01 - Y00 # efeito puro de dismiss
delta_interacao1 <- Y11 - delta_union - delta_dismiss # método BCG
delta_interacao2 <- (Y11 - Y01) - (Y10 - Y00) # método AN

cat("Y00 (union=0, dismiss=0):", Y00, "\n")
cat("Y01 (union=0, dismiss=1):", Y01, "\n")
cat("Y10 (union=1, dismiss=0):", Y10, "\n")
cat("Y11 (union=1, dismiss=1):", Y11, "\n")
cat("\n")

cat("Efeito de union:", delta_union, "\n")
cat("Efeito de dismiss:", delta_dismiss, "\n")
cat("Efeito de interação (BCG):", delta_interacao1, "\n")
cat("Efeito de interação (AI):", delta_interacao2, "\n")


# ods ratio - phat - marginal effect -------------------------------------

# Função para calcular deltas dados os ORs
calcular_deltas <- function(
  OR_intercept,
  OR_union,
  OR_dismiss,
  OR_interaction
) {
  # Converter para betas
  b0 <- log(OR_intercept)
  b1 <- log(OR_union)
  b2 <- log(OR_dismiss)
  b3 <- log(OR_interaction)

  # Calcular etas
  eta_Y1 <- b0
  eta_Y2 <- b0 + b1
  eta_Y3 <- b0 + b2
  eta_Y4 <- b0 + b1 + b2 + b3

  # Converter para p_hat
  Y1 <- plogis(eta_Y1)
  Y2 <- plogis(eta_Y2)
  Y3 <- plogis(eta_Y3)
  Y4 <- plogis(eta_Y4)

  # Deltas
  delta1 <- Y2 - Y1 # efeito da sindicalização para não demitidos
  delta2 <- Y4 - Y3 # efeito da sindicalização para demitidos
  delta3 <- delta1 - delta2 # efeito de interação (Ai & Norton)

  return(list(
    p_hat = tibble::tibble(
      grupo = c(
        "Y1 (not union, not dismissed)",
        "Y2 (union, not dismissed)",
        "Y3 (not union, dismissed)",
        "Y4 (union, dismissed)"
      ),
      eta = c(eta_Y1, eta_Y2, eta_Y3, eta_Y4),
      p_hat = c(Y1, Y2, Y3, Y4)
    ),
    deltas = tibble::tibble(
      delta = c(
        "Δ1 (union effect | not dismissed)",
        "Δ2 (union effect | dismissed)",
        "Δ3 (interaction — Ai & Norton)"
      ),
      value = c(delta1, delta2, delta3)
    )
  ))
}

# Função simplificada que retorna apenas os valores dos deltas
deltas_valores <- function(OR_intercept, OR_union, OR_dismiss, OR_interaction) {
  # Converter para betas
  b0 <- log(OR_intercept)
  b1 <- log(OR_union)
  b2 <- log(OR_dismiss)
  b3 <- log(OR_interaction)

  # Calcular etas
  eta_Y1 <- b0
  eta_Y2 <- b0 + b1
  eta_Y3 <- b0 + b2
  eta_Y4 <- b0 + b1 + b2 + b3

  # Converter para p_hat
  Y1 <- plogis(eta_Y1)
  Y2 <- plogis(eta_Y2)
  Y3 <- plogis(eta_Y3)
  Y4 <- plogis(eta_Y4)

  # Deltas
  delta1 <- Y2 - Y1 # efeito da sindicalização para não demitidos
  delta2 <- Y4 - Y3 # efeito da sindicalização para demitidos
  delta3 <- delta1 - delta2 # efeito de interação (Ai & Norton)

  return(c(delta1, delta2, delta3))
}

tibble::tibble(
  delta = c(
    "Δ Union",
    "Δ Dismissed",
    "Δ Interaction (Ai & Norton)"
  ),
  tabela_4 = deltas_valores(38.564, 9.412, 2.204, 0.875),
  tabela_7 = deltas_valores(0.032, 7.319, 1.907, 0.780)
)
