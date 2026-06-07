# preparacao -------------------------------------------------------------

flextable::set_flextable_defaults(
  decimal.mark = ",",
  big.mark = "."
)

base_raw <- dpc5868::base_final |>
  dplyr::mutate(
    tipologia = stringr::str_to_lower(tipologia),
    ativo   = stringr::str_extract(tipologia, "^[a-z]+"),
    ativo   = dplyr::if_else(ativo == "lh", 1, 0),
    passivo = stringr::str_extract(tipologia, "[a-z]+$"),
    passivo = dplyr::if_else(passivo == "lh", 1, 0)
  )

base_extinto <- base_raw |>
  dplyr::mutate(extinto = dplyr::if_else(resultado == "Extinção", 1L, 0L))

base_resultado <- base_raw |>
  dplyr::filter(resultado != "Extinção") |>
  dplyr::mutate(
    resultado = dplyr::case_when(
      resultado %in% c("Parcialmente procedente", "Totalmente procedente") ~ 1L,
      TRUE ~ 0L
    )
  )

e1 <- glm(extinto  ~ ativo * passivo, data = base_extinto,  family = binomial)
m1 <- glm(resultado ~ ativo * passivo, data = base_resultado, family = binomial)

# funcao -----------------------------------------------------------------

tabela_coef <- function(modelo) {
  coefs <- summary(modelo)$coefficients
  ics   <- confint.default(modelo)
  tibble::tibble(
    termo   = rownames(coefs),
    coef    = coefs[, "Estimate"],
    se      = coefs[, "Std. Error"],
    ic_inf  = ics[, 1],
    ic_sup  = ics[, 2],
    p       = coefs[, "Pr(>|z|)"]
  ) |>
    dplyr::mutate(
      termo  = dplyr::case_when(
        termo == "(Intercept)" ~ "Intercepto",
        termo == "ativo"       ~ "LH (ativo)",
        termo == "passivo"     ~ "LH (passivo)",
        termo == "ativo:passivo" ~ "LH (ativo) × LH (passivo)"
      ),
      p = dplyr::if_else(p < 0.001, "<0,001", as.character(round(p, 3)))
    ) |>
    dplyr::mutate(dplyr::across(c(coef, se, ic_inf, ic_sup), \(x) round(x, 3)))
}

# tabelas ----------------------------------------------------------------

tabela_coef(e1) |>
  flextable::flextable() |>
  flextable::set_header_labels(
    termo  = "Termo",
    coef   = "Coeficiente",
    se     = "EP",
    ic_inf = "IC 95% inf.",
    ic_sup = "IC 95% sup.",
    p      = "p-valor"
  ) |>
  flextable::add_header_row(values = "Extinção", colwidths = 6) |>
  flextable::bold(part = "header") |>
  flextable::autofit()

tabela_coef(m1) |>
  flextable::flextable() |>
  flextable::set_header_labels(
    termo  = "Termo",
    coef   = "Coeficiente",
    se     = "EP",
    ic_inf = "IC 95% inf.",
    ic_sup = "IC 95% sup.",
    p      = "p-valor"
  ) |>
  flextable::add_header_row(values = "Procedência", colwidths = 6) |>
  flextable::bold(part = "header") |>
  flextable::autofit()
