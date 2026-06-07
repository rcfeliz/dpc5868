# preparacao -------------------------------------------------------------

set.seed(42)

base_raw <- dpc5868::base_final |>
  dplyr::mutate(
    tipologia = stringr::str_to_lower(tipologia),
    ativo = stringr::str_extract(tipologia, "^[a-z]+"),
    ativo = dplyr::if_else(ativo == "lh", 1, 0),
    passivo = stringr::str_extract(tipologia, "[a-z]+$"),
    passivo = dplyr::if_else(passivo == "lh", 1, 0)
  )

# base 1: extinto (1) vs nao extinto (0) -- todos os processos
base_extinto <- base_raw |>
  dplyr::mutate(
    extinto = dplyr::if_else(resultado == "Extinção", 1L, 0L)
  ) |>
  dplyr::group_by(tipologia) |>
  dplyr::ungroup()

# base 2: procedente (1) vs improcedente (0) -- exclui extintos
base_resultado <- base_raw |>
  dplyr::filter(resultado != "Extinção") |>
  dplyr::mutate(
    resultado = dplyr::case_when(
      resultado %in% c("Parcialmente procedente", "Totalmente procedente") ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  dplyr::group_by(tipologia) |>
  dplyr::ungroup()

# checks -----------------------------------------------------------------

base_extinto |> dplyr::count(tipologia)
base_resultado |> dplyr::count(tipologia)

# regressoes -----------------------------------------------------

e1 <- glm(extinto ~ ativo * passivo, data = base_extinto, family = binomial)
m1 <- glm(resultado ~ ativo * passivo, data = base_resultado, family = binomial)

# tabela 2x2 --------------------------------------------------------------

novos_dados <- tidyr::expand_grid(
  ativo = c(0, 1),
  passivo = c(0, 1)
)

# tabela de probabilidades preditas -----------------------------------------------------------------

calcular_probs <- function(modelo, ic_method = c("conservador", "delta")) {
  ic_method <- match.arg(ic_method)
  X <- model.matrix(~ ativo * passivo, data = novos_dados)
  V <- vcov(modelo)
  b <- coef(modelo)
  prob <- plogis(as.vector(X %*% b))
  se_fn <- function(grad) sqrt(as.numeric(t(grad) %*% V %*% grad))
  se_dm <- function(pesos) se_fn(colSums(pesos * (prob * (1 - prob)) * X))
  intervalo <- function(p, se_p) c(p - 1.96 * se_p, p + 1.96 * se_p)

  formatar <- function(p, lo, hi) {
    paste0(
      formattable::percent(p),
      "\n[",
      formattable::percent(lo),
      "; ",
      formattable::percent(hi),
      "]"
    )
  }

  if (ic_method == "delta") {
    se <- purrr::map_dbl(seq_len(nrow(X)), \(i) {
      se_fn(as.vector(X[i, ]) * prob[i] * (1 - prob[i]))
    })
    tibble::tibble(
      analise = NA_character_,
      passivo = c("OS", "LH"),
      ativo_OS = c(
        formatar(prob[1], prob[1] - 1.96 * se[1], prob[1] + 1.96 * se[1]),
        formatar(prob[3], prob[3] - 1.96 * se[3], prob[3] + 1.96 * se[3])
      ),
      ativo_LH = c(
        formatar(prob[2], prob[2] - 1.96 * se[2], prob[2] + 1.96 * se[2]),
        formatar(prob[4], prob[4] - 1.96 * se[4], prob[4] + 1.96 * se[4])
      )
    )
  } else {
    base_ic <- intervalo(
      prob[1],
      se_fn(as.vector(X[1, ]) * prob[1] * (1 - prob[1]))
    )
    ativo_ic <- intervalo(prob[2] - prob[1], se_dm(c(-1, 1, 0, 0)))
    passivo_ic <- intervalo(prob[3] - prob[1], se_dm(c(-1, 0, 1, 0)))
    lhlh_ic <- intervalo(
      prob[4] - prob[2] - prob[3] + prob[1],
      se_dm(c(1, -1, -1, 1))
    )
    tibble::tibble(
      analise = NA_character_,
      passivo = c("OS", "LH"),
      ativo_OS = c(
        formatar(prob[1], base_ic[1], base_ic[2]),
        formatar(
          prob[3],
          base_ic[1] + passivo_ic[1],
          base_ic[2] + passivo_ic[2]
        )
      ),
      ativo_LH = c(
        formatar(prob[2], base_ic[1] + ativo_ic[1], base_ic[2] + ativo_ic[2]),
        formatar(
          prob[4],
          base_ic[1] + ativo_ic[1] + passivo_ic[1] + lhlh_ic[1],
          base_ic[2] + ativo_ic[2] + passivo_ic[2] + lhlh_ic[2]
        )
      )
    )
  }
}

dplyr::bind_rows(
  calcular_probs(e1) |>
    dplyr::mutate(
      analise = dplyr::if_else(
        dplyr::row_number() == 1,
        "Extinção",
        NA_character_
      )
    ),
  calcular_probs(m1) |>
    dplyr::mutate(
      analise = dplyr::if_else(
        dplyr::row_number() == 1,
        "Procedência",
        NA_character_
      )
    )
) |>
  flextable::flextable() |>
  flextable::set_header_labels(
    analise = "",
    passivo = "Réu",
    ativo_OS = "OS",
    ativo_LH = "LH"
  ) |>
  flextable::add_header_row(
    values = c("", "", "Autor"),
    colwidths = c(1, 1, 2)
  ) |>
  flextable::hline(i = 2) |>
  flextable::align(align = "center", part = "all") |>
  flextable::autofit()

# tabelas de efeitos marginais ------------------------------------------------------

calcular_deltas <- function(modelo) {
  X <- model.matrix(~ ativo * passivo, data = novos_dados)
  V <- vcov(modelo)
  b <- coef(modelo)
  prob <- plogis(as.vector(X %*% b))
  se_fn <- function(grad) sqrt(as.numeric(t(grad) %*% V %*% grad))
  se_dm <- function(pesos) se_fn(colSums(pesos * (prob * (1 - prob)) * X))

  deltas <- c(
    prob[2] - prob[1],
    prob[3] - prob[1],
    prob[4] - prob[2] - prob[3] + prob[1]
  )
  ses <- c(se_dm(c(-1, 1, 0, 0)), se_dm(c(-1, 0, 1, 0)), se_dm(c(1, -1, -1, 1)))
  pvals <- 2 * pnorm(-abs(deltas / ses))

  tibble::tibble(
    analise = NA_character_,
    efeito = c(
      "LH no polo ativo",
      "LH no polo passivo",
      "LH em ambos os polos"
    ),
    `Efeito marginal` = paste0(
      formattable::percent(deltas),
      "\n[",
      formattable::percent(deltas - 1.96 * ses),
      "; ",
      formattable::percent(deltas + 1.96 * ses),
      "]"
    ),
    `p-valor` = dplyr::if_else(
      pvals < 0.001,
      "<0.001",
      as.character(round(pvals, 3))
    )
  )
}

dplyr::bind_rows(
  calcular_deltas(e1) |>
    dplyr::mutate(
      analise = dplyr::if_else(
        dplyr::row_number() == 1,
        "Extinção",
        NA_character_
      )
    ),
  calcular_deltas(m1) |>
    dplyr::mutate(
      analise = dplyr::if_else(
        dplyr::row_number() == 1,
        "Procedência",
        NA_character_
      )
    )
) |>
  flextable::flextable() |>
  flextable::set_header_labels(analise = "", efeito = "") |>
  flextable::hline(i = 3) |>
  flextable::align(j = 3:4, align = "center", part = "all") |>
  flextable::autofit()

# plots ------------------------------------------------------------------

fazer_grafico <- function(modelo, base) {
  X <- model.matrix(~ ativo * passivo, data = novos_dados)
  V <- vcov(modelo)
  b <- coef(modelo)
  prob <- plogis(as.vector(X %*% b))
  se_fn <- function(grad) sqrt(as.numeric(t(grad) %*% V %*% grad))
  se_dm <- function(pesos) se_fn(colSums(pesos * (prob * (1 - prob)) * X))

  deltas <- c(
    prob[2] - prob[1],
    prob[3] - prob[1],
    prob[4] - prob[2] - prob[3] + prob[1]
  )
  ses <- c(se_dm(c(-1, 1, 0, 0)), se_dm(c(-1, 0, 1, 0)), se_dm(c(1, -1, -1, 1)))

  ns <- base |>
    dplyr::mutate(
      tipologia = dplyr::case_when(
        ativo == 1 & passivo == 0 ~ "LH no polo ativo",
        ativo == 0 & passivo == 1 ~ "LH no polo passivo",
        ativo == 1 & passivo == 1 ~ "LH em ambos os polos"
      )
    ) |>
    dplyr::filter(!is.na(tipologia)) |>
    dplyr::count(tipologia)

  labels <- c(
    "LH no polo ativo",
    "LH no polo passivo",
    "LH em ambos os polos"
  ) |>
    purrr::map_chr(\(e) {
      n <- ns$n[ns$tipologia == e]
      paste0(e, " (n=", scales::comma(n), ")")
    })

  tibble::tibble(
    efeito = factor(labels, levels = rev(labels)),
    delta = deltas,
    ic_inf = deltas - 1.96 * ses,
    ic_sup = deltas + 1.96 * ses
  ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = delta,
      y = efeito,
      xmin = ic_inf,
      xmax = ic_sup
    )) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_pointrange() +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

fazer_grafico(e1, base_extinto)
fazer_grafico(m1, base_resultado)
