# preparacao -------------------------------------------------------------

flextable::set_flextable_defaults(
  decimal.mark = ",",
  big.mark = "."
)

base_raw <- dpc5868::base_final |>
  dplyr::mutate(
    tipologia = stringr::str_to_lower(tipologia),
    ativo = stringr::str_extract(tipologia, "^[a-z]+"),
    ativo = dplyr::if_else(ativo == "lh", 1L, 0L),
    passivo = stringr::str_extract(tipologia, "[a-z]+$"),
    passivo = dplyr::if_else(passivo == "lh", 1L, 0L)
  ) |>
  dplyr::mutate(
    tipologia = dplyr::case_when(
      tipologia == "le x le" ~ "LE x LE",
      tipologia == "le x lh" ~ "LE x LH",
      tipologia == "lh x le" ~ "LH x LE",
      tipologia == "lh x lh" ~ "LH x LH"
    ),
    tipologia = factor(
      tipologia,
      levels = c("LE x LE", "LH x LE", "LE x LH", "LH x LH")
    )
  )

base_extinto <- base_raw |>
  dplyr::mutate(
    classe = dplyr::if_else(resultado == "Extinção", "Extinto", "Não extinto")
  )

base_resultado <- base_raw |>
  dplyr::filter(resultado != "Extinção") |>
  dplyr::mutate(
    classe = dplyr::case_when(
      resultado %in%
        c("Parcialmente procedente", "Totalmente procedente") ~ "Procedente",
      TRUE ~ "Improcedente"
    )
  )

# tabelas ----------------------------------------------------------------

ft_extinto <- base_extinto |>
  dplyr::count(tipologia, classe) |>
  dplyr::group_by(tipologia) |>
  dplyr::mutate(prop = formattable::percent(n / sum(n))) |>
  dplyr::ungroup() |>
  dplyr::arrange(tipologia, classe) |>
  dplyr::mutate(
    tipologia = dplyr::if_else(
      !duplicated(tipologia),
      as.character(tipologia),
      NA_character_
    )
  ) |>
  flextable::flextable() |>
  flextable::set_header_labels(
    tipologia = "Tipologia",
    classe = "Resultado",
    n = "N",
    prop = "%"
  ) |>
  flextable::add_header_row(values = "Extinção", colwidths = 4) |>
  flextable::bold(part = "header") |>
  flextable::hline(i = c(2, 4, 6)) |>
  flextable::autofit()

ft_resultado <- base_resultado |>
  dplyr::count(tipologia, classe) |>
  dplyr::group_by(tipologia) |>
  dplyr::mutate(prop = formattable::percent(n / sum(n))) |>
  dplyr::ungroup() |>
  dplyr::arrange(tipologia, classe) |>
  dplyr::mutate(
    tipologia = dplyr::if_else(
      !duplicated(tipologia),
      as.character(tipologia),
      NA_character_
    )
  ) |>
  flextable::flextable() |>
  flextable::set_header_labels(
    tipologia = "Tipologia",
    classe = "Resultado",
    n = "N",
    prop = "%"
  ) |>
  flextable::add_header_row(values = "Procedência", colwidths = 4) |>
  flextable::bold(part = "header") |>
  flextable::hline(i = c(2, 4, 6)) |>
  flextable::autofit()

ft_extinto
ft_resultado
