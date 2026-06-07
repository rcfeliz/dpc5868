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

# tabela ------------------------------------------------------------------

dplyr::bind_rows(
  base_extinto |>
    dplyr::count(tipologia) |>
    dplyr::mutate(
      base = glue::glue("Extinção\n(N = {scales::comma(sum(n))})"),
      prop = formattable::percent(n / sum(n))
    ) |>
    dplyr::select(base, tipologia, n, prop),
  base_resultado |>
    dplyr::count(tipologia) |>
    dplyr::mutate(
      base = glue::glue("Procedência\n(N = {scales::comma(sum(n))})"),
      prop = formattable::percent(n / sum(n))
    ) |>
    dplyr::select(base, tipologia, n, prop)
) |>
  dplyr::mutate(
    tipologia = dplyr::case_when(
      tipologia == "os x os" ~ "OS x OS",
      tipologia == "os x lh" ~ "OS x LH",
      tipologia == "lh x os" ~ "LH x OS",
      tipologia == "lh x lh" ~ "LH x LH"
    ),
    tipologia = factor(tipologia, levels = c("OS x OS", "LH x OS", "OS x LH", "LH x LH"))
  ) |>
  dplyr::arrange(base, tipologia) |>
  dplyr::mutate(base = dplyr::if_else(!duplicated(base), base, NA_character_)) |>
  flextable::flextable() |>
  flextable::set_header_labels(
    base      = "Base",
    tipologia = "Tipologia",
    n         = "N",
    prop      = "%"
  ) |>
  flextable::bold(part = "header") |>
  flextable::hline(i = 4) |>
  flextable::autofit()
