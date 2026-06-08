# preparacao -------------------------------------------------------------

flextable::set_flextable_defaults(
  decimal.mark = ",",
  big.mark = "."
)

base_extinto <- dpc5868::base_final |>
  dplyr::mutate(extinto = dplyr::if_else(resultado == "Extinção", 1L, 0L))

base_resultado <- dpc5868::base_final |>
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
    dplyr::count(
      classe = dplyr::if_else(extinto == 1L, "Sim", "Não")
    ) |>
    dplyr::mutate(
      base = glue::glue(
        "Extinto\nsem resolução\ndo mérito\n(N = {scales::comma(sum(n))})"
      ),
      prop = formattable::percent(n / sum(n))
    ),
  base_resultado |>
    dplyr::count(
      classe = dplyr::if_else(resultado == 1L, "Sim", "Não")
    ) |>
    dplyr::mutate(
      base = glue::glue("Procedente\nno mérito\n(N = {scales::comma(sum(n))})"),
      prop = formattable::percent(n / sum(n))
    )
) |>
  dplyr::mutate(
    base = dplyr::if_else(!duplicated(base), base, NA_character_)
  ) |>
  dplyr::select(base, classe, n, prop) |>
  flextable::flextable() |>
  flextable::set_header_labels(
    base = "Base",
    classe = "Resultado",
    n = "N",
    prop = "%"
  ) |>
  flextable::bold(part = "header") |>
  flextable::hline(i = 2) |>
  flextable::autofit()
