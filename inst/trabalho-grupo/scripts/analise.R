da |>
  dplyr::mutate(
    resultado = dplyr::case_when(
      stringr::str_detect(
        julgado,
        stringr::regex("indeferido|indeferimento|improcedente", TRUE)
      ) ~ "Improcedente",
      stringr::str_detect(
        julgado,
        stringr::regex("parcial", TRUE)
      ) ~ "Parcialmente procedente",
      stringr::str_detect(
        julgado,
        stringr::regex("deferido|deferimento|procedente", TRUE)
      ) ~ "Totalmente procedente"
    )
  ) |>
  dplyr::count(foro, resultado) |>
  tidyr::complete(foro, resultado) |>
  dplyr::filter(!is.na(resultado)) |>
  dplyr::mutate(
    n = tidyr::replace_na(n, 0)
  ) |>
  dplyr::group_by(foro) |>
  dplyr::mutate(
    prop = n / sum(n),
    prop = formattable::percent(prop)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(resultado == "Totalmente procedente") |>
  dplyr::arrange(desc(prop)) |>
  ggplot2::ggplot() +
  ggplot2::aes(y = foro, x = prop) +
  ggplot2::geom_col()
