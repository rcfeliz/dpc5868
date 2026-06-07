# preparacao -------------------------------------------------------------

flextable::set_flextable_defaults(
  decimal.mark = ",",
  big.mark = "."
)

# tabela ------------------------------------------------------------------

tibble::tibble(
  vara = c(
    "1ª, 2ª e 3ª Varas Empresariais e de Conflitos de Arbitragem",
    "1ª e 2ª Varas Regionais da 1ª RAJ",
    "1ª e 2ª Varas Regionais da 1ª RAJ (jurisdição estendida)",
    "1ª Vara Regional em Campinas",
    "Vara Regional em São José do Rio Preto",
    "Vara Regional em Ribeirão Preto"
  ),
  data = c(
    "Jan/2017",
    "Dez/2019",
    "Set/2022",
    "Mai/2023",
    "Ago/2023",
    "Out/2023"
  ),
  cobertura = c(
    "Capital (Foro Central Cível)",
    "Grande São Paulo",
    "Inclui 7ª RAJ (Santos) e 9ª RAJ (São José dos Campos)",
    "4ª RAJ (Campinas) e 10ª RAJ (Sorocaba)",
    "8ª RAJ (São José do Rio Preto), 2ª RAJ (Araçatuba) e 5ª RAJ (Presidente Prudente)",
    "6ª RAJ (Ribeirão Preto) e 3ª RAJ (Bauru)"
  )
) |>
  flextable::flextable() |>
  flextable::set_header_labels(
    data = "Data de criação",
    vara = "Vara",
    cobertura = "Cobertura"
  ) |>
  flextable::bold(part = "header") |>
  flextable::autofit()
