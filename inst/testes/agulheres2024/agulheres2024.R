df <- readxl::read_xlsx("inst/testes/agulheres2024/dataframe_paper.xlsx")

df |>
  dplyr::count(union, dismiss, lawsuit) |>
  dplyr::transmute(
    union = ifelse(
      union == 1,
      "Sim",
      "Não"
    ),
    dismiss = ifelse(
      dismiss == 1,
      "Demitido",
      "Pediu demissão"
    ),
    lawsuit = ifelse(
      lawsuit == 1,
      "Sim",
      "Não"
    ),
    n
  ) |>
  dplyr::arrange(desc(union)) |>
  janitor::adorn_totals() |>
  flextable::flextable() |>
  flextable::set_header_labels(
    values = c("Sindicalizado", "Tipo de terminação", "Ajuizou ação", "N")
  ) |>
  flextable::bold(part = "header") |>
  flextable::hline(8)
