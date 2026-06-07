# preparacao -------------------------------------------------------------

flextable::set_flextable_defaults(
  decimal.mark = ",",
  big.mark = "."
)

# tabela ------------------------------------------------------------------

excluir <- c(
  "Recuperação judicial e Falência",
  "Classificação de créditos",
  "Falência decretada",
  "Pedido de falência",
  "Autofalência",
  "Recuperação extrajudicial",
  "Convolação de recuperação judicial em falência"
)

readr::read_csv("data-raw/csv/cjpg_empresarial/capa.csv") |>
  dplyr::filter(assunto %in% excluir) |>
  dplyr::count(assunto, sort = TRUE) |>
  janitor::adorn_totals() |>
  flextable::flextable() |>
  flextable::set_header_labels(assunto = "Assunto", n = "N") |>
  flextable::bold(part = "header") |>
  flextable::hline(i = length(excluir)) |>
  flextable::autofit()
