excluir <- c(
  "Recuperação judicial e Falência",
  "Classificação de créditos",
  "Falência decretada",
  "Pedido de falência",
  "Autofalência",
  "Recuperação extrajudicial",
  "Convolação de recuperação judicial em falência"
)

dpc5868::tipologia |>
  dplyr::inner_join(
    readr::read_csv("data-raw/csv/cjpg_empresarial/capa.csv") |>
      dplyr::filter(!(assunto %in% excluir)) |>
      dplyr::distinct(cd_processo) |>
      dplyr::left_join(
        readr::read_csv("data-raw/csv/cjpg_empresarial/partes_full.csv"),
        by = "cd_processo"
      ) |>
      dplyr::filter(deComptipoparte %in% c(tipo_ativo, tipo_passivo)) |>
      dplyr::group_by(cd_processo) |>
      dplyr::filter(!all(tpPessoafisjur == "F")) |>
      dplyr::ungroup() |>
      dplyr::group_by(cd_processo) |>
      dplyr::mutate(n = dplyr::n()) |>
      dplyr::filter(n > 1) |>
      dplyr::ungroup() |>
      dplyr::distinct(cd_processo),
    by = "cd_processo"
  ) |>
  dplyr::count(duvida) |>
  dplyr::mutate(duvida = dplyr::if_else(duvida, "Sim", "Não")) |>
  janitor::adorn_totals() |>
  flextable::flextable() |>
  flextable::set_header_labels(duvida = "Dúvida na classificação", n = "N") |>
  flextable::bold(part = "header") |>
  flextable::hline(2) |>
  flextable::autofit()
