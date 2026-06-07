# n_inicial --------------------------------------------------------------

n_inicial <- readr::read_csv("data-raw/csv/cjpg_empresarial/cjpg.csv") |>
  dplyr::distinct(processo) |>
  nrow()

# n_data -----------------------------------------------------------------

n_data <- readr::read_csv("data-raw/csv/cjpg_empresarial/cjpg.csv") |>
  dplyr::filter(disponibilizacao >= "2023-10-01") |>
  dplyr::distinct(processo) |>
  nrow()

# n_falencia -------------------------------------------------------------

excluir <- c(
  "Recuperação judicial e Falência",
  "Classificação de créditos",
  "Falência decretada",
  "Pedido de falência",
  "Autofalência",
  "Recuperação extrajudicial",
  "Convolação de recuperação judicial em falência"
)

n_falencia <- readr::read_csv("data-raw/csv/cjpg_empresarial/capa.csv") |>
  dplyr::filter(!(assunto %in% excluir)) |>
  dplyr::distinct(processo) |>
  nrow()

# n_pfpf -----------------------------------------------------------------

tipo_ativo <- c(
  "Requerente",
  "Exequente",
  "Embargante",
  "Autor",
  "Apelante"
)

tipo_passivo <- c(
  "Requerido",
  "Executado",
  "Embargado",
  "Réu",
  "Apelado"
)

n_pfpf <- readr::read_csv("data-raw/csv/cjpg_empresarial/capa.csv") |>
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
  dplyr::distinct(cd_processo) |>
  nrow()

# n_classificacao ------------------------------------------------------------------

ss_id <- "1Q17BohMxjtcVK88xv4npNOCSxZ299xBkanT9iS_NuLE"
classificadores <- c("andressa", "debora", "feliz", "samille")

googlesheets4::gs4_auth("ric.feliz@gmail.com")

n_classificacao <- classificadores |>
  purrr::map(\(aba) googlesheets4::read_sheet(ss_id, sheet = aba)) |>
  purrr::list_rbind() |>
  dplyr::select(
    processo,
    tipologia = tipologia_correta
  ) |>
  dplyr::filter(!is.na(tipologia)) |>
  dplyr::bind_rows(
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
      dplyr::filter(!duvida) |>
      dplyr::select(processo, tipologia)
  ) |>
  nrow()


# n_final ----------------------------------------------------------------

n_final <- dpc5868::base_final |>
  nrow()

# sankey -----------------------------------------------------------------

counts <- c(n_inicial, n_data, n_falencia, n_pfpf, n_classificacao, n_final)
titles <- c(
  "N inicial",
  "Filtro de data",
  "Filtro por assunto",
  "Filtro de PF x PF",
  "Classificados",
  "N final"
)

order_idx <- order(counts, decreasing = TRUE)
counts <- counts[order_idx]
titles <- titles[order_idx]

steps <- purrr::imap(counts, \(n_reach, i) {
  step_data <- tibble::tibble(id = seq_len(n_reach))
  for (j in seq_along(counts)) {
    step_data[[paste0("a", j)]] <- j <= i
  }
  step_data
})

sankey_data <- dplyr::bind_rows(steps) |>
  dplyr::mutate(id = dplyr::row_number()) |>
  tidyr::pivot_longer(
    cols = dplyr::contains("a"),
    names_to = "group",
    values_to = "stratum"
  )

step_labels <- setNames(titles, paste0("a", seq_along(counts)))

ggplot2::ggplot(
  dplyr::mutate(sankey_data, stratum = dplyr::if_else(stratum, "Sim", "Não"))
) +
  ggplot2::aes(
    x = group,
    stratum = stratum,
    alluvium = id,
    fill = stratum
  ) +
  ggalluvial::geom_flow() +
  ggalluvial::geom_stratum() +
  ggplot2::scale_fill_manual(values = c("#E69F00", "#0072B2")) +
  ggplot2::scale_x_discrete(
    labels = stringr::str_wrap(step_labels, 15)
  ) +
  ggplot2::labs(
    x = "Filtragem",
    y = "Quantidade de processos",
    fill = "Manteve no\nescopo?"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")
