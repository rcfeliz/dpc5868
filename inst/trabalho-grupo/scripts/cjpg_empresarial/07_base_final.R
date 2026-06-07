# preparacao -------------------------------------------------------------

path_csv <- here::here("data-raw/csv/cjpg_empresarial")
fs::dir_create(path_csv)
repo <- "rcfeliz/dpc5868"
tag <- "cjpg_empresarial"
piggyback::pb_download(
  "movimentacoes.csv",
  repo = repo,
  tag = tag,
  dest = path_csv
)
piggyback::pb_download(
  "cjpg.csv",
  repo = repo,
  tag = tag,
  dest = path_csv
)

# aux_tipologia ---------------------------------------------------------------

ss_id <- "1Q17BohMxjtcVK88xv4npNOCSxZ299xBkanT9iS_NuLE"
classificadores <- c("andressa", "debora", "feliz", "samille")

googlesheets4::gs4_auth("ric.feliz@gmail.com")

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

aux_tipologia <- classificadores |>
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
          dplyr::filter(assunto %in% excluir) |>
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
  )

# aux_resultado ---------------------------------------------------------------

processos_alvo <- aux_tipologia$processo

aux_resultado <- readr::read_csv(
  fs::path(path_csv, "movimentacoes.csv"),
  col_types = readr::cols(
    processo = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(processo %in% processos_alvo) |>
  projetos::extract_sentenca() |>
  dplyr::select(processo, resultado)

# join -------------------------------------------------------------------

base_final <- aux_tipologia |>
  dplyr::left_join(
    aux_resultado,
    by = "processo"
  ) |>
  dplyr::left_join(
    readr::read_csv(fs::path(path_csv, "capa.csv")) |>
      dplyr::select(processo, foro, vara, juiz),
    by = "processo"
  ) |>
  dplyr::filter(!is.na(resultado))

usethis::use_data(base_final, overwrite = TRUE)
