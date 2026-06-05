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

# aux_tipologia ---------------------------------------------------------------

ss_id <- "1Q17BohMxjtcVK88xv4npNOCSxZ299xBkanT9iS_NuLE"
classificadores <- c("andressa", "debora", "feliz", "samille")

googlesheets4::gs4_auth("ric.feliz@gmail.com")

aux_tipologia <- classificadores |>
  purrr::map(\(aba) googlesheets4::read_sheet(ss_id, sheet = aba)) |>
  purrr::list_rbind() |>
  dplyr::select(
    processo,
    tipologia_automatica
  )

# aux_resultado ---------------------------------------------------------------

processos_alvo <- aux_tipologia$processo

aux_resultado <- arrow::open_dataset(
  fs::path(path_csv, "movimentacoes.csv"),
  format = "csv"
) |>
  dplyr::filter(processo %in% processos_alvo) |>
  dplyr::collect() |>
  projetos::extract_sentenca(movs_processo)

# join -------------------------------------------------------------------
