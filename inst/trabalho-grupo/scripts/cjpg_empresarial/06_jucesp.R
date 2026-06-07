# configuracao ------------------------------------------------------------

pasta <- here::here("data-raw", "html", "jucesp")

path_csv <- here::here("data-raw/csv/cjpg_empresarial")
repo <- "rcfeliz/dpc5868"
tag <- "cjpg_empresarial"
piggyback::pb_download(
  "partes_full.csv",
  repo = repo,
  tag = tag,
  dest = path_csv
)

ss_id <- "1Q17BohMxjtcVK88xv4npNOCSxZ299xBkanT9iS_NuLE"
classificadores <- c("feliz")

googlesheets4::gs4_auth("ric.feliz@gmail.com")

# cnpjs -------------------------------------------------------------------

processos_sample <- classificadores |>
  purrr::map(\(aba) {
    googlesheets4::read_sheet(ss_id, sheet = aba, col_types = "c")
  }) |>
  purrr::list_rbind() |>
  dplyr::pull(processo) |>
  unique()

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

cnpjs <- readr::read_csv("data-raw/csv/cjpg_empresarial/partes_full.csv") |>
  dplyr::left_join(
    readr::read_csv("data-raw/csv/cjpg_empresarial/capa.csv") |>
      dplyr::select(processo, cd_processo),
    by = "cd_processo"
  ) |>
  dplyr::filter(processo %in% processos_sample) |>
  dplyr::filter(deComptipoparte %in% c(tipo_ativo, tipo_passivo)) |>
  dplyr::select(processo, deComptipoparte, nmPessoa, nuCpfcnpj) |>
  dplyr::mutate(
    tipo_empresa = dplyr::case_when(
      stringr::str_detect(nmPessoa, stringr::regex("me$", TRUE)) ~ "ME",
      stringr::str_detect(nmPessoa, stringr::regex("epp$", TRUE)) ~ "EPP",
      stringr::str_detect(
        nmPessoa,
        stringr::regex("ltda|ltda?$", TRUE)
      ) ~ "LTDA",
      stringr::str_detect(
        nmPessoa,
        stringr::regex("eirel?li|eir$", TRUE)
      ) ~ "EIRELI",
      stringr::str_detect(nmPessoa, stringr::regex(" s.? ?a.?$", TRUE)) ~ "SA"
    ),
    nuCpfcnpj = nuCpfcnpj |>
      stringr::str_remove_all("[:punct:]")
  ) |>
  dplyr::filter(tipo_empresa == "LTDA", !is.na(nuCpfcnpj)) |>
  dplyr::distinct(nuCpfcnpj) |>
  dplyr::anti_join(
    fs::dir_ls(pasta) |>
      basename() |>
      stringr::str_remove_all("\\.html") |>
      tibble::as_tibble(),
    by = dplyr::join_by("nuCpfcnpj" == "value")
  ) |>
  dplyr::pull(nuCpfcnpj) |>
  unique()

# teste ------------------------------------------------------------------

for (cnpj in cnpjs) {
  dpc5868::jucesp_download(cnpj, pasta = pasta)

  arquivos <- fs::dir_ls(pasta, glob = "*.html")
  if (length(arquivos) > 0) {
    ultimo <- arquivos[length(arquivos)]
    dados_teste <- dpc5868::jucesp_parse(ultimo)
    if (all(is.na(dados_teste$nire) | dados_teste$nire == "")) {
      cat("IP bloqueado, parando\n")
      break
    }
  }
}

# download + parse em lotes de 200 ----------------------------------------

lotes <- split(cnpjs, ceiling(seq_along(cnpjs) / 200))

for (i in seq_along(lotes)) {
  path_rds <- here::here("data-raw", "rds", paste0("jucesp", i, ".rds"))

  if (fs::file_exists(path_rds)) {
    next
  }

  purrr::walk(lotes[[i]], dpc5868::jucesp_download, pasta = pasta)

  arquivos <- fs::dir_ls(pasta, glob = "*.html")

  dados <- purrr::map(arquivos, dpc5868::jucesp_parse) |>
    dplyr::bind_rows()

  readr::write_rds(dados, path_rds)

  gc()
}

# join de todos os lotes --------------------------------------------------

jucesp <- fs::dir_ls(here::here("data-raw", "rds"), glob = "*jucesp*.rds") |>
  purrr::map(readr::read_rds) |>
  dplyr::bind_rows()

readr::write_rds(jucesp, here::here("data-raw", "rds", "jucesp.rds"))
