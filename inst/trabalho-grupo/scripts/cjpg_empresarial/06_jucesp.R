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
partes_full <- readr::read_csv("data-raw/csv/cjpg_empresarial/partes_full.csv")

# cnpjs -------------------------------------------------------------------

set.seed(42)

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

cnpjs <- partes_full |>
  # 1) Exclui partes ligadas a falência e RJ
  dplyr::inner_join(
    readr::read_csv("data-raw/csv/cjpg_empresarial/capa.csv") |>
      dplyr::filter(
        !stringr::str_detect(
          assunto,
          stringr::regex(
            "credor|falência|recuperação|crédito",
            TRUE
          )
        )
      ) |>
      dplyr::distinct(cd_processo),
    by = "cd_processo"
  ) |>
  # 2) Exclui o que tem PF x PF
  dplyr::filter(deComptipoparte %in% c(tipo_ativo, tipo_passivo)) |>
  dplyr::group_by(cd_processo) |>
  dplyr::filter(!all(tpPessoafisjur == "F")) |>
  dplyr::ungroup() |>
  # 3) Excluir o que só tem 1 parte dps dos filtros. São casos com herdeiro, por exemplo
  dplyr::group_by(cd_processo) |>
  dplyr::mutate(n = dplyr::n()) |>
  dplyr::filter(n > 1) |>
  dplyr::ungroup() |>
  # 4) Codifica
  dplyr::select(cd_processo, deComptipoparte, nmPessoa, nuCpfcnpj) |>
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
    )
  ) |>
  dplyr::filter(tipo_empresa == "LTDA") |>
  dplyr::filter(!is.na(nuCpfcnpj)) |>
  dplyr::pull(nuCpfcnpj)

# teste ------------------------------------------------------------------

jucesp_download(
  cnpjs[2],
  pasta = pasta
)

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
