# preparacao -------------------------------------------------------------

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

# analise exploratoria ---------------------------------------------------

# partes_full |>
#     excluir = dplyr::case_when(
#       deComptipoparte == "Requerente" & tpPessoafisjur == "F" ~ TRUE,
#       deComptipoparte == "Requerido" & tpPessoafisjur == "F" ~ TRUE,
#       TRUE ~ FALSE
#     )
#   ) |>
#   dplyr::filter(
#     deComptipoparte %in% c("Requerido", "Requerente"),
#     !excluir
#   ) |>
#   dplyr::group_split(deComptipoparte) |>
#   purrr::pluck(1) |>
#   dplyr::arrange(cd_processo) |>
#   dplyr::distinct(nmPessoa) |>
#   dplyr::arrange(nmPessoa) |>
#   dplyr::pull(nmPessoa)

# codificação parcial ------------------------------------------------------

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

cod_parcial <- partes_full |>
  # 1) Exclui o que tem PF x PF
  dplyr::filter(deComptipoparte %in% c(tipo_ativo, tipo_passivo)) |>
  dplyr::group_by(cd_processo) |>
  dplyr::filter(!all(tpPessoafisjur == "F")) |>
  dplyr::ungroup() |>
  # 2) Excluir o que só tem 1 parte dps dos filtros. São casos com herdeiro, por exemplo
  dplyr::group_by(cd_processo) |>
  dplyr::mutate(n = dplyr::n()) |>
  dplyr::filter(n > 1) |>
  dplyr::ungroup() |>
  # 3) Codifica
  dplyr::select(cd_processo, deComptipoparte, nmPessoa) |>
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
    galanter = dplyr::case_when(
      tipo_empresa %in% c("ME", "EPP", "LTDA", "EIRELI") ~ "OS",
      tipo_empresa %in% c("SA") ~ "LH",
      TRUE ~ "OS"
    ),
    galanter_ativo = dplyr::if_else(
      condition = deComptipoparte %in% tipo_ativo,
      true = galanter,
      false = NA_character_
    ),
    galanter_passivo = dplyr::if_else(
      condition = deComptipoparte %in% tipo_passivo,
      true = galanter,
      false = NA_character_
    ),
    duvida = tipo_empresa == "LTDA",
    duvida = tidyr::replace_na(duvida, FALSE)
  )


# codificação final -------------------------------------------------------------

tipologia <- cod_parcial |>
  dplyr::mutate(
    ltda_ativo = deComptipoparte %in% tipo_ativo & tipo_empresa %in% "LTDA",
    sa_ativo = deComptipoparte %in% tipo_ativo & tipo_empresa %in% "SA",
    ltda_passivo = deComptipoparte %in% tipo_passivo & tipo_empresa %in% "LTDA",
    sa_passivo = deComptipoparte %in% tipo_passivo & tipo_empresa %in% "SA"
  ) |>
  dplyr::group_by(cd_processo) |>
  dplyr::summarise(
    galanter_ativo = dplyr::case_when(
      any(galanter_ativo == "LH", na.rm = TRUE) ~ "LH",
      all(galanter_ativo == "OS", na.rm = TRUE) ~ "OS"
    ),
    galanter_passivo = dplyr::case_when(
      any(galanter_passivo == "LH", na.rm = TRUE) ~ "LH",
      all(galanter_passivo == "OS", na.rm = TRUE) ~ "OS"
    ),
    tipologia = glue::glue("{galanter_ativo} x {galanter_passivo}"),
    duvida = (any(ltda_ativo) & !any(sa_ativo)) |
      (any(ltda_passivo) & !any(sa_passivo))
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    readr::read_csv("data-raw/csv/cjpg_empresarial/capa.csv") |>
      dplyr::select(processo, cd_processo),
    by = "cd_processo"
  ) |>
  dplyr::select(
    processo,
    cd_processo,
    tipologia,
    duvida
  )

usethis::use_data(tipologia, overwrite = TRUE)

# join --------------------------------------------------------------------

assuntos <- readr::read_csv("data-raw/csv/cjpg_empresarial/capa.csv") |>
  dplyr::select(processo, cd_processo, classe, assunto) |>
  dplyr::inner_join(cod_final, by = "cd_processo") |>
  dplyr::count(assunto, tipologia) |>
  dplyr::count(assunto) |>
  dplyr::filter(n == 4) |>
  dplyr::filter(
    !stringr::str_detect(
      assunto,
      stringr::regex(
        "credor|falência|recuperação|arbitral|arbitragem|crédito",
        TRUE
      )
    )
  ) |>
  dplyr::pull(assunto)

readr::read_csv("data-raw/csv/cjpg_empresarial/capa.csv") |>
  dplyr::select(processo, cd_processo, classe, assunto) |>
  dplyr::inner_join(cod_final, by = "cd_processo") |>
  dplyr::filter(assunto %in% assuntos) |>
  dplyr::count(tipologia)
