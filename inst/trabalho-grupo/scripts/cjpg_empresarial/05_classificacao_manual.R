# configuracao ------------------------------------------------------------

set.seed(42)

ss_id <- "1Q17BohMxjtcVK88xv4npNOCSxZ299xBkanT9iS_NuLE"
classificadores <- c("andressa", "debora", "feliz", "samile")

# cada classificador recebe 100 processos de uma tipologia ----------------

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

grupos <- dpc5868::tipologia |>
  dplyr::left_join(
    readr::read_csv("data-raw/csv/cjpg_empresarial/partes_full.csv") |>
      dplyr::filter(deComptipoparte %in% c(tipo_ativo, tipo_passivo)) |>
      dplyr::mutate(
        polo = dplyr::case_when(
          deComptipoparte %in% tipo_ativo ~ "ativo",
          deComptipoparte %in% tipo_passivo ~ "passivo"
        )
      ) |>
      dplyr::group_by(cd_processo, polo) |>
      dplyr::summarise(
        nmPessoa = stringr::str_flatten(nmPessoa, collapse = "\n\n")
      ) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(
        values_from = nmPessoa,
        names_from = polo,
        names_prefix = "polo_"
      ),
    by = "cd_processo"
  ) |>
  dplyr::group_by(tipologia) |>
  dplyr::slice_sample(n = 100) |>
  dplyr::group_split(tipologia)

sheets_data <- purrr::set_names(grupos, classificadores)

# escrever abas no google sheets ------------------------------------------

googlesheets4::gs4_auth()

purrr::iwalk(sheets_data, \(dados, nome_aba) {
  googlesheets4::write_sheet(data = dados, ss = ss_id, sheet = nome_aba)
})

# adicionar dropdown em tipologia_correta (coluna E) ----------------------

ss_meta <- googlesheets4::gs4_get(ss_id)

adicionar_dropdown <- function(sheet_title) {
  sheet_id_num <- ss_meta$sheets |>
    purrr::keep(\(s) s$properties$title == sheet_title) |>
    purrr::pluck(1, "properties", "sheetId")

  body <- list(
    requests = list(
      list(
        setDataValidation = list(
          range = list(
            sheetId = sheet_id_num,
            startRowIndex = 1L,
            endRowIndex = 101L,
            startColumnIndex = 4L,
            endColumnIndex = 5L
          ),
          rule = list(
            condition = list(
              type = "ONE_OF_LIST",
              values = purrr::map(tipologias_validas, \(v) {
                list(userEnteredValue = v)
              })
            ),
            showCustomUi = TRUE,
            strict = TRUE
          )
        )
      )
    )
  )

  req <- gargle::request_build(
    method = "POST",
    path = "v4/spreadsheets/{spreadsheetId}:batchUpdate",
    params = list(spreadsheetId = ss_id),
    body = body,
    base_url = "https://sheets.googleapis.com/",
    token = googlesheets4::gs4_token()
  )
  gargle::request_make(req)
}

purrr::walk(classificadores, adicionar_dropdown)
