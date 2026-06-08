# preparacao -------------------------------------------------------------

flextable::set_flextable_defaults(
  decimal.mark = ",",
  big.mark = "."
)

cod_parcial <- readr::read_csv("data-raw/csv/cjpg_acps/partes_full.csv") |>
  dplyr::mutate(
    excluir = dplyr::case_when(
      deComptipoparte == "Requerente" & tpPessoafisjur == "F" ~ TRUE,
      deComptipoparte == "Requerido" & tpPessoafisjur == "F" ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::filter(
    deComptipoparte %in% c("Requerido", "Requerente"),
    !excluir
  ) |>
  dplyr::mutate(
    galanter_ativo = dplyr::case_when(
      stringr::str_detect(
        nmPessoa,
        stringr::regex(
          "minist[ée]rio p[uú]blico|defensoria p[úu]blica|procuradoria|justiça",
          TRUE
        )
      ) ~ "LH",
      stringr::str_detect(
        nmPessoa,
        stringr::regex("^(munic[ií]pio|prefeitura)", TRUE)
      ) ~ "LE",
      stringr::str_detect(
        nmPessoa,
        stringr::regex("^(estado|fazenda)", TRUE)
      ) ~ "LH",
      stringr::str_detect(nmPessoa, stringr::regex("sind", TRUE)) &
        stringr::str_detect(
          nmPessoa,
          stringr::regex("est|federal", TRUE)
        ) ~ "LH",
      stringr::str_detect(nmPessoa, stringr::regex("sind", TRUE)) ~ "LE",
      stringr::str_detect(
        nmPessoa,
        stringr::regex("associa[cç]|centro d. professorado", TRUE)
      ) &
        stringr::str_detect(
          nmPessoa,
          stringr::regex("est|federal", TRUE)
        ) ~ "LH",
      stringr::str_detect(
        nmPessoa,
        stringr::regex("associa[cç]|centro d. professorado", TRUE)
      ) ~ "LE",
      stringr::str_detect(
        nmPessoa,
        stringr::regex(
          "conselho|união|fundação|instituto|federa[çc][ãa]o",
          TRUE
        )
      ) ~ "LH",
      stringr::str_detect(
        nmPessoa,
        stringr::regex(
          "acadêmico|comissão discente|uni[aã]o|sociedade de moradores da favela",
          TRUE
        )
      ) ~ "LE"
    ),
    galanter_passivo = dplyr::case_when(
      stringr::str_detect(
        nmPessoa,
        stringr::regex(
          "minist[ée]rio p[uú]blico|defensoria p[úu]blica|procuradoria|justiça",
          TRUE
        )
      ) ~ "LH",
      stringr::str_detect(
        nmPessoa,
        stringr::regex("^(munic[ií]pio|prefeitura)", TRUE)
      ) ~ "OS",
      stringr::str_detect(
        nmPessoa,
        stringr::regex("^(estado|fazenda)", TRUE)
      ) ~ "LH",
      stringr::str_detect(nmPessoa, stringr::regex("sind", TRUE)) &
        stringr::str_detect(
          nmPessoa,
          stringr::regex("est|federal", TRUE)
        ) ~ "LH",
      stringr::str_detect(nmPessoa, stringr::regex("sind", TRUE)) ~ "OS",
      stringr::str_detect(
        nmPessoa,
        stringr::regex("associa[cç]|centro d. professorado", TRUE)
      ) &
        stringr::str_detect(
          nmPessoa,
          stringr::regex("est|federal", TRUE)
        ) ~ "LH",
      stringr::str_detect(
        nmPessoa,
        stringr::regex("associa[cç]|centro d. professorado", TRUE)
      ) ~ "OS",
      stringr::str_detect(
        nmPessoa,
        stringr::regex(
          "tim celular|oi móvel|claro|embratel|vivo|santander|bradesco|nubank|universidad|nupagamento|caixa econômica|itaú|mercado pago|picpay|google|amil|notre dame|olx",
          TRUE
        )
      ) ~ "LH",
      TRUE ~ "OS"
    )
  ) |>
  dplyr::filter(!is.na(galanter_ativo)) |>
  dplyr::distinct(
    cd_processo,
    deComptipoparte,
    nmPessoa,
    galanter_ativo,
    galanter_passivo
  )

# tabela ------------------------------------------------------------------

cod_parcial |>
  dplyr::group_by(cd_processo) |>
  dplyr::summarise(
    galanter_ativo = any(galanter_ativo == "LH"),
    galanter_passivo = any(galanter_passivo == "LH"),
    galanter_ativo = dplyr::if_else(galanter_ativo, "LH", "LE"),
    galanter_passivo = dplyr::if_else(galanter_passivo, "LH", "LE")
  ) |>
  dplyr::transmute(
    cd_processo,
    tipologia = glue::glue("{galanter_ativo} x {galanter_passivo}")
  ) |>
  dplyr::ungroup() |>
  dplyr::count(tipologia) |>
  janitor::adorn_totals() |>
  flextable::flextable() |>
  flextable::set_header_labels(values = c("Tipologia", "N")) |>
  flextable::bold(part = "header") |>
  flextable::hline(4)
