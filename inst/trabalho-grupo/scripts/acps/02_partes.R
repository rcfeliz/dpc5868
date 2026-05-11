gmailr::gm_auth("ric.feliz@gmail.com")
tjsp::autenticar(email_provider = "gmail")

path <- here::here("data-raw/html/cpopg/")
fs::dir_create(path)

load("data/acps_2020.rda")

tjsp::tjsp_baixar_partes_docs(acps_2020$cd_processo, diretorio = path)

files <- fs::dir_ls(path)

partes <- tjsp::tjsp_ler_cpopg_partes_docs(
  arquivos = files
) |>
  tibble::as_tibble()

lista_partes <- partes |>
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
      ) ~ "OS"
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
  # dplyr::filter(deComptipoparte == "Requerido") |>
  # dplyr::distinct(nmPessoa, galanter_passivo) |>
  # dplyr::arrange(galanter_passivo)
  dplyr::distinct(
    cd_processo,
    deComptipoparte,
    nmPessoa,
    galanter_ativo,
    galanter_passivo
  )

lista_partes |>
  dplyr::group_by(cd_processo) |>
  dplyr::summarise(
    galanter_ativo = any(galanter_ativo == "LH"),
    galanter_passivo = any(galanter_passivo == "LH"),
    galanter_ativo = dplyr::if_else(galanter_ativo, "LH", "OS"),
    galanter_passivo = dplyr::if_else(galanter_passivo, "LH", "OS")
  ) |>
  dplyr::transmute(
    cd_processo,
    tipologia = glue::glue("{galanter_ativo} x {galanter_passivo}")
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(tipologia == "OS x LH")
dplyr::count(tipologia)

lista_partes |>
  dplyr::group_split(deComptipoparte) |>
  purrr::pluck(1) |>
  dplyr::arrange(cd_processo) |>
  dplyr::distinct(nmPessoa) |>
  dplyr::arrange(nmPessoa) |>
  dplyr::pull(nmPessoa)
