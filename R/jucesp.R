#' Download de ficha cadastral JUCESP por CNPJ
#'
#' Busca a empresa pelo CNPJ no site da JUCESP, extrai o NIRE do resultado
#' e baixa a ficha cadastral em `Pre_Visualiza.aspx?nire=<NIRE>`, salvando o
#' HTML em `pasta/<cnpj_sem_pontuacao>.html`. Se o arquivo já existir, pula.
#' Se a busca não retornar resultado, não salva nada.
#' Se surgir CAPTCHA, resolve via GPT (Azure OpenAI). Se GPT falhar, pede
#' o código no console.
#'
#' @param cnpj CNPJ da empresa como string (com ou sem pontuação)
#' @param pasta Caminho da pasta onde salvar o HTML
#' @returns Caminho do arquivo salvo, ou `NULL` se sem resultado (invisível)
#' @export
jucesp_download <- function(cnpj, pasta) {
  cnpj_limpo <- gsub("[^0-9]", "", cnpj)

  fs::dir_create(pasta)

  caminho <- fs::path(pasta, paste0(cnpj_limpo, ".html"))

  if (fs::file_exists(caminho)) {
    message("[jucesp] ", cnpj_limpo, " | arquivo ja existe, pulando")
    return(invisible(caminho))
  }

  message("[jucesp] ", cnpj_limpo, " | iniciando download")

  cookie_file <- tempfile(fileext = ".txt")
  on.exit(unlink(cookie_file), add = TRUE)

  resp_get <- httr2::request("https://www.jucesponline.sp.gov.br/ResultadoBusca.aspx") |>
    httr2::req_cookie_preserve(cookie_file) |>
    httr2::req_perform()

  html_get <- httr2::resp_body_html(resp_get)

  viewstate     <- rvest::html_element(html_get, "#__VIEWSTATE")          |> rvest::html_attr("value")
  eventval      <- rvest::html_element(html_get, "#__EVENTVALIDATION")    |> rvest::html_attr("value")
  viewstate_gen <- rvest::html_element(html_get, "#__VIEWSTATEGENERATOR") |> rvest::html_attr("value")

  message("[jucesp] ", cnpj_limpo, " | viewstate ok, fazendo busca")

  html_busca <- jucesp_buscar(cnpj_limpo, viewstate, eventval, viewstate_gen, cookie_file)

  captcha_panel <- rvest::html_element(html_busca, "#ctl00_cphContent_gdvResultadoBusca_pnlCaptcha")
  if (!is.na(captcha_panel)) {
    message("[jucesp] ", cnpj_limpo, " | captcha detectado, resolvendo")
    html_busca <- jucesp_resolver_captcha(cnpj_limpo, html_busca, cookie_file, pasta)
    message("[jucesp] ", cnpj_limpo, " | captcha resolvido, html de busca retornado")
  } else {
    message("[jucesp] ", cnpj_limpo, " | sem captcha")
  }

  nire <- rvest::html_element(
    html_busca,
    "#ctl00_cphContent_gdvResultadoBusca_gdvContent a[id*='lbtSelecionar']"
  ) |>
    rvest::html_text2()

  message("[jucesp] ", cnpj_limpo, " | nire extraido: '", nire, "'")

  if (is.na(nire) || nchar(trimws(nire)) == 0) {
    message("[jucesp] ", cnpj_limpo, " | nire vazio ou NA, abortando")
    return(invisible(NULL))
  }

  message("[jucesp] ", cnpj_limpo, " | buscando ficha cadastral (nire=", trimws(nire), ")")

  resp_detalhe <- httr2::request("https://www.jucesponline.sp.gov.br/Pre_Visualiza.aspx") |>
    httr2::req_url_query(nire = nire, idproduto = "") |>
    httr2::req_cookie_preserve(cookie_file) |>
    httr2::req_perform()

  tamanho <- length(httr2::resp_body_raw(resp_detalhe))
  message("[jucesp] ", cnpj_limpo, " | resposta recebida (", tamanho, " bytes), salvando em ", caminho)

  writeBin(httr2::resp_body_raw(resp_detalhe), caminho)

  message("[jucesp] ", cnpj_limpo, " | salvo com sucesso")
  invisible(caminho)
}

# faz o POST de busca e retorna o html do resultado
jucesp_buscar <- function(cnpj_limpo, viewstate, eventval, viewstate_gen, cookie_file) {
  httr2::request("https://www.jucesponline.sp.gov.br/ResultadoBusca.aspx") |>
    httr2::req_cookie_preserve(cookie_file) |>
    httr2::req_body_form(
      `__VIEWSTATE`          = viewstate,
      `__EVENTVALIDATION`    = eventval,
      `__VIEWSTATEGENERATOR` = viewstate_gen,
      `__EVENTTARGET`        = "",
      `__EVENTARGUMENT`      = "",
      `ctl00$cphContent$frmBuscaSimples$txtPalavraChave` = cnpj_limpo,
      `ctl00$cphContent$frmBuscaSimples$btPesquisar`     = "Buscar"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_html()
}

# tenta resolver o captcha via GPT; se falhar, pede ao usuário
jucesp_ler_captcha <- function(img_path) {
  api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")

  sugestao <- ""
  if (nchar(api_key) > 0) {
    img_b64 <- base64enc::base64encode(img_path)

    for (i in seq_len(3)) {
      resp <- httr2::request("https://OpenAI-Jurimetria.openai.azure.com/openai/deployments/Jurimetria_GPT4omini/chat/completions?api-version=2024-02-01") |>
        httr2::req_headers(`api-key` = api_key, `Content-Type` = "application/json") |>
        httr2::req_body_json(list(
          max_tokens = 10,
          messages   = list(list(
            role    = "user",
            content = list(
              list(type = "image_url", image_url = list(url = paste0("data:image/jpeg;base64,", img_b64))),
              list(type = "text", text = "This is a CAPTCHA image with exactly 5 alphanumeric characters. Reply with ONLY those 5 characters in uppercase, no spaces, no punctuation, nothing else.")
            )
          ))
        )) |>
        httr2::req_perform()
      sugestao <- httr2::resp_body_json(resp)$choices[[1]]$message$content |>
        trimws() |>
        gsub(pattern = "[^A-Za-z0-9]", replacement = "") |>
        toupper()
      message("[jucesp] captcha | tentativa ", i, " | gpt retornou: '", sugestao, "'")
      if (nchar(sugestao) == 5) break
    }
  }

  if (nchar(sugestao) == 5) return(sugestao)

  # GPT nao produziu codigo valido em 3 tentativas -> readline
  trimws(toupper(readline(paste0("CAPTCHA: GPT nao conseguiu ler. Abra ", img_path, " e digite o codigo: "))))
}

# detecta captcha, resolve e devolve html com resultados
# fluxo: 3 tentativas GPT -> se rejeitado pelo servidor -> readline()
jucesp_resolver_captcha <- function(cnpj_limpo, html_busca, cookie_file, pasta, cnpj_raw = cnpj_limpo) {
  pasta_captcha <- fs::path(fs::path_dir(fs::path_dir(pasta)), "captcha")
  fs::dir_create(pasta_captcha)

  img_src  <- rvest::html_element(html_busca, "#ctl00_cphContent_gdvResultadoBusca_pnlCaptcha img") |>
    rvest::html_attr("src")
  img_path <- fs::path(pasta_captcha, paste0(cnpj_limpo, ".jpg"))

  resp_img <- httr2::request(paste0("https://www.jucesponline.sp.gov.br/", img_src)) |>
    httr2::req_cookie_preserve(cookie_file) |>
    httr2::req_perform()
  writeBin(httr2::resp_body_raw(resp_img), img_path)

  jucesp_submeter_captcha <- function(html_ref, codigo) {
    vs  <- rvest::html_element(html_ref, "#__VIEWSTATE")          |> rvest::html_attr("value")
    evd <- rvest::html_element(html_ref, "#__EVENTVALIDATION")    |> rvest::html_attr("value")
    vsg <- rvest::html_element(html_ref, "#__VIEWSTATEGENERATOR") |> rvest::html_attr("value")
    httr2::request("https://www.jucesponline.sp.gov.br/ResultadoBusca.aspx") |>
      httr2::req_cookie_preserve(cookie_file) |>
      httr2::req_body_form(
        `__VIEWSTATE`          = vs,
        `__EVENTVALIDATION`    = evd,
        `__VIEWSTATEGENERATOR` = vsg,
        `__EVENTTARGET`        = "ctl00$cphContent$gdvResultadoBusca$btEntrar",
        `__EVENTARGUMENT`      = "",
        `ctl00$cphContent$frmBuscaSimples$txtPalavraChave`            = cnpj_limpo,
        `ctl00$cphContent$frmBuscaSimples$twePalavraChave_ClientState` = "",
        `ctl00$cphContent$gdvResultadoBusca$CaptchaControl1`          = codigo
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_html()
  }

  # tentativa 1: GPT (jucesp_ler_captcha faz 3 chamadas se necessario)
  codigo_gpt <- jucesp_ler_captcha(img_path)
  html_resultado <- jucesp_submeter_captcha(html_busca, codigo_gpt)

  ainda_captcha <- rvest::html_element(html_resultado, "#ctl00_cphContent_gdvResultadoBusca_pnlCaptcha")
  if (is.na(ainda_captcha)) {
    unlink(img_path)
    return(html_resultado)
  }

  # GPT foi rejeitado pelo servidor -> fallback manual com a mesma imagem
  message("[jucesp] captcha | codigo GPT rejeitado, pedindo codigo manual")
  grid::grid.raster(jpeg::readJPEG(img_path))
  unlink(img_path)

  codigo_manual <- trimws(toupper(readline("CAPTCHA: codigo GPT rejeitado. Digite o codigo: ")))
  if (nchar(codigo_manual) == 0) return(html_resultado)

  jucesp_submeter_captcha(html_resultado, codigo_manual)
}

#' Parseia ficha cadastral JUCESP em tibble
#'
#' Lê o HTML da ficha cadastral salvo por [jucesp_download()], extrai os
#' campos da empresa e apaga o arquivo com [unlink()].
#'
#' @param arquivo Caminho do arquivo HTML
#' @returns tibble com uma linha e colunas: `cnpj`, `nire`, `tipo`, `data_constituicao`,
#'   `data_atividade`, `inscricao`, `objeto`, `capital`, `logradouro`, `numero`,
#'   `bairro`, `complemento`, `municipio`, `cep`, `uf`
#' @export
jucesp_parse <- function(arquivo) {
  html <- rvest::read_html(arquivo, encoding = "latin1")
  cnpj <- fs::path_ext_remove(fs::path_file(arquivo))

  campo <- function(id) {
    rvest::html_element(html, paste0("#ctl00_cphContent_frmPreVisualiza_", id)) |>
      rvest::html_text2() |>
      trimws()
  }

  unlink(arquivo)

  tibble::tibble(
    cnpj              = cnpj,
    nire              = campo("lblNire"),
    tipo              = campo("lblDetalhes"),
    data_constituicao = campo("lblConstituicao"),
    data_atividade    = campo("lblAtividade"),
    inscricao         = campo("lblInscricao"),
    objeto            = campo("lblObjeto"),
    capital           = campo("lblCapital"),
    logradouro        = campo("lblLogradouro"),
    numero            = campo("lblNumero"),
    bairro            = campo("lblBairro"),
    complemento       = campo("lblComplemento"),
    municipio         = campo("lblMunicipio"),
    cep               = campo("lblCep"),
    uf                = campo("lblUf")
  )
}
