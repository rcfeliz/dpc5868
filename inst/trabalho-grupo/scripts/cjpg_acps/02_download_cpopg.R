# preparacao -------------------------------------------------------------

path <- here::here("data-raw/html/cpopg")
path_rds_capa <- here::here("data-raw/rds/capa")
path_rds_partes <- here::here("data-raw/rds/partes")
path_rds_movimentacoes <- here::here("data-raw/rds/movimentacoes")

fs::dir_create(path)
fs::dir_create(path_rds_capa)
fs::dir_create(path_rds_partes)
fs::dir_create(path_rds_movimentacoes)

reautenticar <- function() {
  gmailr::gm_auth("ric.feliz@gmail.com")
  tjsp::autenticar(email_provider = "gmail")
}

reautenticar()

# batches ----------------------------------------------------------------

batch_size <- 200
processos_batches <- dpc5868::cjpg$processo |>
  JurisMiner::dividir_sequencia(n = batch_size)

n_batches <- length(processos_batches)

# funcao de download e parse de um batch ---------------------------------

baixar_e_parsear <- function(batch) {
  tjsp::tjsp_baixar_cpopg(processos = batch, diretorio = path)

  files <- fs::dir_ls(path)
  on.exit(unlink(files))

  da <- tjsp2::esaj_cpopg_ler(
    arquivos = files,
    formato = "Padronizado",
    outros = c("Partes", "Movimentacoes")
  )

  capa <- da |>
    dplyr::select(-partes, -movimentacoes)

  partes <- da |>
    dplyr::select(processo, cd_processo, partes) |>
    tidyr::unnest(partes)

  movimentacoes <- da |>
    dplyr::select(processo, cd_processo, movimentacoes) |>
    tidyr::unnest(movimentacoes)

  list(capa = capa, partes = partes, movimentacoes = movimentacoes)
}

# loop com retry e salvamento intermediario ------------------------------

batches_com_erro <- c()

for (i in seq_along(processos_batches)) {
  message("Batch ", i, " de ", n_batches)

  resultado <- tryCatch(
    baixar_e_parsear(processos_batches[[i]]),
    error = function(e) {
      message("Erro no batch ", i, ": ", conditionMessage(e))
      message("Re-autenticando e tentando novamente...")
      reautenticar()
      tryCatch(
        baixar_e_parsear(processos_batches[[i]]),
        error = function(e2) {
          message("Falha definitiva no batch ", i, ": ", conditionMessage(e2))
          batches_com_erro <<- c(batches_com_erro, i)
          NULL
        }
      )
    }
  )

  if (!is.null(resultado)) {
    readr::write_rds(
      resultado$capa,
      file.path(path_rds_capa, paste0("capa_batch", i, ".rds"))
    )
    readr::write_rds(
      resultado$partes,
      file.path(path_rds_partes, paste0("partes_batch", i, ".rds"))
    )
    readr::write_rds(
      resultado$movimentacoes,
      file.path(
        path_rds_movimentacoes,
        paste0("movimentacoes_batch", i, ".rds")
      )
    )
  }
}

if (length(batches_com_erro) > 0) {
  message("Batches com falha: ", paste(batches_com_erro, collapse = ", "))
}

# consolidacao final -----------------------------------------------------

# capa <- fs::dir_ls(path_rds_capa, glob = "*.rds") |>
#   purrr::map(readr::read_rds) |>
#   dplyr::bind_rows()

# partes <- fs::dir_ls(path_rds_partes, glob = "*.rds") |>
#   purrr::map(readr::read_rds) |>
#   dplyr::bind_rows()

# movimentacoes <- fs::dir_ls(path_rds_movimentacoes, glob = "*.rds") |>
#   purrr::map(readr::read_rds) |>
#   dplyr::bind_rows()

# usethis::use_data(capa, partes, movimentacoes, overwrite = TRUE)

# limpeza ----------------------------------------------------------------

# unlink(fs::dir_ls(path_rds_capa,          glob = "*.rds"))
# unlink(fs::dir_ls(path_rds_partes,        glob = "*.rds"))
# unlink(fs::dir_ls(path_rds_movimentacoes, glob = "*.rds"))
