# preparacao -------------------------------------------------------------

path <- here::here("data-raw/json/partes/")
path_rds_partes <- here::here("data-raw/rds/partes_docs")

fs::dir_create(path)
fs::dir_create(path_rds_partes)

# autenticacao inicial ---------------------------------------------------

reautenticar <- function() {
  gmailr::gm_auth("ric.feliz@gmail.com")
  tjsp::autenticar(email_provider = "gmail")
}

reautenticar()

# listagem ---------------------------------------------------------------

cds <- dpc5868::capa$cd_processo

# batches ----------------------------------------------------------------

batch_size <- 200
batches <- split(cds, ceiling(seq_along(cds) / batch_size))
n_batches <- length(batches)

baixar_e_parsear <- function(cds_batch) {
  tjsp::tjsp_baixar_partes_docs(cd_processo = cds_batch, diretorio = path)
  files <- fs::dir_ls(path)
  on.exit(unlink(files))
  tjsp::tjsp_ler_cpopg_partes_docs(arquivos = files) |>
    tibble::as_tibble()
}

# loop com retry e salvamento intermediario ------------------------------

batches_com_erro <- c()

for (i in seq_along(batches)) {
  message("Batch ", i, " de ", n_batches)

  resultado <- tryCatch(
    baixar_e_parsear(batches[[i]]),
    error = function(e) {
      message("Erro no batch ", i, ": ", conditionMessage(e))
      message("Re-autenticando e tentando novamente...")
      reautenticar()
      tryCatch(
        baixar_e_parsear(batches[[i]]),
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
      resultado,
      file.path(path_rds_partes, paste0("partes_batch", i, ".rds"))
    )
  }
}

if (length(batches_com_erro) > 0) {
  message("Batches com falha: ", paste(batches_com_erro, collapse = ", "))
}

# consolidacao final -----------------------------------------------------

# partes_full <- fs::dir_ls(path_rds_partes, glob = "*.rds") |>
#   purrr::map(readr::read_rds) |>
#   dplyr::bind_rows()

# usethis::use_data(partes_full, overwrite = TRUE)

# limpeza ----------------------------------------------------------------

# unlink(fs::dir_ls(path_rds_partes, glob = "*.rds"))
