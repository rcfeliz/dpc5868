# preparacao -------------------------------------------------------------

path <- here::here("data-raw/json/partes/")
path_rds <- here::here("data-raw/rds/partes_docs")
path_csv <- here::here("data-raw/csv/cjpg_empresarial")

fs::dir_create(path)
fs::dir_create(path_rds)
fs::dir_create(path_csv)

repo <- "rcfeliz/dpc5868"
tag <- "cjpg_empresarial"

# autenticacao inicial ---------------------------------------------------

reautenticar <- function() {
  gmailr::gm_auth("ric.feliz@gmail.com")
  tjsp::autenticar(email_provider = "gmail")
}

reautenticar()

# importacao -------------------------------------------------------------

piggyback::pb_download("capa.csv", repo = repo, tag = tag, dest = path_csv)

cds <- readr::read_csv(file.path(path_csv, "capa.csv")) |>
  tibble::as_tibble() |>
  dplyr::distinct(cd_processo) |>
  dplyr::pull(cd_processo)

# batches ----------------------------------------------------------------

ja_processados <- c()
if (length(fs::dir_ls(path_rds, glob = "*.rds")) > 0) {
  ja_processados <- fs::dir_ls(path_rds, glob = "*.rds") |>
    purrr::map_dfr(readr::read_rds) |>
    dplyr::pull(cd_processo) |>
    unique()
}

cds <- setdiff(cds, ja_processados)
message(
  length(ja_processados),
  " processos já baixados. Restam ",
  length(cds),
  "."
)

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
      file.path(path_rds, paste0("partes_batch", i, ".rds"))
    )
  }
}

if (length(batches_com_erro) > 0) {
  message("Batches com falha: ", paste(batches_com_erro, collapse = ", "))
}

# consolidacao e upload --------------------------------------------------

message("Consolidando e subindo: partes_full")

partes_full <- fs::dir_ls(path_rds, glob = "*.rds") |>
  purrr::map_dfr(readr::read_rds)

readr::write_csv(partes_full, file.path(path_csv, "partes_full.csv"))
piggyback::pb_upload(
  file.path(path_csv, "partes_full.csv"),
  repo = repo,
  tag = tag
)
# unlink(fs::dir_ls(path_rds, glob = "*.rds"))
