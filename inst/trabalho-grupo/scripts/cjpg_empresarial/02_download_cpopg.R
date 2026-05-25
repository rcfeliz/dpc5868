# preparacao -------------------------------------------------------------

path <- here::here("data-raw/html/cpopg")
path_rds_capa <- here::here("data-raw/rds/capa")
path_rds_partes <- here::here("data-raw/rds/partes")
path_rds_movs <- here::here("data-raw/rds/movimentacoes")
path_csv <- here::here("data-raw/csv/cjpg_empresarial")

fs::dir_create(path)
fs::dir_create(path_rds_capa)
fs::dir_create(path_rds_partes)
fs::dir_create(path_rds_movs)
fs::dir_create(path_csv)

repo <- "rcfeliz/dpc5868"
tag <- "cjpg_empresarial"

reautenticar <- function() {
  gmailr::gm_auth("ric.feliz@gmail.com")
  tjsp::autenticar(email_provider = "gmail")
}

reautenticar()

# importacao -------------------------------------------------------------

piggyback::pb_download("cjpg.csv", repo = repo, tag = tag, dest = path_csv)

processos_batches <- readr::read_csv(file.path(path_csv, "cjpg.csv")) |>
  tibble::as_tibble() |>
  dplyr::filter(disponibilizacao >= "2023-10-01") |>
  dplyr::distinct(processo) |>
  dplyr::pull(processo) |>
  JurisMiner::dividir_sequencia(n = 200)

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

  list(
    capa = da |> dplyr::select(-partes, -movimentacoes) |> tibble::as_tibble(),
    partes = da |>
      dplyr::select(processo, cd_processo, partes) |>
      tidyr::unnest(partes) |>
      tibble::as_tibble(),
    movimentacoes = da |>
      dplyr::select(processo, cd_processo, movimentacoes) |>
      tidyr::unnest(movimentacoes) |>
      tibble::as_tibble()
  )
}

# loop com retry e salvamento intermediario ------------------------------

batches_com_erro <- c()

for (i in seq_along(processos_batches)) {
  if (file.exists(file.path(path_rds_capa, paste0("capa_batch", i, ".rds")))) {
    message("Batch ", i, " já concluído, pulando.")
    next
  }
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
      file.path(path_rds_movs, paste0("movs_batch", i, ".rds"))
    )
  }
}

if (length(batches_com_erro) > 0) {
  message("Batches com falha: ", paste(batches_com_erro, collapse = ", "))
}

# consolidacao e upload --------------------------------------------------

# consolidar_e_subir <- function(path_rds, nome) {
#   message("Consolidando e subindo: ", nome)
#   df <- fs::dir_ls(path_rds, glob = "*.rds") |>
#     purrr::map_dfr(readr::read_rds) |>
#     dplyr::distinct()
#   arquivo <- file.path(path_csv, paste0(nome, ".csv"))
#   readr::write_csv(df, arquivo)
#   piggyback::pb_upload(
#     arquivo,
#     repo = repo,
#     tag = tag
#   )
#   unlink(fs::dir_ls(path_rds, glob = "*.rds"))
# }

# consolidar_e_subir(path_rds_capa, "capa")
# consolidar_e_subir(path_rds_partes, "partes")
# consolidar_e_subir(path_rds_movs, "movimentacoes")
