repo     <- "rcfeliz/dpc5868"
tag_old  <- "data"
tag_new  <- "cjpg_acps"
path_csv <- here::here("data-raw/csv/cjpg_acps")

arquivos <- c(
  "cjpg.csv",
  "capa.csv",
  "partes.csv",
  "partes_full.csv",
  "movimentacoes.csv",
  "movimentacoes.csv.gz"
)

# remove do release "data" -----------------------------------------------

for (arq in arquivos) {
  tryCatch(
    piggyback::pb_delete(file = arq, repo = repo, tag = tag_old),
    error = function(e) message("Não encontrado em '", tag_old, "': ", arq)
  )
}

# cria o release se nao existir ------------------------------------------

releases <- piggyback::pb_releases(repo = repo)
if (!tag_new %in% releases$tag_name) {
  piggyback::pb_new_release(repo = repo, tag = tag_new)
}

# sobe no release "cjpg_acps" --------------------------------------------

for (arq in arquivos) {
  caminho <- file.path(path_csv, arq)
  if (file.exists(caminho)) {
    piggyback::pb_upload(caminho, repo = repo, tag = tag_new)
  } else {
    message("Arquivo não encontrado localmente: ", caminho)
  }
}
