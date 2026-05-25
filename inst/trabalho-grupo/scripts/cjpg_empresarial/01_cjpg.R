# preparacao -------------------------------------------------------------

path <- here::here("data-raw/html/cjpg")
path_csv <- here::here("data-raw/csv/cjpg_empresarial")

fs::dir_create(path)
fs::dir_create(path_csv)

repo <- "rcfeliz/dpc5868"
tag <- "cjpg_empresarial"

# download ---------------------------------------------------------------

varas <- c(
  "354-1",
  "373-1",
  "359-1",
  "100-1146",
  "100-1145",
  "100-1155",
  "260-1",
  "260-2"
)

for (vara in varas) {
  tjsp::baixar_cjpg(
    # inicio = "01/10/2023",
    # fim = "01/05/2026",
    vara = vara,
    diretorio = path
  )
}

# parse ------------------------------------------------------------------

files <- fs::dir_ls(path)
cjpg <- tjsp::tjsp_ler_cjpg(arquivos = files) |>
  tibble::as_tibble()
unlink(files)

# upload release ---------------------------------------------------------

releases <- piggyback::pb_releases(repo = repo)
if (!tag %in% releases$tag_name) {
  piggyback::pb_new_release(repo = repo, tag = tag)
}

readr::write_csv(cjpg, file.path(path_csv, "cjpg.csv"))
piggyback::pb_upload(file.path(path_csv, "cjpg.csv"), repo = repo, tag = tag)
