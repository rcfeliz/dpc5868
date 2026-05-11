# preparacao -------------------------------------------------------------

path     <- here::here("data-raw/html/cjpg")
path_csv <- here::here("data-raw/csv")

fs::dir_create(path)
fs::dir_create(path_csv)

repo <- "rcfeliz/dpc5868"
tag  <- "data"

# download ---------------------------------------------------------------

tjsp::baixar_cjpg(
  classe = 8537,
  diretorio = path
)

# parse ------------------------------------------------------------------

files <- fs::dir_ls(path)
cjpg <- tjsp::tjsp_ler_cjpg(arquivos = files) |>
  tibble::as_tibble()
unlink(files)

# upload release ---------------------------------------------------------

readr::write_csv(cjpg, file.path(path_csv, "cjpg.csv"))
piggyback::pb_upload(file.path(path_csv, "cjpg.csv"), repo = repo, tag = tag)
