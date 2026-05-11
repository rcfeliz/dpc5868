# preparacao -------------------------------------------------------------

path <- here::here("data-raw/html/cjpg")
fs::dir_create(path)

# download ---------------------------------------------------------------

tjsp::baixar_cjpg(
  classe = 8537,
  diretorio = path
)

# parse ------------------------------------------------------------------

files <- fs::dir_ls(path)
cjpg <- tjsp::tjsp_ler_cjpg(
  arquivos = files
)

usethis::use_data(cjpg, overwrite = TRUE)
unlink(files)
