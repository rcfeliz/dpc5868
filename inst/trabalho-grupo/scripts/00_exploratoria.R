# preparacao -------------------------------------------------------------

path <- here::here("data-raw/html/cjpg")
fs::dir_create(path)

# download ---------------------------------------------------------------

tjsp::baixar_cjpg(
  livre = '"negativação" E ("banco" OU "bancário" OU "financeira")',
  inicio = "01/01/2020",
  fim = "31/12/2020",
  diretorio = path
)

# parse ------------------------------------------------------------------

files <- fs::dir_ls(path)
cjpg <- tjsp::tjsp_ler_cjpg(
  arquivos = files
)

usethis::use_data(cjpg, overwrite = TRUE)
unlink(files)
