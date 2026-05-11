processos <- da$processo
path <- here::here("data-raw/html/cpopg")
fs::dir_create(path)

gmailr::gm_auth("ric.feliz@gmail.com")
tjsp::autenticar(email_provider = "gmail")

tjsp::tjsp_baixar_cpopg(
  processos = processos[1],
  diretorio = path
)

files <- fs::dir_ls(path)

tjsp::ler_dados_cpopg(
  arquivos = files
)
