conn = DBI::dbConnect(
  odbc::odbc(),
  Driver = 'ODBC Driver 18 for SQL Server',
  Server = 'mpdbcaexlab-v',
  Database = 'jurimetria',
  UID = Sys.getenv("CONN_UID"),
  PWD = Sys.getenv("CONN_PWD"),
  TrustServerCertificate = 'yes'
)

acps_2020 <- dplyr::tbl(conn, DBI::Id("tjsp", "api")) |>
  dplyr::filter(
    classe == "Ação civil pública",
    data_recebimento >= as.Date("2020-01-01") &
      data_recebimento <= as.Date("2020-12-31")
  ) |>
  dplyr::collect()

usethis::use_data(acps_2020, overwrite = TRUE)
