# preparacao ----

dt_cpt <- as.Date("2024-05-03")

# agrega por periodo (fev, mar, abr, pos) — media de processos por semana ----

da_periodos <- dpc5868::da_did |>
  dplyr::filter(lubridate::year(dt_dist) == 2024) |>
  dplyr::mutate(
    periodo = dplyr::case_when(
      dplyr::between(
        dt_dist,
        as.Date("2024-02-01"),
        as.Date("2024-02-29")
      ) ~ "fev",
      dplyr::between(
        dt_dist,
        as.Date("2024-03-01"),
        as.Date("2024-03-31")
      ) ~ "mar",
      dplyr::between(
        dt_dist,
        as.Date("2024-04-01"),
        as.Date("2024-04-29")
      ) ~ "abr",
      dplyr::between(
        dt_dist,
        as.Date("2024-05-06"),
        as.Date("2024-05-26")
      ) ~ "mai",
      .default = NA_character_
    )
  ) |>
  dplyr::filter(!is.na(periodo)) |>
  dplyr::count(periodo, autista) |>
  tidyr::complete(periodo, autista, fill = list(n = 0L)) |>
  dplyr::mutate(
    n_semanas = dplyr::case_match(
      periodo,
      "fev" ~ 29 / 7,
      "mar" ~ 31 / 7,
      "abr" ~ 29 / 7,
      "mai" ~ 21 / 7
    ),
    media = n / n_semanas,
    post = periodo == "mai",
    periodo_num = dplyr::case_match(
      periodo,
      "fev" ~ 1L,
      "mar" ~ 2L,
      "abr" ~ 3L,
      "mai" ~ 4L
    ),
    dt_ref = dplyr::case_when(
      periodo == "fev" ~ as.Date("2024-02-15"),
      periodo == "mar" ~ as.Date("2024-03-15"),
      periodo == "abr" ~ as.Date("2024-04-15"),
      periodo == "mai" ~ as.Date("2024-05-15")
    ),
    grupo = dplyr::if_else(autista, "Autista", "Não-autista"),
    periodo = factor(periodo, levels = c("fev", "mar", "abr", "mai"))
  ) |>
  dplyr::arrange(dt_ref)

cat("Períodos no painel:", dplyr::n_distinct(da_periodos$periodo), "\n")
cat("Observações no painel DiD:", nrow(da_periodos), "\n")
print(da_periodos)

# estimativa did: media por periodo ----

medias <- da_periodos |>
  dplyr::group_by(post, autista) |>
  dplyr::summarise(media_periodo = mean(media), .groups = "drop") |>
  tidyr::pivot_wider(
    names_from = post,
    values_from = media_periodo,
    names_prefix = "post_"
  )

diff_nao_autista <- medias$post_TRUE[!medias$autista] -
  medias$post_FALSE[!medias$autista]
diff_autista <- medias$post_TRUE[medias$autista] -
  medias$post_FALSE[medias$autista]
estimativa_did <- diff_autista - diff_nao_autista

cat(
  "\nDiferença (não-autista):",
  round(diff_nao_autista, 1),
  "processos/semana\n"
)
cat("Diferença (autista):    ", round(diff_autista, 1), "processos/semana\n")
cat("Estimativa DiD:         ", round(estimativa_did, 1), "processos/semana\n")


# visualizacao: media por periodo com contrafactual ----

media_pre_nao <- da_periodos |>
  dplyr::filter(!post, !autista) |>
  dplyr::pull(media) |>
  mean()

media_pre_aut <- da_periodos |>
  dplyr::filter(!post, autista) |>
  dplyr::pull(media) |>
  mean()

da_cf <- dplyr::bind_rows(
  da_periodos |>
    dplyr::filter(!post, autista, periodo == "abr") |>
    dplyr::transmute(periodo, dt_ref, media, grupo = "Contrafactual"),
  da_periodos |>
    dplyr::filter(post, !autista) |>
    dplyr::transmute(
      periodo,
      dt_ref,
      media = media_pre_aut + (media - media_pre_nao),
      grupo = "Contrafactual"
    )
)

y11 <- da_periodos |>
  dplyr::filter(autista, post) |>
  dplyr::pull(media)

y10 <- da_periodos |> # contrafactual
  dplyr::filter(post, !autista) |>
  dplyr::transmute(
    periodo,
    dt_ref,
    media = media_pre_aut + (media - media_pre_nao),
    grupo = "Contrafactual"
  ) |>
  dplyr::pull(media)

did <- y11 - y10

x_brace <- as.Date("2024-05-15") + 8L
tick_w <- 3L

ggplot2::ggplot() +
  ggplot2::geom_vline(
    xintercept = dt_cpt,
    linetype = "dashed",
    color = "gray50"
  ) +
  ggplot2::geom_line(
    data = da_periodos,
    ggplot2::aes(x = dt_ref, y = media, color = grupo, group = grupo),
    linewidth = 1
  ) +
  ggplot2::geom_point(
    data = da_periodos,
    ggplot2::aes(x = dt_ref, y = media, color = grupo),
    size = 2.5
  ) +
  ggplot2::geom_line(
    data = da_cf,
    ggplot2::aes(x = dt_ref, y = media, group = grupo),
    linetype = "dashed",
    color = "#E05A2B",
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    data = da_cf |> dplyr::filter(periodo == "mai"),
    ggplot2::aes(x = dt_ref, y = media),
    shape = 21,
    fill = "white",
    color = "#E05A2B",
    size = 2.5
  ) +
  ggplot2::annotate(
    "text",
    x = dt_cpt + 2,
    y = max(da_periodos$media) * 0.96,
    label = format(dt_cpt, "%d/%m/%Y"),
    hjust = 0,
    size = 3,
    color = "gray40"
  ) +
  ggplot2::annotate(
    "segment",
    x = x_brace,
    xend = x_brace,
    y = y10,
    yend = y11,
    linewidth = 0.5,
    color = 'grey50'
  ) +
  ggplot2::annotate(
    "segment",
    x = x_brace - tick_w,
    xend = x_brace,
    y = y10,
    yend = y10,
    linewidth = 0.5,
    color = 'grey50'
  ) +
  ggplot2::annotate(
    "segment",
    x = x_brace - tick_w,
    xend = x_brace,
    y = y11,
    yend = y11,
    linewidth = 0.5,
    color = 'grey50'
  ) +
  ggplot2::annotate(
    "text",
    x = x_brace + 2L,
    y = (y10 + y11) / 2,
    label = paste0(round(did, 1), " processos"),
    hjust = 0,
    size = 2.8,
    color = 'grey30'
  ) +
  ggplot2::scale_x_date(
    breaks = as.Date(c("2024-02-15", "2024-03-15", "2024-04-15", "2024-05-15")),
    labels = c("fev", "mar", "abr", "mai"),
    expand = ggplot2::expansion(add = c(5, 20))
  ) +
  ggplot2::scale_color_manual(
    values = c("Autista" = "#E05A2B", "Não-autista" = "#2B6BE0"),
    labels = c(
      "Autista" = "Autista (observado)",
      "Não-autista" = "Não-autista (observado)"
    )
  ) +
  ggplot2::labs(
    title = "Diferenças em Diferenças: média de processos por semana",
    subtitle = "Tracejado laranja: contrafactual para autistas",
    x = NULL,
    y = "Processos por semana (média do período)",
    color = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

# event study: coeficiente diferencial por periodo ----
# dados diarios para ter graus de liberdade (period-level tem 0 df)
# referencia: abr (ultimo pre-periodo); vline entre abr e mai

da_es <- dpc5868::da_did |>
  dplyr::mutate(
    periodo = dplyr::case_when(
      dplyr::between(
        dt_dist,
        as.Date("2024-02-01"),
        as.Date("2024-02-29")
      ) ~ "fev",
      dplyr::between(
        dt_dist,
        as.Date("2024-03-01"),
        as.Date("2024-03-31")
      ) ~ "mar",
      dplyr::between(
        dt_dist,
        as.Date("2024-04-01"),
        as.Date("2024-04-29")
      ) ~ "abr",
      dplyr::between(
        dt_dist,
        as.Date("2024-05-06"),
        as.Date("2024-05-26")
      ) ~ "mai",
      .default = NA_character_
    )
  ) |>
  dplyr::filter(!is.na(periodo)) |>
  dplyr::count(dt_dist, autista, periodo) |>
  tidyr::complete(
    tidyr::nesting(dt_dist, periodo),
    autista,
    fill = list(n = 0L)
  ) |>
  dplyr::mutate(
    periodo = factor(periodo, levels = c("abr", "fev", "mar", "mai"))
  )

modelo_es <- estimatr::lm_robust(
  n ~ autista * periodo,
  data = da_es,
  se_type = "HC2"
)

es_coefs <- broom::tidy(modelo_es) |>
  dplyr::filter(stringr::str_detect(term, "autistaTRUE:periodo")) |>
  dplyr::mutate(
    evento = dplyr::case_when(
      stringr::str_detect(term, "fev") ~ -2L,
      stringr::str_detect(term, "mar") ~ -1L,
      stringr::str_detect(term, "mai") ~ 1L
    )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      term = "referencia (abr)",
      evento = 0L,
      estimate = 0,
      conf.low = 0,
      conf.high = 0
    )
  ) |>
  dplyr::arrange(evento)

ggplot2::ggplot(es_coefs, ggplot2::aes(x = evento, y = estimate)) +
  ggplot2::geom_hline(yintercept = 0, color = "gray80") +
  ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
  ggplot2::geom_pointrange(
    ggplot2::aes(ymin = conf.low, ymax = conf.high),
    shape = 21,
    fill = "white",
    size = 0.5
  ) +
  ggplot2::scale_x_continuous(breaks = c(-2L, -1L, 0L, 1L)) +
  ggplot2::labs(
    title = "Event study: efeito diferencial autistas × período",
    subtitle = "Referência: abr | Erros-padrão HC2",
    x = "Período (relativo ao tratamento)",
    y = "Coeficiente (processos/dia)"
  ) +
  ggplot2::theme_minimal()

# placebos: mar e abr como falsos tratamentos ----
# pre de cada placebo: apenas os periodos anteriores ao falso corte

medias_mar <- da_periodos |>
  dplyr::filter(periodo %in% c("fev", "mar")) |>
  dplyr::mutate(fake_post = periodo == "mar") |>
  dplyr::group_by(fake_post, autista) |>
  dplyr::summarise(m = mean(media), .groups = "drop") |>
  tidyr::pivot_wider(names_from = fake_post, values_from = m, names_prefix = "p_")

did_mar <- (medias_mar$p_TRUE[medias_mar$autista] - medias_mar$p_FALSE[medias_mar$autista]) -
  (medias_mar$p_TRUE[!medias_mar$autista] - medias_mar$p_FALSE[!medias_mar$autista])

medias_abr <- da_periodos |>
  dplyr::filter(periodo %in% c("fev", "mar", "abr")) |>
  dplyr::mutate(fake_post = periodo == "abr") |>
  dplyr::group_by(fake_post, autista) |>
  dplyr::summarise(m = mean(media), .groups = "drop") |>
  tidyr::pivot_wider(names_from = fake_post, values_from = m, names_prefix = "p_")

did_abr <- (medias_abr$p_TRUE[medias_abr$autista] - medias_abr$p_FALSE[medias_abr$autista]) -
  (medias_abr$p_TRUE[!medias_abr$autista] - medias_abr$p_FALSE[!medias_abr$autista])

cat("DiD placebo (mar):", round(did_mar, 1), "processos/semana\n")
cat("DiD placebo (abr):", round(did_abr, 1), "processos/semana\n")
cat("DiD real   (mai):", round(did, 1), "processos/semana\n")

tibble::tibble(
  corte = factor(
    c("mar (placebo)", "abr (placebo)", "mai (observado)"),
    levels = c("mar (placebo)", "abr (placebo)", "mai (observado)")
  ),
  estimativa = c(did_mar, did_abr, did),
  real = c(FALSE, FALSE, TRUE)
) |>
  ggplot2::ggplot(ggplot2::aes(x = corte, y = estimativa, fill = real)) +
  ggplot2::geom_col(width = 0.5) +
  ggplot2::geom_hline(yintercept = 0, color = "gray40") +
  ggplot2::geom_text(
    ggplot2::aes(
      label = round(estimativa, 1),
      vjust = dplyr::if_else(estimativa >= 0, -0.5, 1.5)
    ),
    size = 3
  ) +
  ggplot2::scale_fill_manual(
    values = c("FALSE" = "gray70", "TRUE" = "#E05A2B")
  ) +
  ggplot2::labs(
    title = "Teste placebo: DiD por data de corte",
    subtitle = "Placebos: mar e abr como falso tratamento | Observado: mai",
    x = NULL,
    y = "DiD estimado (processos/semana)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

# fim ----
