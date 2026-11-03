# ==============================================================================
# Zika RIDE | Aba "Análises"
#
#   - ranking de municipios (tabela, respeita o filtro de classificacao)
#   - classificacao final por ano (quanto se confirma, descarta ou fica
#     pendente)
#   - achados da analise exploratoria, CALCULADOS da base no start (nao ha
#     numero escrito a mao: um novo ETL atualiza o texto)
# ==============================================================================

# ------------------------------------------------------------------ achados --
calcular_achados <- function() {
  d <- DADOS
  prov <- d[d$classe %in% PROVAVEIS, , drop = FALSE]
  conf <- d[d$classe %in% CONFIRMADOS, , drop = FALSE]
  mes_ext <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto",
               "setembro", "outubro", "novembro", "dezembro")
  pct <- function(x, dg = 0) fmt_pct(x, dg)
  var <- function(a, b) if (a > 0) sprintf("%s%s", if (b >= a) "+" else "−", pct(abs(b / a - 1))) else "n/a"
  nome_mun <- function(cod) GEO_DICT$municipio_nome[match(cod, GEO_DICT$municipio_geocodigo)]

  # 1. a epidemia de 2016-2017 e o que veio depois
  p_ano <- tapply(prov$casos, prov$ano, sum)
  epi <- sum(p_ano[names(p_ano) %in% c("2016", "2017")]) / sum(p_ano)
  ano_pico <- as.integer(names(which.max(p_ano)))
  media_rec <- mean(p_ano[as.integer(names(p_ano)) >= 2021 & as.integer(names(p_ano)) < ANO_MAX])

  # 2. sazonalidade (mes da quarta-feira da semana)
  mes <- as.integer(format(prov$data_ini_se + 3L, "%m"))
  por_mes <- tapply(prov$casos, factor(mes, levels = 1:12), sum)
  por_mes[is.na(por_mes)] <- 0
  jan_mai <- sum(por_mes[1:5]) / sum(por_mes)
  mes_pico <- which.max(por_mes)

  # 3. o maior surto recente (desde 2021): ano e municipio
  rec <- prov[prov$ano >= 2021, , drop = FALSE]
  r_ano <- tapply(rec$casos, rec$ano, sum)
  ano_surto <- as.integer(names(which.max(r_ano)))
  s <- rec[rec$ano == ano_surto, , drop = FALSE]
  s_mun <- sort(tapply(s$casos, s$municipio_geocodigo, sum), decreasing = TRUE)
  mun_surto <- as.integer(names(s_mun)[1])
  s_m <- s[s$municipio_geocodigo == mun_surto, , drop = FALSE]
  lab_surto <- sum(s_m$casos[s_m$classe == "confirmado_lab"]) / sum(s_m$casos)

  # 4. descarte: a onda de notificacao de 2020
  n_ano <- d |>
    dplyr::group_by(ano) |>
    dplyr::summarise(n = sum(casos), desc = sum(casos[classe == "descartado"]),
                     df = sum(casos[municipio_geocodigo == 5300108]), .groups = "drop") |>
    dplyr::mutate(p_desc = desc / n)
  maior_desc <- n_ano[n_ano$n >= 100, ] |> dplyr::slice_max(desc, n = 1, with_ties = FALSE)
  desc_total <- sum(n_ano$desc) / sum(n_ano$n)

  # 5. gestantes
  g_not <- sum(d$gestantes)
  g_desc <- sum(d$gestantes[d$classe == "descartado"]) / g_not
  g_prov <- sum(prov$gestantes)
  g_conf <- sum(conf$gestantes)
  g_ig <- sum(prov$gest_1tri) + sum(prov$gest_2tri) + sum(prov$gest_3tri)
  g_1tri <- if (g_ig) sum(prov$gest_1tri) / g_ig else NA_real_
  g_anos <- tapply(d$gestantes, d$ano, sum)
  g_anos <- g_anos[g_anos > 0]
  g_top <- names(sort(g_anos, decreasing = TRUE))[1:min(3, length(g_anos))]

  # 6. perfil
  fem <- sum(prov$feminino) / sum(prov$casos)
  fertil <- sum(prov$casos[prov$faixa_etaria %in% c("15-29", "30-44")]) / sum(prov$casos)
  fem_ng <- (sum(prov$feminino) - g_prov) / sum(prov$casos)

  # 7. ano corrente x anterior (mesmo periodo)
  sem_ult <- max(d$semana[d$ano == ANO_MAX])
  p_cor <- sum(prov$casos[prov$ano == ANO_MAX])
  p_ant <- sum(prov$casos[prov$ano == ANO_MAX - 1L & prov$semana <= sem_ult])
  c_cor <- sum(conf$casos[conf$ano == ANO_MAX])
  c_ant <- sum(conf$casos[conf$ano == ANO_MAX - 1L & conf$semana <= sem_ult])
  n_cor <- sum(d$casos[d$ano == ANO_MAX])

  # 8. gravidade e autoctonia
  obit <- sum(conf$obitos)
  aut <- sum(conf$autoctones); imp <- sum(conf$importados)

  # 9. pendencia: em investigacao nas ultimas 52 semanas
  u52 <- d[d$data_ini_se > DATA_MAX - 52L * 7L, ]
  inv52 <- if (sum(u52$casos)) sum(u52$casos[u52$classe == "em_investigacao"]) / sum(u52$casos) else NA_real_

  list(
    list(titulo = sprintf("Uma epidemia em %d e transmissão residual depois", ano_pico),
         texto = sprintf(
           "%s dos casos prováveis da série são de 2016–2017, logo após a chegada do vírus ao Brasil. Desde 2021 a RIDE registra em média %s casos prováveis por ano - quase todas as semanas têm zero notificação, por isso o painel não usa canal endêmico nem nowcast.",
           pct(epi), fmt_int(media_rec))),
    list(titulo = sprintf("Surto localizado em %s, %d", nome_mun(mun_surto), ano_surto),
         texto = sprintf(
           "O maior ano desde 2021 foi %d, com %s casos prováveis na RIDE; %s deles (%s) de residentes em %s, e %s desses confirmados por laboratório. Um surto real, não um artefato de notificação - e restrito a um município.",
           ano_surto, fmt_int(sum(s$casos)), fmt_int(sum(s_m$casos)), pct(sum(s_m$casos) / sum(s$casos)),
           nome_mun(mun_surto), pct(lab_surto))),
    list(titulo = sprintf("Em %d, %s das notificações foram descartadas", maior_desc$ano, pct(maior_desc$p_desc)),
         texto = sprintf(
           "A RIDE notificou %s suspeitas de zika em %d e descartou %s; %s das notificações daquele ano foram do DF. Na série toda, %s das notificações são descartadas: o filtro \"Casos\" (padrão: prováveis) separa a vigilância da doença.",
           fmt_int(maior_desc$n), maior_desc$ano, fmt_int(maior_desc$desc),
           pct(maior_desc$df / maior_desc$n), pct(desc_total))),
    list(titulo = "Gestantes: muita investigação, pouca confirmação",
         texto = sprintf(
           "Foram notificadas %s gestantes com suspeita de zika (anos com mais: %s); %s foram descartadas e %s ficaram como casos prováveis, %s confirmadas. Entre as prováveis com idade gestacional informada, %s estavam no 1º trimestre, o de maior risco de síndrome congênita.",
           fmt_int(g_not), paste(g_top, collapse = ", "), pct(g_desc), fmt_int(g_prov),
           fmt_int(g_conf), pct(g_1tri))),
    list(titulo = "Mulheres em idade fértil são a maioria",
         texto = sprintf(
           "%s dos casos prováveis são mulheres e %s têm entre 15 e 44 anos. Mesmo tirando as gestantes, as mulheres não gestantes são %s dos prováveis: o predomínio feminino não se explica só pela busca ativa de gestantes.",
           pct(fem), pct(fertil), pct(fem_ng))),
    list(titulo = sprintf("Verão: pico em %s", mes_ext[mes_pico]),
         texto = sprintf(
           "%s dos casos prováveis tiveram início dos sintomas entre janeiro e maio, a estação do Aedes aegypti (mesmo vetor da dengue e da chikungunya). O pico mensal da série é em %s.",
           pct(jan_mai), mes_ext[mes_pico])),
    list(titulo = sprintf("%d frente a %d (até a SE %d)", ANO_MAX, ANO_MAX - 1L, sem_ult),
         texto = sprintf(
           "Casos prováveis: %s → %s (%s); confirmados: %s → %s. Com números tão pequenos, a variação percentual diz pouco. Foram %s notificações em %d; das notificações das últimas 52 semanas, %s ainda estão em investigação.",
           fmt_int(p_ant), fmt_int(p_cor), var(p_ant, p_cor), fmt_int(c_ant), fmt_int(c_cor),
           fmt_int(n_cor), ANO_MAX, pct(inv52))),
    list(titulo = if (obit == 0) "Nenhum óbito por zika na série" else sprintf("%s óbito(s) por zika na série", fmt_int(obit)),
         texto = sprintf(
           "Entre os confirmados, %s com o campo preenchido foram autóctones (infecção no próprio município) e %s importados. A zika em adultos é quase sempre branda; o dano está no feto, que a ficha do SINAN de zika não acompanha (a síndrome congênita tem notificação própria, no RESP).",
           pct(aut / (aut + imp)), pct(imp / (aut + imp))))
  )
}

ACHADOS <- tryCatch(calcular_achados(), error = function(e) {
  warning("[Zika RIDE] Achados nao calculados: ", conditionMessage(e), call. = FALSE)
  list()
})

# ----------------------------------------------------------------------- UI --
mod_analises_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(class = "bg-primary text-white fw-semibold",
                           shiny::textOutput(ns("titulo_ranking"), inline = TRUE)),
        bslib::card_body(DT::DTOutput(ns("ranking")))
      ),
      cartao_grafico(
        "Classificação final das notificações, por ano",
        echarts4r::echarts4rOutput(ns("classif"), height = "560px"),
        rodape = "Ano de início dos sintomas, recorte RIDE-DF inteiro. Casos prováveis = tudo menos descartados."
      )
    ),
    bslib::card(
      bslib::card_header(class = "bg-primary text-white fw-semibold",
                         "Achados da análise exploratória"),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = "380px",
          !!!lapply(ACHADOS, function(a) shiny::div(
            class = "insight",
            shiny::tags$h6(a$titulo),
            shiny::tags$p(class = "small", a$texto)
          ))
        )
      )
    )
  )
}

mod_analises_server <- function(id, classe) {
  shiny::moduleServer(id, function(input, output, session) {

    output$titulo_ranking <- shiny::renderText(
      sprintf("Municípios — %s", rotulo_filtro_classe(classe())))

    output$ranking <- DT::renderDT({
      d <- filtrar(list(nivel = "base"), classes_do_filtro(classe()))
      pop <- POP_MUN_ANO |> dplyr::filter(ano == max(ano)) |> dplyr::select(-ano)
      pop_16 <- POP_MUN_ANO |> dplyr::filter(ano == 2016) |>
        dplyr::select(municipio_geocodigo, pop16 = pop)
      t <- d |>
        dplyr::group_by(municipio_geocodigo) |>
        dplyr::summarise(
          c52  = sum(casos[data_ini_se > DATA_MAX - 52L * 7L]),
          c1617 = sum(casos[ano %in% 2016:2017]),
          g    = sum(gestantes),
          tot  = sum(casos),
          .groups = "drop")
      GEO_DICT |>
        dplyr::select(municipio_geocodigo, municipio_nome, uf_sigla) |>
        dplyr::left_join(t, by = "municipio_geocodigo") |>
        dplyr::left_join(pop, by = "municipio_geocodigo") |>
        dplyr::left_join(pop_16, by = "municipio_geocodigo") |>
        dplyr::mutate(dplyr::across(c(c52, c1617, g, tot), ~ dplyr::coalesce(.x, 0))) |>
        dplyr::transmute(
          `Município` = municipio_nome, UF = uf_sigla,
          `Casos (52 sem.)` = c52,
          `2016–2017` = c1617,
          `Incid. 2016–17 /100 mil` = round(c1617 / pop16 * 1e5, 1),
          `Gestantes (série)` = g,
          `Série completa` = tot,
          `Incid. série /100 mil` = round(tot / pop * 1e5, 1)
        ) |>
        dplyr::arrange(dplyr::desc(`Incid. série /100 mil`), dplyr::desc(`Série completa`)) |>
        DT::datatable(
          rownames = FALSE, options = list(pageLength = 12, dom = "ftp",
                                           language = list(search = "Buscar:",
                                                           paginate = list(previous = "Anterior", `next` = "Próxima"))),
          caption = "Incidência da série = casos acumulados desde 2015 sobre a população mais recente."
        ) |>
        DT::formatRound(c("Incid. 2016–17 /100 mil", "Incid. série /100 mil"),
                        digits = 1, mark = ".", dec.mark = ",") |>
        DT::formatStyle("Incid. série /100 mil",
                        background = DT::styleColorBar(c(0, 300), TONS_DOENCA[["200"]]),
                        backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                        backgroundPosition = "center")
    })

    output$classif <- echarts4r::renderEcharts4r({
      larga <- DADOS |>
        dplyr::group_by(ano, classe) |>
        dplyr::summarise(casos = sum(casos), .groups = "drop") |>
        tidyr::pivot_wider(names_from = classe, values_from = casos, values_fill = 0)
      cls <- CLASSES$classe[CLASSES$classe %in% names(larga)]
      larga$x <- as.character(larga$ano)

      e <- larga |> echarts4r::e_charts(x)
      for (a in cls) e <- e |> echarts4r::e_bar_(a, name = ROTULO_CLASSE[[a]], stack = "c")
      e |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_color(unname(COR_CLASSE[cls])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE) |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_y_axis(inverse = TRUE) |>
        echarts4r::e_x_axis(name = "Notificações") |>
        echarts4r::e_grid(left = 50, right = 25, top = 10, bottom = 60) |>
        e_padrao()
    })
  })
}
