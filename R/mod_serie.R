# ==============================================================================
# Zika RIDE | Modulo: evolucao por classificacao final
#
# Barras empilhadas por classificacao (casos ou incidencia por semana ou mes
# de inicio dos sintomas). Na visao "Proporção" as barras viram 100% e
# mostram o encerramento das fichas - a onda de 2020 no DF, por exemplo, foi
# quase toda descartada.
#
# Sem nowcast: com 0-2 notificacoes por semana, a correcao de atraso seria
# ruido. O padrao e a serie completa agregada por MES, porque a semanal fica
# quase toda vazia depois de 2017.
# ==============================================================================

mod_serie_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(ns("visao"), "Visão",
                        c("Casos" = "casos", "Incidência /100 mil" = "inc",
                          "Proporção por classificação (%)" = "prop"),
                        selected = "casos"),
    shiny::radioButtons(ns("agrega"), "Agregação",
                        c("Mês" = "mes", "Semana epidemiológica" = "semana"),
                        selected = "mes"),
    shiny::radioButtons(ns("periodo"), "Período",
                        c("Últimas 52 semanas" = "52", "Últimas 104 semanas" = "104",
                          "Série completa" = "tudo"),
                        selected = "tudo")
  )
}

mod_serie_saida <- function(id, height = "380px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}

mod_serie_server <- function(id, geo, classe, serie) {
  shiny::moduleServer(id, function(input, output, session) {

    output$grafico <- echarts4r::renderEcharts4r({
      cls <- classes_do_filtro(classe())
      ini <- switch(input$periodo %||% "tudo",
                    "52" = DATA_MAX - 51L * 7L, "104" = DATA_MAX - 103L * 7L, DATA_MIN)
      por_mes <- identical(input$agrega %||% "mes", "mes")

      # Periodo de cada semana: o domingo de inicio (semanal) ou o mes da
      # quarta-feira da semana (mensal - mesmo criterio do ano epidemiologico).
      periodo_de <- function(dd) if (por_mes) as.Date(format(dd + 3L, "%Y-%m-01")) else dd

      d <- filtrar(geo(), cls)
      d <- d[d$data_ini_se >= ini, , drop = FALSE] |>
        dplyr::mutate(per = periodo_de(data_ini_se)) |>
        dplyr::group_by(per, classe) |>
        dplyr::summarise(casos = sum(casos), .groups = "drop")

      grade <- serie() |>
        dplyr::filter(data_ini_se >= ini) |>
        dplyr::mutate(per = periodo_de(data_ini_se)) |>
        dplyr::group_by(per) |>
        dplyr::summarise(pop = max(pop), .groups = "drop")

      larga <- grade |>
        dplyr::left_join(tidyr::pivot_wider(d, names_from = classe, values_from = casos,
                                            values_fill = 0),
                         by = "per")
      cls <- CLASSES$classe[CLASSES$classe %in% names(larga)]
      shiny::validate(shiny::need(length(cls) > 0, "Sem casos para este recorte e classificação."))
      larga[cls] <- lapply(larga[cls], function(x) dplyr::coalesce(x, 0))

      visao <- input$visao %||% "casos"
      fator <- switch(visao, inc = 1e5 / larga$pop, prop = 100 / pmax(rowSums(larga[cls]), 1), 1)
      larga[cls] <- lapply(larga[cls], function(x) round(x * fator, 2))
      larga$x <- if (por_mes) format(larga$per, "%m/%Y") else as.character(larga$per)

      e <- larga |> echarts4r::e_charts(x)
      for (a in cls) {
        e <- e |> echarts4r::e_bar_(a, name = ROTULO_CLASSE[[a]], stack = "classes",
                                    barCategoryGap = "15%")
      }

      eixo_y <- switch(visao, casos = "Casos", inc = "Incidência /100 mil", prop = "%")
      fmt_x <- if (por_mes) "function(v){return v;}"
               else sprintf("function(v){return %s(v);}", JS_DATA_BR)
      e |>
        echarts4r::e_color(unname(COR_CLASSE[cls])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"),
                             confine = TRUE) |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_x_axis(axisLabel = list(formatter = htmlwidgets::JS(fmt_x))) |>
        echarts4r::e_y_axis(name = eixo_y, max = if (visao == "prop") 100 else NULL) |>
        echarts4r::e_grid(left = 55, right = 20, top = 30, bottom = 70) |>
        echarts4r::e_datazoom(type = "inside") |>
        e_padrao()
    })
  })
}
