# ==============================================================================
# Zika RIDE | Modulo: gestantes
#
# No lugar do canal endemico do chikungunya. O que pesa na zika e a infeccao
# na gravidez (sindrome congenita / microcefalia), entao o painel olha as
# gestantes notificadas:
#
#   "Por classificação": gestantes notificadas por ano, empilhadas pela
#     classificacao final - quanto da vigilancia de gestantes se confirma,
#     quanto e descartado. Ignora o filtro "Casos" de proposito.
#   "Por trimestre": gestantes do filtro por ano e idade gestacional
#     (CS_GESTANT 1-3 = trimestre, 4 = ignorada). O 1o trimestre e o de
#     maior risco para o feto.
# ==============================================================================

mod_gestantes_saida <- function(id, qual = c("classe", "trimestre"), height = "360px") {
  qual <- match.arg(qual)
  echarts4r::echarts4rOutput(shiny::NS(id)(qual), height = height)
}

mod_gestantes_server <- function(id, geo, classe) {
  shiny::moduleServer(id, function(input, output, session) {

    grade_anos <- data.frame(ano = seq(ANO_MIN, ANO_MAX))

    output$classe <- echarts4r::renderEcharts4r({
      d <- filtrar(geo())
      shiny::validate(shiny::need(sum(d$gestantes) > 0, "Nenhuma gestante notificada neste recorte."))
      larga <- d |>
        dplyr::group_by(ano, classe) |>
        dplyr::summarise(n = sum(gestantes), .groups = "drop") |>
        tidyr::pivot_wider(names_from = classe, values_from = n, values_fill = 0)
      larga <- grade_anos |> dplyr::left_join(larga, by = "ano")
      cls <- CLASSES$classe[CLASSES$classe %in% names(larga)]
      larga[cls] <- lapply(larga[cls], function(x) dplyr::coalesce(x, 0))
      larga$x <- as.character(larga$ano)

      e <- larga |> echarts4r::e_charts(x)
      for (a in cls) e <- e |> echarts4r::e_bar_(a, name = ROTULO_CLASSE[[a]], stack = "c")
      e |>
        echarts4r::e_color(unname(COR_CLASSE[cls])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE) |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_y_axis(name = "Gestantes notificadas") |>
        echarts4r::e_grid(left = 55, right = 15, top = 35, bottom = 60) |>
        e_padrao()
    })

    output$trimestre <- echarts4r::renderEcharts4r({
      d <- filtrar(geo(), classes_do_filtro(classe()))
      shiny::validate(shiny::need(sum(d$gestantes) > 0,
                                  "Nenhuma gestante neste recorte e classificação."))
      g <- d |>
        dplyr::group_by(ano) |>
        dplyr::summarise(t1 = sum(gest_1tri), t2 = sum(gest_2tri), t3 = sum(gest_3tri),
                         ign = sum(gestantes) - sum(gest_1tri) - sum(gest_2tri) - sum(gest_3tri),
                         .groups = "drop")
      g <- grade_anos |>
        dplyr::left_join(g, by = "ano") |>
        dplyr::mutate(dplyr::across(c(t1, t2, t3, ign), ~ dplyr::coalesce(.x, 0)),
                      x = as.character(ano))

      g |>
        echarts4r::e_charts(x) |>
        echarts4r::e_bar(t1, name = "1º trimestre", stack = "t") |>
        echarts4r::e_bar(t2, name = "2º trimestre", stack = "t") |>
        echarts4r::e_bar(t3, name = "3º trimestre", stack = "t") |>
        echarts4r::e_bar(ign, name = "Idade gestacional ignorada", stack = "t") |>
        echarts4r::e_color(c(TONS_DOENCA[["800"]], TONS_DOENCA[["500"]], TONS_DOENCA[["300"]],
                             PALETA[["neutro"]])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE) |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_y_axis(name = "Gestantes") |>
        echarts4r::e_grid(left = 55, right = 15, top = 35, bottom = 60) |>
        e_padrao()
    })
  })
}
