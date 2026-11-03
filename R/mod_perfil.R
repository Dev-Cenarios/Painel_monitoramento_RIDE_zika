# ==============================================================================
# Zika RIDE | Modulo: perfil (faixa etaria x sexo)
#
# Casos por faixa, mulheres x homens. A zika da RIDE pesa nas mulheres (cerca
# de 2 em cada 3 notificacoes): parte e exposicao, parte e a busca ativa de
# gestantes depois de 2016 - toda gestante com exantema passou a ser
# notificada e testada.
# ==============================================================================

mod_perfil_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::radioButtons(ns("periodo"), "Período",
                      c("Últimas 52 semanas" = "52", "Desde 2018" = "2018",
                        "Série completa" = "tudo"), selected = "tudo")
}

mod_perfil_saida <- function(id, height = "330px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("faixa"), height = height)
}

mod_perfil_server <- function(id, geo, classe) {
  shiny::moduleServer(id, function(input, output, session) {

    output$faixa <- echarts4r::renderEcharts4r({
      ini <- switch(input$periodo %||% "tudo",
                    "52" = DATA_MAX - 51L * 7L, "2018" = as.Date("2017-12-31"), DATA_MIN)
      d <- filtrar(geo(), classes_do_filtro(classe()))
      d <- d[d$data_ini_se >= ini & d$faixa_etaria %in% FAIXAS, , drop = FALSE]
      shiny::validate(shiny::need(sum(d$casos) > 0, "Sem casos no período."))

      g <- d |>
        dplyr::group_by(faixa_etaria) |>
        dplyr::summarise(gestantes = sum(gestantes),
                         outras = sum(feminino) - sum(gestantes),
                         homens = sum(casos) - sum(feminino), .groups = "drop")
      g <- data.frame(faixa_etaria = FAIXAS) |>
        dplyr::left_join(g, by = "faixa_etaria") |>
        dplyr::mutate(dplyr::across(c(gestantes, outras, homens), ~ dplyr::coalesce(.x, 0)),
                      x = unname(ROTULO_FAIXA[faixa_etaria]))

      g |>
        echarts4r::e_charts(x) |>
        echarts4r::e_bar(gestantes, name = "Mulheres gestantes", stack = "s") |>
        echarts4r::e_bar(outras, name = "Mulheres não gestantes", stack = "s") |>
        echarts4r::e_bar(homens, name = "Homens", stack = "s") |>
        echarts4r::e_color(c(TONS_DOENCA[["700"]], TONS_DOENCA[["300"]], TONS_AZUL[["300"]])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE) |>
        echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_y_axis(name = "Casos") |>
        echarts4r::e_grid(left = 55, right = 15, top = 30, bottom = 50) |>
        e_padrao()
    })
  })
}
