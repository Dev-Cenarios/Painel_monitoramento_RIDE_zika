# ==============================================================================
# Zika RIDE | Modulo: KPIs
#
#   1. casos (do filtro) no ano corrente x mesmo periodo do ano anterior
#   2. casos (do filtro) nas ultimas 52 semanas x as 52 anteriores - com tao
#      poucos casos, 4 semanas (como no chikungunya) seria quase sempre zero
#   3. confirmados no ano e % por laboratorio
#   4. gestantes entre os casos provaveis do ano (e quantas confirmadas)
#   5. notificacoes do ano descartadas e ainda em investigacao
#
# 3 a 5 ignoram o filtro de classificacao de proposito: respondem sempre
# sobre o recorte geografico.
# ==============================================================================

mod_kpi_ui <- function(id) shiny::uiOutput(shiny::NS(id)("boxes"))

mod_kpi_server <- function(id, geo, classe, serie) {
  shiny::moduleServer(id, function(input, output, session) {

    tom <- function(bg, claro = FALSE) {
      bslib::value_box_theme(bg = bg, fg = if (claro) PALETA[["tinta"]] else "#FFFFFF")
    }

    output$boxes <- shiny::renderUI({
      s <- serie()
      shiny::req(nrow(s) > 0)

      ult_se   <- max(s$se)
      ano_cor  <- ult_se %/% 100L
      sem_cor  <- ult_se %% 100L
      no_ano   <- s$ano == ano_cor
      ant_per  <- s$ano == ano_cor - 1L & s$semana <= sem_cor
      casos_ano <- sum(s$casos[no_ano])
      casos_ant <- sum(s$casos[ant_per])
      var_ano   <- if (casos_ant > 0) casos_ano / casos_ant - 1 else NA_real_

      n     <- nrow(s)
      ult52 <- sum(s$casos[max(1, n - 51):n])
      ant52 <- sum(s$casos[max(1, n - 103):max(1, n - 52)])
      var52 <- if (ant52 > 0) ult52 / ant52 - 1 else NA_real_

      d_ano <- filtrar(geo())
      d_ano <- d_ano[d_ano$ano == ano_cor, , drop = FALSE]
      tot   <- sum(d_ano$casos)
      conf  <- sum(d_ano$casos[d_ano$classe %in% CONFIRMADOS])
      lab   <- sum(d_ano$casos[d_ano$classe == "confirmado_lab"])
      prov  <- d_ano[d_ano$classe %in% PROVAVEIS, , drop = FALSE]
      gest  <- sum(prov$gestantes)
      gest_conf <- sum(prov$gestantes[prov$classe %in% CONFIRMADOS])
      desc  <- sum(d_ano$casos[d_ano$classe == "descartado"])
      inv   <- sum(d_ano$casos[d_ano$classe == "em_investigacao"])

      seta <- function(v) {
        if (is.na(v)) return("")
        sprintf("%s %s", if (v >= 0) "▲" else "▼", fmt_pct(abs(v), 0))
      }

      bslib::layout_column_wrap(
        width = "190px", fill = FALSE, class = "mb-3",
        bslib::value_box(
          title = sprintf("%s em %d", rotulo_filtro_classe(classe()), ano_cor),
          value = fmt_int(casos_ano),
          shiny::tags$small(sprintf("%s vs. mesmo período de %d (%s)",
                                    seta(var_ano), ano_cor - 1L, fmt_int(casos_ant))),
          theme = tom(TONS_DOENCA[["600"]])
        ),
        bslib::value_box(
          title = "Últimas 52 semanas",
          value = fmt_int(ult52),
          shiny::tags$small(sprintf("%s vs. 52 semanas anteriores (%s)", seta(var52), fmt_int(ant52))),
          theme = if (!is.na(var52) && var52 > 0.5 && ult52 >= 10) tom(TONS_DOENCA[["800"]])
                  else tom(TONS_DOENCA[["100"]], claro = TRUE)
        ),
        bslib::value_box(
          title = sprintf("Confirmados em %d", ano_cor),
          value = fmt_int(conf),
          shiny::tags$small(if (conf) sprintf("%s por laboratório", fmt_pct(lab / conf, 0))
                            else "nenhum caso confirmado no ano"),
          theme = tom(TONS_AZUL[["700"]])
        ),
        bslib::value_box(
          title = sprintf("Gestantes em %d", ano_cor),
          value = fmt_int(gest),
          shiny::tags$small(sprintf("entre os casos prováveis · %s confirmada%s",
                                    fmt_int(gest_conf), if (gest_conf == 1) "" else "s")),
          theme = tom(PALETA[["critico"]])
        ),
        bslib::value_box(
          title = sprintf("Descartadas em %d", ano_cor),
          value = fmt_pct(if (tot) desc / tot else NA, 0),
          shiny::tags$small(sprintf("%s de %s notificações · %s em investigação",
                                    fmt_int(desc), fmt_int(tot), fmt_int(inv))),
          theme = tom(TONS_AZUL[["100"]], claro = TRUE)
        )
      )
    })
  })
}
