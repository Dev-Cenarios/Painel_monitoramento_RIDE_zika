# ==============================================================================
# Zika RIDE | ui.R
#
# Aba "Painel":
#   0. cabecalho
#   1. filtros (recorte geografico + classificacao dos casos)
#   2. KPIs
#   3. mapa (5) | evolucao por classificacao (7)
#   4. Rt do InfoDengue (largura total)
#   5. gestantes (7) | sazonalidade (5)
#   6. perfil (sexo x idade)
# Aba "Análises": ranking de municipios, classificacao por ano, achados.
# Aba "Sobre os dados": fonte, definicoes e limitacoes.
# ==============================================================================

bslib::page_navbar(
  title        = "Zika RIDE",
  theme        = TEMA_PAINEL,
  window_title = "Zika RIDE — Monitoramento de zika",
  padding      = "1rem",
  fillable     = FALSE,

  bslib::nav_panel(
    "Painel",
    painel_ui_assets(),

    shiny::tags$div(
      class = "d-flex justify-content-between align-items-end mb-3",
      shiny::tags$div(
        shiny::tags$h4(class = "mb-0 fw-semibold", "Zika RIDE"),
        shiny::tags$span(class = "text-muted small",
                         "Painel de monitoramento de zika · fonte SINAN")
      ),
      shiny::tags$span(
        class = "text-muted small text-end",
        sprintf("%d municípios · %d–%d · dados até a semana de %s",
                nrow(GEO_DICT), ANO_MIN, ANO_MAX, format(DATA_MAX, "%d/%m/%Y"))
      )
    ),

    bslib::card(
      class = "mb-3",
      bslib::card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        shiny::tags$span(class = "fw-semibold", "Recorte"),
        shiny::tags$span(class = "small", mod_filtros_resumo("filtros"))
      ),
      bslib::card_body(mod_filtros_ui("filtros"))
    ),

    mod_kpi_ui("kpi"),

    bslib::layout_columns(
      col_widths = c(5, 7),
      cartao_grafico("Distribuição espacial", mod_mapa_saida("mapa", "460px"),
                     mod_mapa_controles("mapa")),
      cartao_grafico("Casos por classificação final", mod_serie_saida("serie", "460px"),
                     mod_serie_controles("serie"),
                     rodape = "Início dos sintomas. Sem correção de atraso: com tão poucos casos por semana, o nowcast seria ruído.")
    ),

    cartao_grafico("Rt (InfoDengue) — número de reprodução efetivo", mod_rt_saida("rt", "320px"),
                   mod_rt_controles("rt"),
                   rodape = sprintf(
                     "Acima de 1 (linha tracejada), cada caso gera mais de um caso novo. No estado e na RIDE, média dos municípios ponderada pelos casos. O InfoDengue parou de estimar o Rt de zika em %s (no Brasil todo).",
                     if (is.na(RT_DATA_MAX)) "fev/2024" else format(RT_DATA_MAX, "%d/%m/%Y"))),

    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::navset_card_tab(
        full_screen = TRUE,
        title = shiny::tags$span(class = "fw-semibold", "Gestantes"),
        bslib::nav_panel("Por classificação",
                         bslib::card_body(class = "p-0", mod_gestantes_saida("gestantes", "classe"),
                                          shiny::tags$p(class = "small text-muted px-3",
                                                        "Todas as gestantes notificadas, independentemente do filtro \"Casos\"."))),
        bslib::nav_panel("Por trimestre",
                         bslib::card_body(class = "p-0", mod_gestantes_saida("gestantes", "trimestre"),
                                          shiny::tags$p(class = "small text-muted px-3",
                                                        "Gestantes do filtro \"Casos\", por idade gestacional na notificação.")))
      ),
      cartao_grafico("Sazonalidade — ano × semana epidemiológica",
                     mod_heatmap_saida("heatmap", "420px"), mod_heatmap_controles("heatmap"))
    ),

    cartao_grafico("Perfil — faixa etária × sexo", mod_perfil_saida("perfil"),
                   mod_perfil_controles("perfil"))
  ),

  bslib::nav_panel("Análises", mod_analises_ui("analises")),

  bslib::nav_panel(
    "Sobre os dados",
    bslib::card(
      bslib::card_body(
        class = "small",
        shiny::tags$h5("Fonte"),
        shiny::tags$p(
          "Microdados de zika do SINAN (arquivos ZIKABR do DATASUS), tabela ", shiny::tags$code("public.sinan_zika"),
          sprintf(" no banco do projeto. Notificações de residentes nos 34 municípios da RIDE-DF (LC 163/2018), de %s a %s. ",
                  format(DATA_MIN, "%d/%m/%Y"), format(DATA_MAX, "%d/%m/%Y")),
          "O ETL (", shiny::tags$code("etl_zika_ride.R"), ") agrega tudo no Postgres; o painel não lê microdado."
        ),
        shiny::tags$h5("Definições"),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$b("Semana:"), " semana epidemiológica (domingo a sábado) do início dos sintomas. Na visão mensal, a semana vai para o mês da sua quarta-feira."),
          shiny::tags$li(shiny::tags$b("Caso provável:"), " toda notificação, exceto as descartadas (definição do Ministério da Saúde)."),
          shiny::tags$li(shiny::tags$b("Classificação:"), " CLASSI_FIN = 1 é zika confirmada - por laboratório (CRITERIO = 1) ou clínico-epidemiológico; 2 descartado; 8 inconclusivo; vazio = em investigação."),
          shiny::tags$li(shiny::tags$b("Gestante:"), " CS_GESTANT 1, 2 e 3 = 1º, 2º e 3º trimestre; 4 = idade gestacional ignorada."),
          shiny::tags$li(shiny::tags$b("Incidência:"), " casos por 100 mil habitantes, com a estimativa populacional anual do IBGE usada pelo InfoDengue."),
          shiny::tags$li(shiny::tags$b("DF:"), " residência 53xxxx somada no município Brasília (5300108).")
        ),
        shiny::tags$h5("Por que não há canal endêmico nem nowcast"),
        shiny::tags$p("Desde 2021 a RIDE tem de 0 a 2 notificações de zika por semana. Quartis históricos e correção de atraso sobre esses números produziriam faixas sem sentido; no lugar do canal, o painel mostra as gestantes notificadas."),
        shiny::tags$h5("Rt"),
        shiny::tags$p("Número de reprodução efetivo estimado pelo InfoDengue para cada município (tabela ", shiny::tags$code("\"Info_Dengue\".zika"),
                      "), o mesmo usado no painel de dengue. No município é o valor direto; no estado e na RIDE, média ponderada pelos casos de cada município - aproximação, porque Rt não se soma entre áreas. A série do InfoDengue para zika termina em fevereiro de 2024; com 0 a 2 casos por semana, o Rt fica 0 na maioria das semanas e oscila muito quando há caso."),
        shiny::tags$h5("Limitações"),
        shiny::tags$ul(
          shiny::tags$li("Zika, dengue e chikungunya têm sintomas parecidos e o mesmo vetor; sem exame, a classificação é incerta e boa parte das suspeitas é descartada ou reclassificada."),
          shiny::tags$li("A sorologia IgM de zika tem reação cruzada com dengue; confirmações por laboratório antes de 2017 dependem muito de qual teste foi usado."),
          shiny::tags$li("A síndrome congênita associada ao zika (microcefalia) é notificada em outro sistema (RESP) e não está aqui."),
          shiny::tags$li("Notificações recentes ainda em investigação vão virar confirmadas ou descartadas.")
        )
      )
    )
  )
)
