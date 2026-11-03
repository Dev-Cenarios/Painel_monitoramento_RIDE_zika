# ==============================================================================
# Zika RIDE | global.R
# Ambiente, dados (Parquet agregado do SINAN), dicionario geografico,
# classificacao final e helpers. Executado UMA vez, antes de ui.R e server.R.
#
# Mesma arquitetura dos paineis de chikungunya e gripe: o ETL entrega a base
# agregada (semana x municipio x classificacao x faixa etaria, poucos milhares
# de linhas) e ela fica inteira em memoria.
#
# Diferente do chikungunya, nao ha nowcast nem canal endemico: desde 2021 a
# RIDE tem de 0 a 2 notificacoes de zika por semana. O foco do painel e a
# serie historica, o descarte e as gestantes (risco de sindrome congenita).
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(echarts4r)
library(DT)

if (dir.exists("app_cache") && file.access("app_cache", 2) == 0) {
  options(sass.cache = sass::sass_file_cache(file.path("app_cache", "sass")))
}

# ------------------------------------------------------------ fonte de dados --
PARQUET_ZIKA <- Sys.getenv("ZIKA_PARQUET", file.path("data", "zika_ride_df.parquet"))
PARQUET_POP  <- Sys.getenv("ZIKA_PARQUET_POP", file.path("data", "zika_ride_pop.parquet"))

if (!file.exists(PARQUET_ZIKA)) {
  stop("Parquet nao encontrado em '", PARQUET_ZIKA, "'. Rode `Rscript etl_zika_ride.R` ",
       "(precisa do .Renviron com PG_*).", call. = FALSE)
}

MEDIDAS <- c("casos", "obitos", "gestantes", "gest_1tri", "gest_2tri", "gest_3tri",
             "feminino", "autoctones", "importados")

DADOS <- arrow::read_parquet(PARQUET_ZIKA) |>
  as.data.frame() |>
  dplyr::mutate(
    data_ini_se = as.Date(data_ini_se),
    se          = as.integer(se),
    ano         = se %/% 100L,
    semana      = se %%  100L,
    municipio_geocodigo = as.integer(municipio_geocodigo),
    uf_ibge     = as.integer(uf_ibge),
    dplyr::across(dplyr::all_of(MEDIDAS), ~ dplyr::coalesce(as.numeric(.x), 0))
  )

ROTULO_BASE <- Sys.getenv("ZIKA_ROTULO_BASE", "RIDE-DF")

message(sprintf("[Zika RIDE] Fonte: %s (%s notificações).",
                PARQUET_ZIKA, format(sum(DADOS$casos), big.mark = ".", decimal.mark = ",")))

# ------------------------------------------------- cobertura temporal --------
DATA_MIN <- min(DADOS$data_ini_se)
DATA_MAX <- max(DADOS$data_ini_se)
ANO_MIN  <- min(DADOS$ano)
ANO_MAX  <- max(DADOS$ano)

# ------------------------------------------------- dicionario geografico -----
UF_LOOKUP <- data.frame(
  uf_ibge  = c(31L, 52L, 53L),
  uf_sigla = c("MG", "GO", "DF"),
  uf_nome  = c("Minas Gerais", "Goiás", "Distrito Federal"),
  stringsAsFactors = FALSE
)

# Populacao por municipio e ano. O ETL grava a dos 34 municipios a parte
# (zika_ride_pop.parquet): ha municipio da RIDE sem NENHUMA notificacao de
# zika, que nao existe na base agregada. Sem esse arquivo (ETL antigo), cai na
# populacao que vem nas linhas da base. Nos dois casos a grade e completada
# com o ano vizinho mais proximo.
POP_BRUTA <- if (file.exists(PARQUET_POP)) {
  arrow::read_parquet(PARQUET_POP) |> as.data.frame() |>
    dplyr::mutate(municipio_geocodigo = as.integer(municipio_geocodigo),
                  uf_ibge = as.integer(uf_ibge), ano = as.integer(ano))
} else {
  DADOS |> dplyr::distinct(municipio_geocodigo, municipio_nome, uf_ibge, ano, pop)
}

POP_MUN_ANO <- POP_BRUTA |>
  dplyr::group_by(municipio_geocodigo, ano) |>
  dplyr::summarise(pop = max(pop, na.rm = TRUE), .groups = "drop") |>
  tidyr::complete(municipio_geocodigo, ano = seq(ANO_MIN, ANO_MAX)) |>
  dplyr::group_by(municipio_geocodigo) |>
  tidyr::fill(pop, .direction = "downup") |>
  dplyr::ungroup() |>
  dplyr::filter(ano >= ANO_MIN, ano <= ANO_MAX)

GEO_DICT <- POP_BRUTA |>
  dplyr::distinct(uf_ibge, municipio_geocodigo, municipio_nome) |>
  dplyr::left_join(UF_LOOKUP, by = "uf_ibge") |>
  dplyr::arrange(uf_nome, municipio_nome)

UF_DISPONIVEIS <- GEO_DICT |> dplyr::distinct(uf_ibge, uf_sigla, uf_nome) |> dplyr::arrange(uf_nome)

# ------------------------------------------------- classificacao final -------
# Ordem = ordem de empilhamento nos graficos (de baixo para cima). Confirmados
# na cor do painel (rosa-magenta), inconclusivo em ocre, em investigacao e
# descartado em neutros.
CLASSES <- data.frame(
  classe = c("confirmado_lab", "confirmado_clin", "inconclusivo", "em_investigacao", "descartado"),
  rotulo = c("Confirmado (laboratório)", "Confirmado (clínico-epid.)", "Inconclusivo",
             "Em investigação", "Descartado"),
  cor    = c("#A12659", "#EA86A8", "#C9A043", "#B8C4CC", "#E3E8EC"),
  stringsAsFactors = FALSE
)
COR_CLASSE    <- stats::setNames(CLASSES$cor, CLASSES$classe)
ROTULO_CLASSE <- stats::setNames(CLASSES$rotulo, CLASSES$classe)

CONFIRMADOS <- c("confirmado_lab", "confirmado_clin")
PROVAVEIS   <- setdiff(CLASSES$classe, "descartado")

# Opcoes do seletor "Casos" (rotulo -> chave). Padrao: casos PROVAVEIS
# (notificados menos descartados), a definicao do Ministerio da Saude.
FILTROS_CLASSE <- c(
  "Casos prováveis"                  = "provaveis",
  "Confirmados"                      = "confirmados",
  "Confirmados por laboratório"      = "confirmado_lab",
  "Em investigação"                  = "em_investigacao",
  "Todas as notificações"            = "todas",
  "Descartados"                      = "descartado"
)

classes_do_filtro <- function(chave) {
  switch(
    chave,
    provaveis   = PROVAVEIS,
    confirmados = CONFIRMADOS,
    todas       = CLASSES$classe,
    chave
  )
}

rotulo_filtro_classe <- function(chave) names(FILTROS_CLASSE)[FILTROS_CLASSE == chave]

FAIXAS <- c("0-4", "5-14", "15-29", "30-44", "45-59", "60+")
ROTULO_FAIXA <- c(`0-4` = "0–4", `5-14` = "5–14", `15-29` = "15–29", `30-44` = "30–44",
                  `45-59` = "45–59", `60+` = "60+", ign = "Ignorada")

# ------------------------------------------------------------------ paleta --
# Mesma interface dos paineis de dengue, chikungunya e gripe (azul Cenarios,
# Poppins/DM Sans). A rampa de magnitude e rosa-magenta, cor desta doenca -
# ColorBrewer RdPu no mapa, sequencial e segura para daltonismo.
TONS_AZUL <- c(
  `50`  = "#ECF7FE", `100` = "#CFE9F9", `200` = "#A8D5F1", `300` = "#7ABEE6",
  `400` = "#4FA4D3", `500` = "#3891C0", `600` = "#0E7BA8", `700` = "#00638C",
  `800` = "#004565", `900` = "#00324C"
)

TONS_DOENCA <- c(
  `50`  = "#FDF0F4", `100` = "#FAD9E4", `200` = "#F4B3C9", `300` = "#EA86A8",
  `400` = "#DB5C88", `500` = "#C43C6F", `600` = "#A12659", `700` = "#831C49",
  `800` = "#651439", `900` = "#470C28"
)

PALETA <- c(
  fundo       = "#F2F6F9",
  superficie  = "#FFFFFF",
  linha       = "#D8E3EA",
  tinta       = "#12222B",
  texto       = "#4A4A4A",
  sub         = "#7A8B95",
  neutro      = "#B8C4CC",
  azul        = TONS_AZUL[["500"]],
  azul_acao   = TONS_AZUL[["600"]],
  azul_titulo = TONS_AZUL[["700"]],
  doenca      = TONS_DOENCA[["600"]],
  doenca_forte = TONS_DOENCA[["800"]],
  ok          = "#17895B",
  atencao     = "#DCA744",
  grave       = "#C65D26",
  critico     = "#8F2639"
)

ESCALA_MAPA <- c("#FEEBE2", "#FCC5C0", "#FA9FB5", "#F768A1", "#C51B8A", "#7A0177")

FONTE_GRAFICO <- "'DM Sans', system-ui, sans-serif"

cor_alpha <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)[, 1]
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], alpha)
}

# ------------------------------------------------------------------- malhas --
# GeoJSON do IBGE (mesmos arquivos do painel de dengue) recortados aos
# municipios da base. Parse uma vez no start.
GEO_DIR <- file.path("data", "geo")

MALHA_RIDE <- local({
  codigos <- as.character(GEO_DICT$municipio_geocodigo)
  feicoes <- list()
  for (uf in sort(unique(GEO_DICT$uf_ibge))) {
    arq <- file.path(GEO_DIR, sprintf("MUN_%d.geojson", uf))
    if (!file.exists(arq)) next
    js <- jsonlite::fromJSON(arq, simplifyVector = FALSE)
    feicoes <- c(feicoes, Filter(function(f) as.character(f$properties$codarea) %in% codigos,
                                 js$features))
  }
  list(type = "FeatureCollection", features = feicoes)
})

message(sprintf("[Zika RIDE] Malha: %d de %d municípios.",
                length(MALHA_RIDE$features), nrow(GEO_DICT)))

# Contorno das UFs e da RIDE (gerado por prep/gerar_contorno.R), juntado ao
# FIM da malha do mapa: as feicoes extras sao desenhadas por ultimo, por cima
# dos municipios, e por serem parte do mesmo mapa acompanham zoom e arraste.
# Opcional: sem o arquivo, o mapa sai so com as bordas cinza dos municipios.
CONTORNO_RIDE <- local({
  arq <- file.path(GEO_DIR, "contorno_ride.geojson")
  if (!file.exists(arq)) return(list())
  Filter(function(f) is.character(f$properties$codarea),
         jsonlite::fromJSON(arq, simplifyVector = FALSE)$features)
})
CODIGOS_CONTORNO <- vapply(CONTORNO_RIDE, function(f) f$properties$codarea, "")
MALHA_MAPA <- list(type = "FeatureCollection",
                   features = c(MALHA_RIDE$features, CONTORNO_RIDE))

# --------------------------------------------------------- Rt (InfoDengue) --
# Rt por municipio e semana estimado pelo InfoDengue, o mesmo do painel de
# dengue. O InfoDengue parou de estimar a zika em fev/2024 (no Brasil todo):
# o grafico mostra a serie historica e diz onde ela acaba. Opcional: sem o
# Parquet (ETL antigo) o card de Rt avisa e o resto do painel funciona.
PARQUET_RT <- Sys.getenv("ZIKA_PARQUET_RT", file.path("data", "zika_ride_rt.parquet"))
RT_FONTE   <- "\"Info_Dengue\".zika"
RT_PERIODO_PADRAO <- "tudo"

RT_INFODENGUE <- if (file.exists(PARQUET_RT)) {
  arrow::read_parquet(PARQUET_RT) |>
    as.data.frame() |>
    dplyr::mutate(data_ini_se = as.Date(data_ini_se),
                  municipio_geocodigo = as.integer(municipio_geocodigo),
                  casos = as.numeric(casos), rt = as.numeric(rt))
} else NULL

RT_DATA_MAX <- if (is.null(RT_INFODENGUE)) NA else max(RT_INFODENGUE$data_ini_se)

#' Serie semanal de Rt do recorte, no formato do painel de dengue.
#'
#' `rt` e estimado POR MUNICIPIO e nao e aditivo entre areas. No municipio,
#' o valor direto; acima disso, a media ponderada pelos casos de cada
#' municipio (aproximacao) e o numero de municipios com Rt > 1 na semana
#' (bem definido em qualquer nivel - serve de contraprova).
serie_rt <- function(geo) {
  if (is.null(RT_INFODENGUE)) return(NULL)
  mun <- switch(
    geo$nivel,
    base      = GEO_DICT$municipio_geocodigo,
    uf        = GEO_DICT$municipio_geocodigo[GEO_DICT$uf_ibge == geo$uf_ibge],
    municipio = geo$geocodigo
  )
  d <- RT_INFODENGUE[RT_INFODENGUE$municipio_geocodigo %in% mun, , drop = FALSE] |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(
      # rt_num e rt_peso ANTES de `casos`: o summarise e sequencial e, depois
      # de casos = sum(casos), `casos` passaria a ser o total da semana.
      rt_num    = sum(rt * casos, na.rm = TRUE),
      rt_peso   = sum(casos[!is.na(rt)], na.rm = TRUE),
      casos     = sum(casos, na.rm = TRUE),
      rt_direto = suppressWarnings(max(rt, na.rm = TRUE)),
      n_mun     = dplyr::n_distinct(municipio_geocodigo),
      n_mun_rt1 = sum(rt > 1, na.rm = TRUE),
      .groups   = "drop"
    )
  d$rt <- if (identical(geo$nivel, "municipio")) {
    ifelse(is.finite(d$rt_direto), d$rt_direto, NA_real_)
  } else {
    ifelse(d$rt_peso > 0, d$rt_num / d$rt_peso, NA_real_)
  }
  d
}

# ----------------------------------------------------------------- helpers ---

#' Filtra a base pelo recorte geografico e pelo conjunto de classificacoes.
filtrar <- function(geo, classes = CLASSES$classe) {
  d <- DADOS[DADOS$classe %in% classes, , drop = FALSE]
  switch(
    geo$nivel,
    base      = d,
    uf        = d[d$uf_ibge == geo$uf_ibge, , drop = FALSE],
    municipio = d[d$municipio_geocodigo == geo$geocodigo, , drop = FALSE],
    stop("Nivel geografico desconhecido: ", geo$nivel, call. = FALSE)
  )
}

#' Populacao do recorte por ano (soma dos municipios).
pop_recorte <- function(geo) {
  mun <- switch(
    geo$nivel,
    base      = GEO_DICT$municipio_geocodigo,
    uf        = GEO_DICT$municipio_geocodigo[GEO_DICT$uf_ibge == geo$uf_ibge],
    municipio = geo$geocodigo
  )
  POP_MUN_ANO |>
    dplyr::filter(municipio_geocodigo %in% mun) |>
    dplyr::group_by(ano) |>
    dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
}

#' SE (AAAASS) de um domingo de inicio de semana - calendario do MS.
se_de_domingo <- function(dd) {
  ano <- as.integer(format(dd + 3L, "%Y"))
  jan4 <- as.Date(sprintf("%d-01-04", ano))
  ano * 100L + as.integer((dd - (jan4 - as.integer(format(jan4, "%w")))) / 7) + 1L
}

#' Grade completa de semanas (sem buraco quando nao ha caso - a regra aqui:
#' a maioria das semanas desde 2021 tem zero notificacao).
SEMANAS <- data.frame(data_ini_se = seq(DATA_MIN, DATA_MAX, by = 7))
SEMANAS$se <- se_de_domingo(SEMANAS$data_ini_se)

#' Serie semanal (total do recorte e classificacoes), com incidencia.
serie_semanal <- function(geo, classes) {
  d <- filtrar(geo, classes) |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(casos = sum(casos), obitos = sum(obitos),
                     gestantes = sum(gestantes), .groups = "drop")
  SEMANAS |>
    dplyr::left_join(d, by = "data_ini_se") |>
    dplyr::mutate(dplyr::across(c(casos, obitos, gestantes), ~ dplyr::coalesce(.x, 0)),
                  ano = se %/% 100L, semana = se %% 100L) |>
    dplyr::left_join(pop_recorte(geo), by = "ano") |>
    dplyr::mutate(incidencia = casos / pop * 1e5)
}

#' Expressao JS que formata "AAAA-MM-DD" como "dd/mm/aaaa" (sem new Date,
#' que interpreta como UTC e volta um dia em UTC-3).
JS_DATA_BR <- "(function(s){var p=String(s).split('-');return p[2]+'/'+p[1]+'/'+p[0];})"

fmt_int <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x)), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x), digits), big.mark = ".", decimal.mark = ",",
         nsmall = digits, scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x)) || !is.finite(x)) return("-")
  paste0(fmt_num(100 * x, digits), "%")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --------------------------------------------------------- modulos do app ----
for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)
