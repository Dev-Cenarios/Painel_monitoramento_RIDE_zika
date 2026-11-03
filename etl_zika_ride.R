# ==============================================================================
# Zika RIDE | etl_zika_ride.R
# Extrai do PostgreSQL (public.sinan_zika, microdado SINAN de zika, um arquivo
# ZIKABRaa.dbc por ano) as notificacoes de residentes nos 34 municipios da
# RIDE-DF e grava um Parquet JA AGREGADO, unica fonte de dados lida pelo app:
#
#   data/zika_ride_df.parquet   semana x municipio x classificacao x faixa
#                               etaria -> notificacoes, gestantes (por
#                               trimestre), mulheres, autoctonia, obitos
#   data/zika_ride_pop.parquet  populacao dos 34 municipios por ano
#   data/zika_ride_rt.parquet   semana x municipio -> Rt e casos do
#                               InfoDengue ("Info_Dengue".zika)
#
# Nenhuma linha de microdado sai do banco: toda a agregacao roda no Postgres.
#
# Uso (working directory = raiz do projeto):  Rscript etl_zika_ride.R
#
# Por que public.sinan_zika e nao silver.zika: e o mesmo microdado do SINAN,
# com os nomes de coluna originais, e o usuario do painel tem SELECT nela (no
# silver nao).
#
# Diferente do painel de chikungunya, nao ha triangulo de atraso/nowcast: a
# RIDE tem de 0 a 2 notificacoes de zika por semana desde 2021, e corrigir
# atraso sobre isso so produziria ruido.
#
# Particularidades que o SQL abaixo trata:
#   - Todas as colunas sao texto; campo vazio vira '' (COALESCE), senao
#     (NULL = '1')::int da NULL e a soma de um grupo sai NULL em vez de 0.
#   - Municipio com 6 digitos. Todo 53xxxx vira o municipio 5300108.
#   - Idade no formato SINAN: 1o digito = unidade (1 hora, 2 dia, 3 mes,
#     4 ano), demais = valor. 4035 = 35 anos.
# ==============================================================================

library(DBI)
library(RPostgres)
library(arrow)
library(dplyr)

if (file.exists(".Renviron")) readRenviron(".Renviron")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("PG_HOST"),
  port     = as.integer(Sys.getenv("PG_PORT", "5432")),
  dbname   = Sys.getenv("PG_DB"),
  user     = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASS"),  # credenciais no .Renviron (ver .Renviron.example)
  bigint   = "numeric"
)

# Data minima de inicio de sintomas: a notificacao de zika no SINAN comeca em
# 2015 (doenca de notificacao compulsoria a partir de fev/2016).
DATA_MIN <- Sys.getenv("ZIKA_DATA_MIN", "2015-01-04")

# Codigos IBGE oficiais da RIDE-DF (LC 163/2018) - 34 municipios. Mesma lista
# do etl_ride.R do painel de dengue.
ride_codes_oficiais <- c(
  5300108,                                   # Distrito Federal (1)
  3104502, 3109303, 3109451, 3170404,        # Minas Gerais (4)
  5200100, 5200175, 5200258, 5200308, 5200605, 5200803, 5203203, 5204003,
  5205307, 5205497, 5205513, 5205802, 5206206, 5207907, 5208004, 5208608,
  5212501, 5213053, 5214606, 5215231, 5215603, 5217302, 5217609, 5219753,
  5220009, 5220686, 5221858, 5222203, 5222302  # Goias (29)
)
mun6 <- substr(ride_codes_oficiais, 1, 6)
filtro_mun <- sprintf(
  "(id_mn_resi IN (%s) OR id_mn_resi LIKE '53%%')",
  paste0("'", mun6, "'", collapse = ", ")
)

# ------------------------------------------------------------- consulta --
# Classificacao final (classi_fin) da ficha de zika do SINAN:
#   1 = confirmado -> confirmado_lab (criterio 1) ou confirmado_clin
#   2 = descartado
#   8 = inconclusivo
#   vazio = em investigacao
# Caso PROVAVEL (definicao do Ministerio da Saude) = notificado - descartado.
# Gestante: cs_gestant 1-3 = trimestre, 4 = idade gestacional ignorada.
sql <- sprintf("
WITH base AS (
  SELECT
    substr(dt_sin_pri, 1, 10)::date                       AS dt_sin,
    id_mn_resi,
    COALESCE(split_part(classi_fin, '.', 1), '')          AS cf,
    COALESCE(split_part(criterio, '.', 1), '')            AS crit,
    COALESCE(split_part(evolucao, '.', 1), '')            AS evol,
    COALESCE(split_part(tpautocto, '.', 1), '')           AS autoc,
    COALESCE(split_part(cs_gestant, '.', 1), '')          AS gest,
    COALESCE(cs_sexo, '')                                 AS sexo,
    NULLIF(split_part(nu_idade_n, '.', 1), '')            AS idade_txt
  FROM public.sinan_zika
  WHERE %s
    AND dt_sin_pri ~ '^[0-9]{4}-[0-9]{2}-[0-9]{2}'
    AND substr(dt_sin_pri, 1, 10) >= '%s'
),
classif AS (
  SELECT
    dt_sin - EXTRACT(DOW FROM dt_sin)::int                AS data_ini_se,
    CASE WHEN id_mn_resi LIKE '53%%' THEN '530010' ELSE id_mn_resi END AS mun6,
    CASE
      WHEN cf = '1' AND crit = '1' THEN 'confirmado_lab'
      WHEN cf = '1'                THEN 'confirmado_clin'
      WHEN cf = '2'                THEN 'descartado'
      WHEN cf = '8'                THEN 'inconclusivo'
      ELSE 'em_investigacao'
    END                                                   AS classe,
    CASE
      WHEN idade_txt IS NULL OR idade_txt !~ '^[1-4][0-9]{3}$' THEN 'ign'
      WHEN left(idade_txt, 1) <> '4'           THEN '0-4'
      WHEN substr(idade_txt, 2)::int < 5       THEN '0-4'
      WHEN substr(idade_txt, 2)::int < 15      THEN '5-14'
      WHEN substr(idade_txt, 2)::int < 30      THEN '15-29'
      WHEN substr(idade_txt, 2)::int < 45      THEN '30-44'
      WHEN substr(idade_txt, 2)::int < 60      THEN '45-59'
      ELSE '60+'
    END                                                   AS faixa_etaria,
    (evol = '2')::int                                     AS obito,
    (gest IN ('1', '2', '3', '4'))::int                   AS gestante,
    (gest = '1')::int                                     AS gest_1tri,
    (gest = '2')::int                                     AS gest_2tri,
    (gest = '3')::int                                     AS gest_3tri,
    (sexo = 'F')::int                                     AS feminino,
    (autoc = '1')::int                                    AS autoctone,
    (autoc = '2')::int                                    AS importado
  FROM base
)
SELECT data_ini_se, mun6, classe, faixa_etaria,
       count(*)        AS casos,
       sum(obito)      AS obitos,
       sum(gestante)   AS gestantes,
       sum(gest_1tri)  AS gest_1tri,
       sum(gest_2tri)  AS gest_2tri,
       sum(gest_3tri)  AS gest_3tri,
       sum(feminino)   AS feminino,
       sum(autoctone)  AS autoctones,
       sum(importado)  AS importados
FROM classif
GROUP BY data_ini_se, mun6, classe, faixa_etaria
", filtro_mun, DATA_MIN)

message("[ETL Zika] Agregando notificacoes da RIDE-DF no Postgres...")
t0 <- Sys.time()
agg <- DBI::dbGetQuery(con, sql)
message(sprintf("[ETL Zika] %d linhas agregadas (%s notificacoes) em %.1fs.",
                nrow(agg), format(sum(agg$casos), big.mark = ".", decimal.mark = ","),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# -------------------------------------------- populacao (InfoDengue) --
# O SINAN nao traz populacao. A mesma estimativa anual do IBGE que o painel de
# dengue usa ja esta no banco, em "Info_Dengue".dengue - um valor por
# municipio e ano. Anos sem estimativa repetem o ultimo ano disponivel.
message("[ETL Zika] Lendo populacao de Info_Dengue...")
pop <- DBI::dbGetQuery(con, sprintf("
SELECT municipio_geocodigo, municipio_nome, uf_ibge,
       EXTRACT(YEAR FROM data_ini_se)::int AS ano, max(pop) AS pop
FROM \"Info_Dengue\".dengue
WHERE municipio_geocodigo IN (%s)
GROUP BY 1, 2, 3, 4", paste(ride_codes_oficiais, collapse = ", ")))

# ------------------------------------------------------- Rt (InfoDengue) --
# Numero de reproducao efetivo estimado pelo InfoDengue, por municipio e
# semana - o mesmo `rt` do painel de dengue. Atencao: o InfoDengue parou de
# atualizar a zika em fev/2024 (no Brasil todo), entao a serie de Rt termina
# antes da de notificacoes.
message("[ETL Zika] Lendo Rt de Info_Dengue.zika...")
rt <- DBI::dbGetQuery(con, sprintf("
SELECT data_ini_se, municipio_geocodigo::int AS municipio_geocodigo,
       casos, rt, p_rt1
FROM \"Info_Dengue\".zika
WHERE municipio_geocodigo IN (%s) AND data_ini_se >= '%s'
ORDER BY 1, 2", paste(ride_codes_oficiais, collapse = ", "), DATA_MIN)) |>
  mutate(data_ini_se = as.Date(data_ini_se), casos = as.integer(casos),
         rt = as.numeric(rt), p_rt1 = as.numeric(p_rt1))

DBI::dbDisconnect(con)

# ----------------------------------------------- semana epidemiologica (MS) --
#' Semana epidemiologica a partir do domingo que a inicia.
#' A SE 1 de um ano e a semana (dom-sab) que contem o dia 4 de janeiro, ou
#' seja, o ano epidemiologico de uma semana e o ano da sua quarta-feira.
se_de_domingo <- function(domingo) {
  ano <- as.integer(format(domingo + 3L, "%Y"))
  jan4 <- as.Date(sprintf("%d-01-04", ano))
  inicio <- jan4 - as.integer(format(jan4, "%w"))
  semana <- as.integer((domingo - inicio) / 7) + 1L
  ano * 100L + semana
}

medidas <- c("casos", "obitos", "gestantes", "gest_1tri", "gest_2tri", "gest_3tri",
             "feminino", "autoctones", "importados")

agg <- agg |>
  mutate(
    data_ini_se = as.Date(data_ini_se),
    se          = se_de_domingo(data_ini_se),
    across(all_of(medidas), as.integer)
  )
agg$municipio_geocodigo <- ride_codes_oficiais[match(agg$mun6, mun6)]

pop_full <- pop |>
  mutate(municipio_geocodigo = as.integer(municipio_geocodigo)) |>
  group_by(municipio_geocodigo) |>
  tidyr::complete(ano = seq(min(ano), max(as.integer(format(max(agg$data_ini_se), "%Y")), max(ano)))) |>
  tidyr::fill(pop, municipio_nome, uf_ibge, .direction = "downup") |>
  ungroup()

agg <- agg |>
  mutate(ano_pop = as.integer(format(data_ini_se + 3L, "%Y"))) |>
  left_join(pop_full, by = c("municipio_geocodigo", "ano_pop" = "ano")) |>
  transmute(
    data_ini_se, se,
    municipio_geocodigo = as.integer(municipio_geocodigo),
    municipio_nome, uf_ibge = as.integer(uf_ibge),
    classe, faixa_etaria,
    across(all_of(medidas)),
    pop = as.numeric(pop)
  ) |>
  arrange(data_ini_se, municipio_geocodigo, classe, faixa_etaria)

sem_pop <- unique(agg$municipio_geocodigo[is.na(agg$pop)])
if (length(sem_pop)) warning("[ETL Zika] Municipios sem populacao: ", paste(sem_pop, collapse = ", "))

dir.create("data", showWarnings = FALSE, recursive = TRUE)
arrow::write_parquet(agg, "data/zika_ride_df.parquet")

# Populacao dos 34 municipios por ano, a parte: um municipio sem nenhuma
# notificacao de zika (Agua Fria de Goias) nao aparece na base agregada, mas
# precisa estar no mapa e no denominador da RIDE.
pop_full |>
  transmute(municipio_geocodigo, municipio_nome, uf_ibge = as.integer(uf_ibge),
            ano = as.integer(ano), pop = as.numeric(pop)) |>
  arrow::write_parquet("data/zika_ride_pop.parquet")

arrow::write_parquet(rt, "data/zika_ride_rt.parquet")

message(sprintf(
  "[ETL Zika] OK: %s notificacoes (%s provaveis), %s a %s, %d municipios. Parquet em data/.",
  format(sum(agg$casos), big.mark = ".", decimal.mark = ","),
  format(sum(agg$casos[agg$classe != "descartado"]), big.mark = ".", decimal.mark = ","),
  format(min(agg$data_ini_se), "%d/%m/%Y"), format(max(agg$data_ini_se), "%d/%m/%Y"),
  dplyr::n_distinct(agg$municipio_geocodigo)
))
