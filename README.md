# Zika RIDE — Painel de monitoramento de zika

Painel Shiny (bslib + echarts4r) irmão do **dengue_ride**, **chik_ride** e
**gripe_ride**, com as notificações de zika de residentes nos 34 municípios
da RIDE-DF. Fonte: microdado do SINAN (arquivos ZIKABR do DATASUS), tabela
`public.sinan_zika` do banco do projeto.

## 1. Estrutura

```
zika_ride/
├── etl_zika_ride.R      # Postgres -> data/*.parquet (tudo agregado no banco)
├── global.R             # dados em memória, classificação, paleta, helpers
├── ui.R / server.R
├── R/
│   ├── mod_filtros.R    # recorte (RIDE-DF / Estado / Município) + "Casos"
│   ├── mod_kpi.R        # 5 indicadores do topo
│   ├── mod_mapa.R       # incidência / casos / gestantes por município
│   ├── mod_serie.R      # casos por mês ou semana, empilhados por classificação
│   ├── mod_rt.R         # Rt do InfoDengue (como no dengue_ride)
│   ├── mod_gestantes.R  # gestantes por ano: classificação e trimestre
│   ├── mod_heatmap.R    # sazonalidade ano x SE
│   ├── mod_perfil.R     # faixa etária x sexo (gestantes destacadas)
│   ├── mod_analises.R   # ranking, classificação por ano, achados calculados
│   └── utils_ui.R       # tema e cards
└── data/
    ├── zika_ride_df.parquet   # semana x município x classificação x faixa
    ├── zika_ride_pop.parquet  # população dos 34 municípios por ano
    ├── zika_ride_rt.parquet   # semana x município -> Rt do InfoDengue
    └── geo/MUN_{31,52,53}.geojson
```

## 2. Atualizar os dados

Precisa do `.Renviron` com `PG_HOST`, `PG_PORT`, `PG_DB`, `PG_USER`, `PG_PASS`
(o mesmo dos outros painéis; ver `.Renviron.example`).

```bash
cd /srv/shiny-server/zika_ride && Rscript etl_zika_ride.R && touch restart.txt
```

O ETL leva poucos segundos. Os Parquets **não são versionados**
(`.gitignore`): no servidor, rode o ETL depois do primeiro `git pull`.

Rodar local (RStudio): abrir a pasta, `shiny::runApp()`. Pacotes: shiny,
bslib, echarts4r, DT, dplyr, tidyr, arrow, jsonlite, sass, htmlwidgets (+ DBI e
RPostgres para o ETL).

## 3. Decisões de tratamento

| Tema | Decisão |
|---|---|
| Fonte | `public.sinan_zika`, não `silver.zika`: mesmo microdado com os nomes originais do SINAN, e o usuário do painel tem SELECT nela (no `silver`, não). |
| Caso | **Provável** = notificado − descartado (definição do MS). O seletor "Casos" troca para confirmados, confirmados por laboratório, em investigação, todas as notificações ou descartados. |
| Classificação | `CLASSI_FIN` 1 = confirmado (laboratório se `CRITERIO` = 1, senão clínico-epidemiológico); 2 descartado; 8 inconclusivo; vazio = em investigação. |
| Gestantes | `CS_GESTANT` 1–3 = trimestre; 4 = idade gestacional ignorada. Substituem o canal endêmico no painel. |
| Sem canal / nowcast | Desde 2021 a RIDE tem 0–2 notificações por semana: quartis históricos e correção de atraso seriam ruído. A série abre agregada por mês. |
| Rt | `rt` do InfoDengue (`"Info_Dengue".zika`), por município, como no dengue_ride; no estado e na RIDE, média ponderada pelos casos. **O InfoDengue parou de estimar zika em 11/02/2024** (Brasil todo): o gráfico mostra a série histórica até lá. |
| Município sem notificação | Água Fria de Goiás nunca notificou zika; a população dos 34 municípios vem em `zika_ride_pop.parquet` para ele aparecer no mapa e no denominador. Sem esse arquivo (ETL antigo) o app roda com 33. |
| Residência no DF | Todo 53xxxx vai para 5300108. |
| População | Estimativa anual do IBGE em `"Info_Dengue".dengue`; anos sem estimativa herdam o ano vizinho. |

## 4. Achados da exploratória (nov/2026)

Calculados pelo app a partir da base (aba **Análises**):

- **Epidemia de 2016**: 56% dos casos prováveis da série são de 2016–2017.
  Desde 2021, cerca de 41 prováveis por ano.
- **Surto de 2023 em Águas Lindas de Goiás**: 97 dos 106 prováveis da RIDE
  no ano (92%), 97% confirmados por laboratório.
- **Descarte**: em 2020, 98% das 2,5 mil notificações foram descartadas
  (92% do DF). Na série, 80% das notificações são descartadas.
- **Gestantes**: 899 notificadas, 77% descartadas, 203 prováveis, 83
  confirmadas; 26% das prováveis com IG informada no 1º trimestre.
- **Perfil**: 69% dos prováveis são mulheres; 62% têm 15–44 anos.
- **Sazonalidade**: 71% dos prováveis entre janeiro e maio; pico em março.
- **Gravidade**: nenhum óbito por zika na série; 92% dos confirmados com o
  campo preenchido são autóctones.

## 5. Próximos passos possíveis

- Cruzar com a notificação de síndrome congênita (RESP), se disponível.
- Se o InfoDengue voltar a publicar zika, o Rt se atualiza sozinho no ETL.
