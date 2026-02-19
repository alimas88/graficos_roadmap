library(tidyverse)
library(duckdb)
library(arrow)

dicionario_cmo <- jsonlite::read_json(
  'https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/DicionarioDados_Cmo_Semanal.json',
  simplifyVector = TRUE 
  )$dicionario_simplificado

caminho_cmo_semanal <- glue::glue(
  "s3://ons-aws-prod-opendata/dataset/cmo_se/CMO_SEMANAL_{2005:2024}.parquet"
)

cmo_semanal_arrow <- open_dataset(caminho_cmo_semanal) |>
  mutate(across(contains('val'), as.numeric)) |>
  select(!c(val_cmoleve, val_cmomedia, val_cmopesada)) |>
  mutate(
    nom_subsistema = forcats::fct_relevel(nom_subsistema, 'SUDESTE', 'NORDESTE', 'SUL', 'NORTE')
  ) |>
  collect()


cmo_semanal_arrow <- dados |> 
  mutate(across(contains('val'), as.numeric)) |> 
  select(!c(val_cmoleve, val_cmomedia, val_cmopesada)) |> 
  mutate(
    nom_subsistema = forcats::fct_relevel(
      nom_subsistema,
      'SUDESTE', 'NORDESTE', 'SUL', 'NORTE'
    )
  )


ggplot(cmo_semanal, aes(din_instante)) +
  geom_rect(
    data = crises,
    aes(
      xmin = inicio,
      xmax = fim,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey80"
  ) +
  geom_line(aes(y = val_cmomediasemanal), 
            colour = "#08519C",
            linewidth = 0.8) +
  facet_wrap(~ nom_subsistema, ncol = 1, scales = 'free_y') +
  escala_x_tempo +
  labs(
    title = 'CMO Médio Semanal por Subsistema',
    x = 'Ano',
    y = 'CMO (R$/MWh)'
  ) + 
  tema_comum



# teste DuckDB
con <- dbConnect(duckdb())

dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")


cmo_parquet <- tbl(
  con,
  "read_parquet('s3://ons-aws-prod-opendata/dataset/cmo_se/*.parquet')"
) |> collect() |> 
  mutate(dia = as.Date(din_instante)) |> 
  filter(dia <= as.Date('2024-12-31'))

cmo_semanal <- cmo_parquet |>
  select(!c(val_cmoleve, val_cmomedia, val_cmopesada)) |>
  mutate(
    across(contains('val'), as.numeric)
  ) |> 
  collect() |> 
  mutate(
    nom_subsistema = forcats::fct_relevel(nom_subsistema, 'SUDESTE', 'NORDESTE', 'SUL', 'NORTE')
  )
  




caminho_cmo_semanal <- glue::glue(
  "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/CMO_SEMANAL_{2005:2024}.csv"
)

dados_csv <- caminho_cmo_semanal |> 
  map(\(x) read_delim(x, locale = locale(decimal_mark = "."))) |> 
  map(\(df) dplyr::mutate(
    df,
    dplyr::across(contains("val"), as.numeric)
  ))

dados_csv <- dados_csv |> 
  bind_rows()
