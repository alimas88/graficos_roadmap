library(tidyverse)
library(arrow)

dicionario_geracao <- jsonlite::read_json(
  "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/geracao_usina_2_ho/DicionarioDados_GeracaoPorUsina.json",
  simplifyVector = T
  )$dicionario_simplificado

anos <- 2000:2024
meses <- 1:12

caminho_geral <- "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/geracao_usina_2_ho/GERACAO_USINA-2_"

url_anos <- glue::glue("{caminho_geral}{2000:2021}.parquet")


url_mensais <- expand_grid(ano = 2022:2024, meses = 1:12) |>
  mutate(url = glue::glue("{caminho_geral}{ano}_{str_pad(meses, 2,  pad = '0')}.parquet")) |> 
  pull(url)

urls_gerais <- c(url_anos, url_mensais)

dir.create("ons_parquet")

walk(urls_gerais, \(u){
  download.file(u, file.path("ons_parquet", basename(u)), mode="wb")
})

geracao <- open_dataset("ons_parquet") |>
  collect()

geracao_sum <- geracao |> 
  mutate(
    din_instante = with_tz(din_instante, 'UTC'),
    ano = floor_date(din_instante, 'year'),
    val_geracao = as.numeric(val_geracao)
  ) |> 
  filter(nom_tipousina != "BOMBEAMENTO") |>
  group_by(ano, nom_tipousina, nom_usina) |> 
  summarise(
    geracao_mwmed = mean(val_geracao, na.rm = TRUE),
    .groups = 'drop'
  )

geracao_mes <- geracao_sum |> 
  mutate(mes = lubridate::with_tz(mes, 'UTC'))



ggplot(geracao_teste_sum, aes(mes, geracao_mwmed)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~nom_tipousina, scales = "free_y", ncol = 1)
  


################ teste Geração ############

geracao_sum <- open_dataset(
  "C:/Users/alfredo.silva/OneDrive - epe.gov.br/Roadmap Resiliência/ons_parquet"
  ) |>
  filter(val_geracao != '') |> 
  mutate(
    # Garantir tipo numérico
    val_geracao = round(as.numeric(val_geracao), 2),
    din_instante = lubridate::with_tz(din_instante, 'UTC'),
    # Truncar para mês (equivalente ao floor_date)
    mes = lubridate::floor_date(din_instante, "month")
  ) |>
  filter(nom_tipousina != "BOMBEAMENTO") |>
  group_by(mes, nom_tipousina, nom_usina) |>
  summarise(
    geracao_mwmed = round(mean(val_geracao, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  collect()

geracao_mes <- geracao_sum |> 
  group_by(mes, nom_tipousina) |> 
  summarise(
    geracao_mwmed = sum(geracao_mwmed),
    .groups = 'drop'
  )

ggplot(geracao_mes, aes(mes, geracao_mwmed)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~nom_tipousina, scales = "free_y", ncol = 1)



# teste CSV

caminho_csv <- "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/geracao_usina_2_ho/GERACAO_USINA-2_2000.csv"

geracao_csv <- read_delim(caminho_csv, locale = locale(decimal_mark = '.'))

geracao_csv_sum <- geracao_csv |> 
  mutate(
    val_geracao = as.numeric(val_geracao),
    mes = lubridate::floor_date(din_instante, "month")
  ) |> 
  filter(
    nom_tipousina == "TÉRMICA",
    mes == as.Date('2000-01-01')
    ) |>
  group_by(mes, nom_tipousina, nom_usina) |>
  summarise(
    geracao_mwh = round(sum(val_geracao, na.rm = TRUE), 2),
    horas = n(),
    geracao_mwmed = round(geracao_mwh / horas, 2),
    .groups = "drop"
  ) 


j_lacerda <- geracao_csv |> 
  filter(str_detect(nom_usina, 'Lacerda II')) |> 
  mutate(dia = floor_date(din_instante, 'day'),
         val_geracao = as.numeric(val_geracao)) |> 
  filter(dia <= as.Date('2000-01-31')) |> 
  group_by(dia, nom_usina) |> 
  summarise(
    geracao_mwh = round(sum(val_geracao, na.rm = TRUE), 2),
    horas = n(),
    geracao_mwmed = round(geracao_mwh / horas, 2),
    .groups = "drop"
  )

