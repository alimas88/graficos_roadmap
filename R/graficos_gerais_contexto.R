library(tidyverse)
library(duckdb)
library(patchwork)
library(arrow)
library(scales)

# EXTRAÇÃO DOS DADOS ----

## CAPACIDADE INSTALADA ----

# dados de capacidade instalada do ONS
cap_inst_ons <- read_delim(
  'https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/capacidade-geracao/CAPACIDADE_GERACAO.csv',
  delim = ';',
  locale = locale(decimal_mark = '.')
)

# dados de GD retirados das planilhas do BEN
gd <- tibble::tibble(
  ano = 2015:2024,
  nom_tipousina = "GD",
  potencia = c(
    17, 72, 246, 670, 2162,
    4768, 8965, 17325, 26627, 36168
  )
)


cap_long <- cap_inst_ons |> 
  mutate(ano_inicio = lubridate::year(dat_entradaoperacao),
         ano_fim = if_else(
           is.na(dat_desativacao), year(Sys.time()),
           lubridate::year(dat_desativacao)))


cap_por_ano <- cap_long |>
  rowwise() |>
  mutate(ano = list(seq(ano_inicio, ano_fim))) |>
  unnest(ano) |>
  ungroup() |>
  summarise(potencia = sum(val_potenciaefetiva),
            .by = c(ano, nom_tipousina)) |>
  bind_rows(gd) |>
  filter(ano >= 2000, ano <= 2024, ano %% 2 == 0) |>
  mutate(
    nom_tipousina = dplyr::recode(nom_tipousina, "EOLIELÉTRICA" = "EÓLICA"),
    nom_tipousina = forcats::fct_relevel(
      nom_tipousina,
      "HIDROELÉTRICA",
      "TÉRMICA",
      "NUCLEAR",
      "EÓLICA",
      "FOTOVOLTAICA",
      "GD" # topo
    ),
    nom_tipousina = forcats::fct_rev(nom_tipousina),
    ano = case_when(
      ano == 2000 ~ as.Date(paste0(ano, "-01-01")), 
      TRUE ~ as.Date(paste0(ano, "-12-31")))
  )


## TRANSMISSÃO ----

linhas_transmissao <- read_delim(
  'https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/linha_transmissao/LINHA_TRANSMISSAO.csv',
  delim = ';',
  locale = locale(decimal_mark = '.')
) |>
  mutate(across(contains('dat'), as.Date), 
         across(contains('val'), as.numeric)
  )

evolucao_trans <- linhas_transmissao |> 
  mutate(ano_inicio = year(dat_entradaoperacao),
         ano_fim = if_else(
           is.na(dat_desativacao), year(Sys.Date()),
           year(dat_desativacao)
         )) |> 
  rowwise() |> 
  mutate(anos = list(seq(ano_inicio, ano_fim))) |> # lista de anos com a LT em operação
  unnest(anos) |> 
  ungroup() |> 
  mutate(
    val_niveltensao_kv = case_when(
      val_niveltensao_kv %in% c(500, 525) ~ '500/525',
      TRUE ~ as.character(val_niveltensao_kv)
    )) |> 
  select(c(nom_linhadetransmissao, val_comprimento, val_niveltensao_kv, anos)) |> 
  summarise(extensao = sum(val_comprimento, na.rm = T),
            .by = c(anos, val_niveltensao_kv)) |> 
  filter(anos >= 2000,
         anos <= 2024,
         anos %% 2 == 0) |> 
  mutate(
    val_niveltensao_kv = forcats::fct_relevel(val_niveltensao_kv, '800', '765', '600', '500/525', '440', '345'),
    anos_date = case_when(
      anos == 2000 ~ as.Date(paste0(anos, "-01-01")),
      anos == 2024 ~ as.Date(paste0(anos, "-12-31")),
      TRUE ~ as.Date(paste0(anos, "-01-01"))
    )
  )

## ENA ----

# dicionário de dados do ENA Diário por Subsistema
dicionario_ena <- jsonlite::read_json(
  "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/DicionarioDados_EnaPorSubsistema.json",
  simplifyVector = T
)$dicionario_simplificado

# intervalo de anos para criar as urls
anos <- 2000:2024
camiho_generico_ena <- "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_"

# leitura de todas as urls
ena_subsistema <- map_chr(anos, \(x) glue::glue("{camiho_generico_ena}{x}.csv")) |> 
  map(\(x) read_delim(x, locale = locale(decimal_mark = '.'))) |> 
  bind_rows()

# ena anual
ena_anual <- ena_subsistema |>
  mutate(
    mlt_dia = 100 * ena_bruta_regiao_mwmed / ena_bruta_regiao_percentualmlt,
    mes = month(ena_data),
    ano = year(ena_data)
  ) |>
  summarise(
    ena_mwmed = mean(ena_bruta_regiao_mwmed),
    mlt_mes = mean(mlt_dia),
    ena_perc_mlt = 100 * ena_mwmed / mlt_mes,
    .by = c(nom_subsistema, ano)
  )

# ena mensal  
ena_mensal <- ena_subsistema |>
  mutate(
    mlt_dia = 100 * ena_bruta_regiao_mwmed / ena_bruta_regiao_percentualmlt,
    mes = floor_date(ena_data, 'month')
  ) |>
  summarise(
    ena_mwmed = mean(ena_bruta_regiao_mwmed),
    mlt_mes = mean(mlt_dia),
    ena_perc_mlt = 100 * ena_mwmed / mlt_mes,
    .by = c(nom_subsistema, mes)
  ) |> 
  mutate(
    nom_subsistema = forcats::fct_relevel(nom_subsistema, 'SUDESTE', 'NORDESTE', 'SUL', 'NORTE')
  )

## EAR ----

# dicionário de dados do EAR Diário por Subsistema
dicionario_ear <- jsonlite::read_json(
  "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/DicionarioDados_EarPorSubsistema.json",
  simplifyVector = T
)$dicionario_simplificado

# intervalo de anos para criar as urls
anos <- 2000:2024
camiho_generico_ear <- "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_"


# leaitura de todas as urls
ear_subsistema <- map_chr(anos, \(x) glue::glue("{camiho_generico_ear}{x}.csv")) |>
  map(\(x) read_delim(x, locale = locale(decimal_mark = '.'))) |>
  bind_rows()


# transformação - redução da temporalidade para reduzir a variabilidade
ear_percentagem <- ear_subsistema |> 
  mutate(
    p10 = quantile(ear_verif_subsistema_percentual, 0.10, rm.na = TRUE),
    semana = floor_date(ear_data, "week", week_start = 1),
    .by = nom_subsistema
  ) |>
  summarise(
    ear_sub_perc = min(ear_verif_subsistema_percentual, na.rm = TRUE),
    p10 = unique(p10),
    .by = c(nom_subsistema, semana)
  ) |>
  arrange(nom_subsistema, semana)


# EAR com SIN
ear_percentagem <- ear_subsistema |>
  summarise(
    nom_subsistema = 'SIN',
    ear_max_subsistema = sum(ear_max_subsistema),
    ear_verif_subsistema_mwmes = sum(ear_verif_subsistema_mwmes),
    ear_verif_subsistema_percentual = round(100 * ear_verif_subsistema_mwmes / ear_max_subsistema, 2),
    .by = ear_data
  ) |>
  bind_rows(ear_subsistema) |>
  mutate(
    p10 = quantile(ear_verif_subsistema_percentual, 0.10, rm.na = TRUE),
    mes = floor_date(ear_data, "month"),
    .by = nom_subsistema
  ) |>
  summarise(
    ear_sub_perc = min(ear_verif_subsistema_percentual, na.rm = TRUE),
    p10 = unique(p10),
    .by = c(nom_subsistema, mes)
  ) |>
  arrange(nom_subsistema, mes) |>
  mutate(
    nom_subsistema = forcats::fct_relevel(nom_subsistema, 'SIN', 'SUDESTE', 'NORDESTE', 'SUL', 'NORTE')
  ) |> 
  filter(nom_subsistema != 'SIN')


## GERAÇÃO ----

# os dados estão sendo lidos da máquina devido ao tamanho dos arquivos
geracao_sum <- open_dataset(
  "C:/Users/alfredo.silva/OneDrive - epe.gov.br/Roadmap Resiliência/ons_parquet"
) |>
  filter(val_geracao != '') |> 
  mutate(
    val_geracao = round(as.numeric(val_geracao), 2),
    din_instante = lubridate::with_tz(din_instante, 'UTC'),
    mes = lubridate::floor_date(din_instante, "month")
  ) |>
  filter(nom_tipousina != "BOMBEAMENTO") |>
  group_by(mes, nom_tipousina, nom_usina) |>
  summarise(
    geracao_mwmed = round(mean(val_geracao, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  collect()

# agrupar por mês
geracao_mes <- geracao_sum |>
  mutate(
    nom_tipousina = dplyr::recode(nom_tipousina, "EOLIELÉTRICA" = "EÓLICA"),
    nom_tipousina = forcats::fct_relevel(
      nom_tipousina,
      "HIDROELÉTRICA",
      "TÉRMICA",
      "NUCLEAR",
      "EÓLICA",
      "FOTOVOLTAICA"
    )
  ) |>
  group_by(mes, nom_tipousina) |>
  summarise(geracao_mwmed = sum(geracao_mwmed), .groups = 'drop') |>
  filter(nom_tipousina %in% c("HIDROELÉTRICA", "TÉRMICA"))


## CMO Semanal ----

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



# GRÁFICOS ----


## PARÂMETROS GERAIS PARA OS GRÁFICOS ----

# tema comum
tema_comum <- theme_minimal() +
  theme(
    # Margens e eixos
    plot.margin = margin(5, 5, 5, 5, unit = "mm"),
    axis.title = element_text(margin = margin(t = 6),
                                size = 8),
    axis.text = element_text(size = 7),
    
    # linhas de grade
    panel.grid.minor = element_blank(),
    
    # Título e legenda
    plot.title = element_text(hjust = 0.5, 
                              size = 9),
    legend.position = "right",
    
    # Facets
    strip.text = element_text(hjust = 0),
    
    # Caption
    plot.caption = element_text(
      hjust = 1,
      size = 7,
      colour = "grey40",
      margin = margin(t = 6)
    ),
    plot.title.position = "plot",
    plot.caption.position = "panel",
    
    # label 
    legend.key.height = unit(4, "mm"),
    legend.key.width  = unit(5, "mm"),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8)
  )

# escala de tempo no eixo x - deixar todos os gráfcos com mesma divisão temporal
escala_x_tempo <- scale_x_date(
  limits = c(as.Date("2000-01-01"), as.Date("2024-12-31")),
  date_labels = "%Y",
  breaks = seq.Date(
    from = as.Date("2000-01-01"),
    to   = as.Date("2024-12-31"),
    by   = "4 years"
  ),
  expand = c(0, 0)
)

# crises de escassez hídrica
crises <- tibble(
  inicio = as.Date(c("2001-01-01", "2012-10-01", "2020-12-01")),
  fim    = as.Date(c("2002-02-28", "2016-12-31", "2022-04-30"))
)

## GRÁFICOS CAPACIDADE INSTALADA ----

pal_usinas <- c(
  "HIDROELÉTRICA" = "#4DA3D9",
  "TÉRMICA" = "#6B3E26",
  "EÓLICA" = "#8FD19E",
  "FOTOVOLTAICA" = "#F2C94C",
  "NUCLEAR" = "#C0392B",
  "GD" = "#F29900"
)

# gráfico de área
p1 <- ggplot(cap_por_ano, aes(x = ano, y = potencia / 1000, fill = nom_tipousina)) +
  
  geom_area(color = "white",
            linewidth = 0.3,
            alpha = 0.95) +
  geom_rect(
    data = crises,
    aes(
      xmin = inicio,
      xmax = fim,
      ymin = 0,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    show.legend = FALSE,
    alpha = 0.3 
    
  ) +
  scale_fill_manual(values = pal_usinas) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    ),
    expand = expansion(mult = c(0, 0.02))
  ) +
  escala_x_tempo +
  labs(
    title = "Evolução da Capacidade Instalada por Tipo de Usina",
    x = "Ano",
    y = "Potência Instalada (GW)",
    fill = "Tipo de Usina"
  ) +
  tema_comum

## GRÁFICOS TRANSMISSÃO ----

# paleta de cores para os níveis de tensão
paleta_tensao <- c(
  "800"     = "#0B4D6E",  # azul petróleo
  "765"     = "#000000",  # preto
  "600"     = "#B5A800",  # amarelo/oliva
  "500/525" = "#E41A1C",  # vermelho
  "440"     = "#984EA3",  # roxo
  "345"     = "#08519C",  # azul
  "230"     = "#238B45"   # verde
)


p2 <- ggplot(evolucao_trans,
       aes(x = anos_date, y = extensao / 1000, fill = val_niveltensao_kv)) +
  
  geom_area() +
  geom_rect(
    data = crises,
    aes(
      xmin = inicio,
      xmax = fim,
      ymin = 0,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    alpha = 0.3,
    show.legend = F
  ) +
  escala_x_tempo +
  scale_y_continuous(labels = scales::label_number_auto()) +
  scale_fill_manual(values = paleta_tensao) +
  labs(
    x = "Ano",
    y = "Extensão acumulada (mil km)",
    fill = "Tensão (kV)",
    title = "Evolução Física das Linhas de Transmissão",
    caption = "*Faixas cinzas indicam períodos aproximados das crises no setor."
  ) +
  tema_comum




##- GRÁFICOS EAR ----

ggplot(ear_percentagem, aes(x = mes)) +
  geom_rect(
    data = crises,
    aes(
      xmin = inicio, xmax = fim,
      ymin = -Inf, ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.5
  ) +
  geom_line(
    aes(y = ear_sub_perc, colour = "EAR"),
    linewidth = 0.9
  ) +
  geom_line(
    aes(y = p10, colour = "p10"),
    linetype = "dashed"
  ) +
  facet_wrap(~ nom_subsistema, ncol = 1) +
  escala_x_tempo +
  scale_colour_manual(
    name = NULL,  # remove "colour"
    values = c("EAR" = "#08519C", "p10" = "black")
  ) +
  labs(
    title = "Percentagem de EAR por subsistema",
    x = 'Ano',
    y = "EAR (%)",
    caption = "*Áreas sombreadas indicam períodos de escassez"
  ) +
  tema_comum


# EAR COM SIN
ggplot(ear_percentagem, aes(x = mes)) +
  geom_rect(
    data = crises,
    aes(
      xmin = inicio, xmax = fim,
      ymin = -Inf, ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.5
  ) +
  geom_line(
    aes(y = ear_sub_perc, colour = "EAR"),
    linewidth = 0.9
  ) +
  geom_line(
    aes(y = p10, colour = "p10"),
    linetype = "dashed"
  ) +
  facet_wrap(~ nom_subsistema, ncol = 1) +
  escala_x_tempo +
  scale_colour_manual(
    name = NULL,  # remove "colour"
    values = c("EAR" = "#08519C", "p10" = "black")
  ) +
  labs(
    title = "Percentagem de EAR por subsistema",
    x = 'Ano',
    y = "EAR (%)",
    caption = "*Áreas sombreadas indicam períodos de escassez"
  ) +
  tema_comum



## GRÁFICOS ENA ----


ggplot(ena_mensal, aes(x = mes)) +
  
  ## Períodos de crise (contexto)
  geom_rect(
    data = crises,
    aes(
      xmin = inicio,
      xmax = fim,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.5
  ) +
  
  ## ENA (% da MLT)
  geom_line(
    aes(y = ena_perc_mlt, colour = "ENA (% MLT)"),
    linewidth = 0.6
  ) +
  
  ## Linha de referência (100% da MLT)
  geom_hline(
    aes(yintercept = 100, colour = "MLT (100%)"),
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  ## Facets por subsistema
  facet_wrap(~ nom_subsistema, ncol = 1, scales = 'free_y') +
  
  ## Escala temporal comum
  escala_x_tempo +
  
  ## Escala de cores (linhas)
  scale_colour_manual(
    name = NULL,
    values = c(
      "ENA (% MLT)" = "#08519C",
      "MLT (100%)" = "black"
    )
  ) +
  
  ## Rótulos
  labs(
    title = "ENA mensal em percentagem da MLT por subsistema",
    x = "Ano",
    y = "ENA (% MLT)",
    caption = "*Faixas cinzas indicam períodos aproximados das crises no setor."
  ) +
  
  ## Tema comum
  tema_comum


## GRÁFICOS GERAÇÃO ----


ggplot(geracao_mes, aes(x = mes)) +
  
  ## Períodos de crise (contexto)
  geom_rect(
    data = crises,
    aes(
      xmin = inicio,
      xmax = fim,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.5
  ) +
  
  ## Geração mensal em GWmed
  geom_line(
    aes(y = geracao_mwmed / 1000),
    colour = "#08519C",
    linewidth = 0.9
  ) +
  
  ## Facets por tipo de usina
  facet_wrap(~ nom_tipousina, ncol = 1, scales = 'free_y') +
  
  ## Escala temporal comum
  escala_x_tempo +
  
  ## Escala Y
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  
  ## Rótulos
  labs(
    title = "Geração mensal por tipo de usina",
    x = "Ano",
    y = "Geração (GWmed)",
    caption = "*Faixas cinzas indicam períodos aproximados das crises no setor."
  ) +
  
  ## Tema comum
  tema_comum


## GRÁFICOS CMO ----

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
    fill = "grey80",
    alpha = 0.5
  ) +
  geom_line(aes(y = val_cmomediasemanal), 
            colour = "#08519C",
            linewidth = 0.8) +
  facet_wrap(~ nom_subsistema, ncol = 1, scales = 'free_y') +
  escala_x_tempo +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  labs(
    title = 'CMO Médio Semanal por Subsistema',
    x = 'Ano',
    y = 'CMO (R$/MWh)',
    caption = "*Faixas cinzas indicam períodos aproximados das crises no setor."
  ) + 
  tema_comum +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(hjust = 0)
  )


# TESTES GRÁFICOS JUNTOS ----

(p1 / p2) + plot_layout(guides = 'collect')
