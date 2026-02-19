library(tidyverse)
library(purrr)

################# EAR por Subsistema ###################

# dicionário de dados do EAR Diário por Subsistema
dicionario_ear <- jsonlite::read_json(
  "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/DicionarioDados_EarPorSubsistema.json",
  simplifyVector = T
)$dicionario_simplificado


# intervalo de anos para criar as urls
anos <- 2000:2024
camiho_generico_ear <- "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_"


# leaitura de todas as urls
ear_subsistema <- map_chr(anos, ~ glue::glue("{camiho_generico_ear}{.x}.csv")) |>
  map(~read_delim(.x, locale = locale(decimal_mark = '.'))) |>
  bind_rows()


# intervalos das crises de escassez - aproximado, auxilio visual
crises_hidricas <- tibble(
  inicio = as.Date(c("2001-01-01", "2012-10-01", "2020-12-01")),
  fim    = as.Date(c("2002-02-28", "2016-12-31", "2022-04-30"))
)


# transformação - redução da temporalidade para reduzir a variabilidade
ear_percentagem <- ear_subsistema |> 
  mutate(
    mlt = mean(ear_verif_subsistema_percentual),
    semana = floor_date(ear_data, "week", week_start = 1),
    .by = nom_subsistema
  ) |>
  summarise(
    ear_sub_perc = min(ear_verif_subsistema_percentual, na.rm = TRUE),
    mlt = unique(mlt),
    .by = c(nom_subsistema, semana)
  ) |>
  arrange(nom_subsistema, semana)


# gráficos facetados de %EAR por subsistema com destaque para o período das crises
ggplot(ear_percentagem, aes(x = semana)) +
  geom_rect(
    data = crises_hidricas,
    aes(
      xmin = inicio,
      xmax = fim,
      ymin = -Inf,
      ymax = Inf,
      fill = "Escassez hídrica"
    ),
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  geom_line(aes(y = ear_sub_perc, colour = "EAR"), linewidth = 0.9) +
  geom_line(aes(y = mlt), color = "MLT", linetype = 'dashed') +
  facet_wrap( ~ nom_subsistema, scales = 'fixed', ncol = 1) +
  scale_x_date(
    name = "Anos",
    date_labels = "%Y",
    limits = c(as.Date("2000-01-01"), as.Date("2024-12-31")),
    breaks = seq.Date(
      from = as.Date("2000-01-01"),
      to   = as.Date("2024-01-01"),
      by   = "2 years"
    ),
    expand = c(0, 0.01)
  ) +
  scale_colour_manual(name = NULL,
                      values = c("EAR" = "#08519C", "MLT" = "black")) +
  scale_fill_manual(name = NULL,
                    values = c("Escassez hídrica" = "grey80")) +
  labs(title = "Percentagem de EAR por subsistema", y = "EAR (%)") +
  
  theme_minimal()

# alternativa 2
ggplot(ear_percentagem, aes(x = semana)) +
  ## Escassez hídrica (faixa)
  geom_rect(
    data = crises_hidricas,
    aes(
      xmin = inicio,
      xmax = fim,
      ymin = -Inf,
      ymax = Inf,
      fill = "Escassez"
    ),
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  ## EAR
  geom_line(aes(y = ear_sub_perc, colour = "EAR"), linewidth = 0.9) +
  ## MLT
  geom_line(aes(y = mlt, colour = "MLT"), linetype = "dashed") +
  facet_wrap( ~ nom_subsistema, scales = "fixed", ncol = 1) +
  scale_x_date(
    name = "Anos",
    date_labels = "%Y",
    limits = c(as.Date("2000-01-01"), as.Date("2024-12-31")),
    breaks = seq.Date(
      from = as.Date("2000-01-01"),
      to   = as.Date("2024-01-01"),
      by   = "2 years"
    ),
    expand = c(0, 0.01)
  ) +
  scale_colour_manual(name = NULL,
                      values = c("EAR" = "#08519C", "MLT" = "black")) +
  scale_fill_manual(name = NULL,
                    values = c("Escassez" = "grey80")) +
  labs(title = "Percentagem de EAR por subsistema", y = "EAR (%)") +
  theme_minimal() +
  theme(legend.position = "right", legend.box = "vertical")



################# %EAR SIN ###################

ear_sin <- ear_subsistema |>
  summarise(
    sistema = 'SIN',
    ear_max = sum(ear_max_subsistema),
    ear_verificado_mwmes = sum(ear_verif_subsistema_mwmes),
    ear_percent = round(100 * ear_verificado_mwmes / ear_max, 2),
    .by = ear_data
  ) |> 
  mutate(mlt_ear = mean(ear_percent), .by = sistema) |> 
  mutate(
    semana = floor_date(ear_data, 'week', week_start = 1)
  ) |> 
  summarise(
    ear_verificado_mwmes = min(ear_verificado_mwmes),
    ear_max = ear_max[which.min(ear_verificado_mwmes)],
    ear_percent = ear_percent[which.min(ear_verificado_mwmes)],
    .by = c(sistema, semana, mlt_ear)
  )


# gráfico %EAR SIN
ggplot(ear_sin, aes(x = semana)) +
  geom_rect(
    data = crises,
    aes(
      xmin = inicio,
      xmax = fim,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = 'grey80',
    alpha = 0.5
  ) +
  geom_line(aes(y = ear_percent),
            colour = "#08519C",
            linewidth = 0.9) +
  geom_line(aes(y = mlt_ear), linetype = 'dashed', color = 'black') +
  labs(title = 'Percentagem do EAR no SIN', x = 'Anos', y = 'EAR (%)')
theme_minimal()
