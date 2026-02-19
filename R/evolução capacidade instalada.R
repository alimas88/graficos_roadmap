library(tidyverse)
library(scales)

################# DADOS INICIAIS E DEFINIÇÕES ###################


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


################# GRÁFICOS ###################

pal_usinas <- c(
  "HIDROELÉTRICA" = "#4DA3D9",
  "TÉRMICA" = "#6B3E26",
  "EÓLICA" = "#8FD19E",
  "FOTOVOLTAICA" = "#F2C94C",
  "NUCLEAR" = "#C0392B",
  "GD" = "#F29900"
)
  

# participação na matriz em %
ggplot(cap_por_ano, aes(x = ano, y = potencia, fill = nom_tipousina)) +
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
  geom_col(
    position = "fill",
    width = 0.85,
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_manual(values = pal_usinas) +
  escala_x_tempo +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Participação da Capacidade Instalada por Tipo de Usina",
    x = "Ano",
    y = "Participação (%)",
    fill = "Tipo de Usina"
  ) +
  tema_comum

# participação total na matriz
ggplot(cap_por_ano,
       aes(
         x = factor(ano),
         y = potencia / 1000,
         fill = nom_tipousina
       )) +
  geom_col(width = 0.9,
           color = "white",
           linewidth = 0.2) +
  geom_text(
    aes(label = if_else(
      potencia / 1000 >= 5,
      scales::number(potencia / 1000, accuracy = 1),
      ""
    )),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = 'bold',
    size = 3
  ) +
  scale_fill_manual(values = pal_usinas) +
  scale_y_continuous(labels = scales::label_number(
    big.mark = ".",
    decimal.mark = ",",
    accuracy = 1
  )) +
  labs(title = "Capacidade Instalada de Geração",
       x = "Ano",
       y = "Potência Instalada (GW)",
       fill = "Tipo de Usina") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", panel.grid.major.x = element_blank())


# intervalos das crises de escassez - aproximado, auxilio visual
crises_hidricas <- tibble(
  inicio = as.Date(c("2001-01-01", "2012-10-01", "2020-12-01")),
  fim    = as.Date(c("2002-02-28", "2016-12-31", "2022-04-30"))
)


# gráfico de área
ggplot(cap_por_ano, aes(x = ano, y = potencia / 1000, fill = nom_tipousina)) +
  
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
    fill = "Tipo de Usina",
    caption = "*Faixas cinzas indicam períodos de crises"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right"
  )
