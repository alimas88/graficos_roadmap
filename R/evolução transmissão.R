library(tidyverse)

################# Evolução LTs existentes ###################

dicionario_lts <- jsonlite::read_json(
  "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/linha_transmissao/DicionarioDados_Linha_Transmissao.json", 
  simplifyVector = T)$dicionario_simplificado

paleta_tensao <- c(
  "800"     = "#0B4D6E",  # azul petróleo
  "765"     = "#000000",  # preto
  "600"     = "#B5A800",  # amarelo/oliva
  "500/525" = "#E41A1C",  # vermelho
  "440"     = "#984EA3",  # roxo
  "345"     = "#08519C",  # azul
  "230"     = "#238B45"   # verde
)


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
  mutate(anos = list(seq(ano_inicio, ano_fim))) |> 
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
    val_niveltensao_kv = forcats::fct_relevel(
      val_niveltensao_kv,
      '800', '765', '600', '500/525', '440', '345'
      )
    )

# gráfico de área
ggplot(evolucao_trans,
       aes(x = anos, y = extensao/1000, fill = val_niveltensao_kv)) +
  geom_area()+
  escala_x_tempo +
  scale_y_continuous(labels = scales::label_number_auto()
  )+
  scale_fill_manual(values = paleta_tensao) +
  labs(
    x = "Ano",
    y = "Extensão acumulada (mil km)",
    fill = "Tensão (kV)",
    title = "Evolução Física das Linhas de Transmissão"
  ) +
  theme_minimal()

# gráfico de colunas
evolucao_trans |> 
  filter(anos %% 2 == 0) |> 
ggplot(
  aes(anos, extensao, fill = factor(val_niveltensao_kv)))+
  geom_col()+
  scale_fill_manual(values = paleta_tensao)+
  labs(
    title = "Evolução física das Linhas de Transmissão", 
    x = 'Ano', 
    y = 'Extensão (km)',
    fill = "Tensão (kV)"
  )+
  theme_minimal()




escala_x_tempo <- scale_x_date(
  limits = c(as.Date("2000-01-01"), as.Date("2024-12-31")),
  date_labels = "%Y",
  breaks = seq.Date(
    from = as.Date("2000-01-01"),
    to   = as.Date("2024-01-01"),
    by   = "2 years"
  ),
  expand = c(0, 0)
)


