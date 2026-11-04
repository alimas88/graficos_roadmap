# ENA (% da MLT) e EAR (% da EAR máxima) mensais por subsistema

# ENA mensal: razão entre a ENA média e a MLT média do mês.
# A MLT diária é recuperada de ENA (MWmed) / ENA (% MLT).
preparar_ena_mensal <- function(ena_di) {
  ena_di |>
    filter(
      nom_subsistema %in% ordem_subsistemas,
      ena_bruta_regiao_percentualmlt > 0
    ) |>
    mutate(
      mes = floor_date(as_date(ena_data), "month"),
      mlt_mwmed = 100 * ena_bruta_regiao_mwmed / ena_bruta_regiao_percentualmlt
    ) |>
    summarise(
      ena_mwmed = mean(ena_bruta_regiao_mwmed, na.rm = TRUE),
      mlt_mwmed = mean(mlt_mwmed, na.rm = TRUE),
      .by = c(nom_subsistema, mes)
    ) |>
    mutate(valor = 100 * ena_mwmed / mlt_mwmed)
}

# EAR mensal: último dia disponível do mês.
# incluir_sin = TRUE acrescenta o SIN (soma verificada / soma máxima).
preparar_ear_mensal <- function(ear_di, incluir_sin = FALSE) {
  ear <- ear_di |>
    filter(nom_subsistema %in% ordem_subsistemas) |>
    transmute(
      nom_subsistema,
      dia = as_date(ear_data),
      ear_verif = ear_verif_subsistema_mwmes,
      ear_max = ear_max_subsistema
    )

  if (incluir_sin) {
    sin <- ear |>
      summarise(across(c(ear_verif, ear_max), \(x) sum(x, na.rm = TRUE)), .by = dia) |>
      mutate(nom_subsistema = "SIN")
    ear <- bind_rows(ear, sin)
  }

  ear |>
    mutate(mes = floor_date(dia, "month")) |>
    slice_max(dia, n = 1, by = c(nom_subsistema, mes), with_ties = FALSE) |>
    mutate(valor = 100 * ear_verif / ear_max) |>
    arrange(nom_subsistema, mes)
}

# Junta as duas séries em formato longo, uma linha por subsistema, mês e variável
juntar_ena_ear <- function(ena_mensal, ear_mensal) {
  bind_rows(
    list(
      ENA = select(ena_mensal, nom_subsistema, mes, valor),
      EAR = select(ear_mensal, nom_subsistema, mes, valor)
    ),
    .id = "variavel"
  ) |>
    mutate(
      variavel = factor(variavel, levels = c("ENA", "EAR")),
      nom_subsistema = fct_relevel(nom_subsistema, intersect(c(ordem_subsistemas, "SIN"), unique(nom_subsistema)))
    )
}

rotulo_variavel <- c(ENA = "ENA (% da MLT)", EAR = "EAR (% da EAR máxima)")

fig_ena_ear <- function(dados, subsistema, crises, periodo_critico = TRUE,
                        inicio = "2000-01-01", fim = "2024-12-31") {
  d <- dados |>
    filter(
      nom_subsistema == subsistema,
      between(mes, as.Date(inicio), as.Date(fim))
    ) |>
    # dica com as duas variáveis do mês, igual nos dois painéis
    mutate(
      linha = paste0(rotulo_variavel[as.character(variavel)], ": ", fmt_br(valor, 0.1), "%")
    ) |>
    mutate(
      tooltip = paste0("<b>", mes_ano(mes), "</b><br>", paste(sort(linha, decreasing = TRUE), collapse = "<br>")),
      .by = mes
    ) |>
    mutate(id = as.character(mes))

  # referência de 100% só no painel de ENA
  ref_mlt <- tibble(variavel = factor("ENA", levels = levels(d$variavel)), y = 100)

  ggplot(d, aes(mes, valor)) +
    camada_crises(crises, periodo_critico) +
    geom_hline(data = ref_mlt, aes(yintercept = y),
               linetype = "dashed", linewidth = 0.4, colour = "black") +
    geom_line(colour = cor_principal, linewidth = 0.5) +
    # pontos quase invisíveis para a dica; o mesmo mês acende nos dois painéis
    # (colour = NA faria o ggplot descartar os pontos; alpha 0 some do SVG)
    geom_point_interactive(aes(tooltip = tooltip, data_id = id),
                           size = 2.2, colour = cor_principal, alpha = 0.01) +
    facet_wrap(~variavel, ncol = 1, scales = "free_y",
               labeller = as_labeller(rotulo_variavel)) +
    escala_x_tempo(inicio, fim) +
    scale_y_continuous(labels = num_br(), expand = expansion(mult = c(0.02, 0.08))) +
    labs(x = NULL, y = NULL, caption = legenda_faixas(periodo_critico)) +
    tema_nt()
}
