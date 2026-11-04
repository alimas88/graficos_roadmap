# Geração hidrelétrica e térmica do SIN (GWmed) e CMO semanal por subsistema

# Geração mensal do SIN por tipo, em GWmed
preparar_geracao_sin <- function(geracao_mensal, tipos = c("HIDROELÉTRICA", "TÉRMICA")) {
  geracao_mensal |>
    filter(nom_tipousina %in% tipos) |>
    summarise(valor = sum(geracao_mwmed, na.rm = TRUE) / 1000, .by = c(mes, nom_tipousina)) |>
    mutate(mes = as_date(mes))
}

rotulo_painel_gc <- c(
  "HIDROELÉTRICA" = "Geração hidrelétrica do SIN (GWmed)",
  "TÉRMICA"       = "Geração termelétrica do SIN (GWmed)",
  "CMO"           = "CMO médio semanal (R$/MWh)"
)

fig_geracao_cmo <- function(geracao_sin, cmo, subsistema_cmo, crises,
                            inicio = "2000-01-01", fim = "2024-12-31") {
  periodo <- \(d, col) filter(d, between({{ col }}, as.Date(inicio), as.Date(fim)))

  ger <- geracao_sin |>
    periodo(mes) |>
    transmute(painel = nom_tipousina, data = mes, valor,
              tooltip = paste0("<b>", mes_ano(mes), "</b><br>",
                               stringr::str_to_sentence(nom_tipousina), ": ",
                               fmt_br(valor, 0.1), " GWmed"),
              id = as.character(mes))

  cmo_sub <- cmo |>
    filter(nom_subsistema == subsistema_cmo) |>
    mutate(semana = as_date(semana)) |>
    periodo(semana) |>
    transmute(painel = "CMO", data = semana, valor = cmo,
              tooltip = paste0("<b>Semana de ", format(semana, "%d/%m/%Y"), "</b><br>",
                               "CMO ", rotulo_subsistema[subsistema_cmo], ": R$ ",
                               fmt_br(valor, 1), "/MWh"),
              # mesmo id do mês: passar o mouse acende o mês nos três painéis
              id = as.character(floor_date(semana, "month")))

  d <- bind_rows(ger, cmo_sub) |>
    mutate(painel = factor(painel, levels = names(rotulo_painel_gc)))

  ggplot(d, aes(data, valor)) +
    camada_crises(crises) +
    geom_line(colour = cor_principal, linewidth = 0.45) +
    geom_point_interactive(aes(tooltip = tooltip, data_id = id),
                           size = 1.8, colour = cor_principal, alpha = 0.01) +
    facet_wrap(~painel, ncol = 1, scales = "free_y",
               labeller = as_labeller(rotulo_painel_gc)) +
    escala_x_tempo(inicio, fim) +
    scale_y_continuous(labels = num_br(), limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = NULL,
         caption = paste(legenda_crises, "Os dados de CMO iniciam em 2005.")) +
    tema_nt()
}
