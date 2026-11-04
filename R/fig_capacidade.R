# Capacidade instalada por tipo de usina (GW) e participação no total (%)

# GD (micro e minigeração distribuída): série anual fora do cadastro do ONS
ler_gd <- function(arquivo = "dados/gd.csv") {
  readr::read_csv(arquivo, col_types = readr::cols(ano = "i", potencia_mw = "d", .default = "c"))
}

# Estoque em 31/12 de cada ano: unidades que entraram até a data e não foram
# desativadas até ela. O cadastro do ONS traz só unidades ainda registradas;
# desativações anteriores a 2003 podem não constar.
preparar_capacidade <- function(cap_ons, gd, anos = 2000:2024) {
  unidades <- cap_ons |>
    transmute(
      tipo = recode(nom_tipousina, "EOLIELÉTRICA" = "EÓLICA"),
      entrada = as_date(dat_entradaoperacao),
      saida = as_date(dat_desativacao),
      potencia_mw = as.numeric(val_potenciaefetiva)
    ) |>
    filter(!is.na(entrada))

  estoque <- tibble(ano = anos, data_ref = make_date(anos, 12, 31)) |>
    cross_join(unidades) |>
    filter(entrada <= data_ref, is.na(saida) | saida > data_ref) |>
    summarise(potencia_mw = sum(potencia_mw, na.rm = TRUE), .by = c(ano, tipo))

  bind_rows(estoque, transmute(gd, ano, tipo = "GD", potencia_mw)) |>
    filter(ano %in% anos) |>
    mutate(
      tipo = factor(tipo, levels = names(pal_usinas)),
      gw = potencia_mw / 1000,
      pct = 100 * potencia_mw / sum(potencia_mw),
      .by = ano
    ) |>
    arrange(ano, tipo)
}

rotulo_painel_cap <- c(
  gw  = "Capacidade instalada (GW)",
  pct = "Participação no total (%)"
)

fig_capacidade <- function(capacidade, crises, inicio = "2000-01-01", fim = "2024-12-31") {
  d <- capacidade |>
    mutate(
      # barra centrada no meio do ano; o valor é o de 31/12
      data = make_date(ano, 7, 1),
      tooltip = paste0(
        "<b>", rotulo_usina[as.character(tipo)], ", dez/", ano, "</b><br>",
        fmt_br(gw, 0.1), " GW (", fmt_br(pct, 0.1), "% do total)"
      ),
      id = as.character(tipo)
    ) |>
    pivot_longer(c(gw, pct), names_to = "painel", values_to = "valor") |>
    mutate(painel = factor(painel, levels = names(rotulo_painel_cap)))

  ggplot(d, aes(data, valor, fill = tipo)) +
    camada_crises(crises) +
    geom_col_interactive(
      aes(tooltip = tooltip, data_id = id),
      position = position_stack(reverse = TRUE),
      width = 300, colour = "white", linewidth = 0.15
    ) +
    facet_wrap(~painel, ncol = 1, scales = "free_y",
               labeller = as_labeller(rotulo_painel_cap)) +
    escala_x_tempo(inicio, fim) +
    scale_y_continuous(labels = num_br(), expand = expansion(mult = c(0, 0.04))) +
    scale_fill_manual_interactive(
      values = pal_usinas,
      labels = rotulo_usina[names(pal_usinas)],
      drop = FALSE,
      data_id = \(breaks) as.character(breaks),
      tooltip = \(breaks) rotulo_usina[as.character(breaks)]
    ) +
    labs(x = NULL, y = NULL, fill = NULL,
         caption = paste(legenda_crises, "Valores em 31/12 de cada ano; GD a partir de 2015.")) +
    guides(fill = guide_legend(nrow = 1)) +
    tema_nt()
}
