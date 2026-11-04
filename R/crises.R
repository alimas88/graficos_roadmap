# Tabela central de crises: única fonte das faixas usadas nas figuras

ler_crises <- function(arquivo = "dados/crises.csv") {
  readr::read_csv(
    arquivo,
    col_types = readr::cols(
      inicio = readr::col_date(),
      fim    = readr::col_date(),
      .default = readr::col_character()
    )
  ) |>
    mutate(
      tooltip = paste0(
        "<b>", rotulo, "</b><br>",
        mes_ano(inicio), " a ", mes_ano(fim), "<br>",
        "<span style='color:#666'>", fonte_data, "</span>"
      )
    )
}

# Camadas ggplot das faixas: período crítico (opcional) por baixo, crises por cima.
# Cores fixas, sem mapear fill, para não conflitar com a escala de fill de cada figura.
camada_crises <- function(crises, periodo_critico = FALSE) {
  faixa <- function(dados, cor, alpha) {
    geom_rect_interactive(
      data = dados,
      aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf,
          tooltip = tooltip, data_id = crise),
      inherit.aes = FALSE,
      fill = cor,
      alpha = alpha,
      colour = NA
    )
  }

  camadas <- list(faixa(filter(crises, camada == "crise"), "grey80", 0.6))

  if (periodo_critico) {
    camadas <- c(
      list(faixa(filter(crises, camada == "periodo_critico"), "#C6DBEF", 0.45)),
      camadas
    )
  }
  camadas
}

legenda_faixas <- function(periodo_critico = FALSE) {
  if (periodo_critico) paste(legenda_crises, legenda_periodo_critico) else legenda_crises
}
