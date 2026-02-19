#' Gera tema comum e escala do eixo x para os gráficos
#'
#' @returns Uma lista com `tema_comum` e `escala_x_tempo`
#' @export
#'
#' @examples
tema_escala <- function() {

  tema_comum <- theme_minimal() +
    theme(
      # Margens e eixos
      plot.margin = margin(5, 5, 5, 5, unit = "mm"),
      axis.title = element_text(margin = margin(t = 6), size = 8),
      axis.text = element_text(size = 7),

      # linhas de grade
      panel.grid.minor = element_blank(),

      # Título e legenda
      plot.title = element_text(hjust = 0.5, size = 9),
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

  return(list(
    tema_comum = tema_comum,
    escala_x_tempo = escala_x_tempo
  ))
}
