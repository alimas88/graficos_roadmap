# Tema, escalas, paletas e formatos comuns às figuras da NT

cor_principal <- "#08519C"

pal_usinas <- c(
  "HIDROELÉTRICA" = "#4DA3D9",
  "TÉRMICA"       = "#6B3E26",
  "NUCLEAR"       = "#C0392B",
  "EÓLICA"        = "#8FD19E",
  "FOTOVOLTAICA"  = "#F2C94C",
  "GD"            = "#F29900"
)

paleta_tensao <- c(
  "800"     = "#0B4D6E",
  "765"     = "#000000",
  "600"     = "#B5A800",
  "500/525" = "#E41A1C",
  "440"     = "#984EA3",
  "345"     = "#08519C",
  "230"     = "#238B45"
)

# subsistemas: ordem e rótulos de exibição
ordem_subsistemas <- c("SUDESTE", "NORDESTE", "SUL", "NORTE")
rotulo_subsistema <- c(
  SUDESTE  = "Sudeste/Centro-Oeste",
  NORDESTE = "Nordeste",
  SUL      = "Sul",
  NORTE    = "Norte",
  SIN      = "SIN"
)

legenda_crises <- "Faixas cinzas indicam períodos aproximados das crises no setor."
legenda_periodo_critico <- "Faixa azul: período crítico de referência (jun/2012 a nov/2021)."

# números no padrão brasileiro
num_br <- function(accuracy = 1, ...) {
  scales::label_number(accuracy = accuracy, big.mark = ".", decimal.mark = ",", ...)
}
fmt_br <- function(x, accuracy = 0.1, ...) {
  scales::number(x, accuracy = accuracy, big.mark = ".", decimal.mark = ",", ...)
}

# "jan/2021" sem depender do locale do sistema
meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun",
              "jul", "ago", "set", "out", "nov", "dez")
mes_ano <- function(data) {
  paste0(meses_pt[lubridate::month(data)], "/", lubridate::year(data))
}

tema_nt <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.margin        = margin(2, 2, 2, 2, unit = "mm"),
      panel.grid.minor   = element_blank(),
      axis.title         = element_text(size = rel(0.9)),
      axis.text          = element_text(size = rel(0.8)),
      strip.text         = element_text(hjust = 0, face = "bold", size = rel(0.9)),
      legend.position    = "bottom",
      legend.text        = element_text(size = rel(0.8)),
      legend.title       = element_text(size = rel(0.85)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, size = rel(0.75), colour = "grey40",
                                  margin = margin(t = 6))
    )
}

# mesma janela e divisão temporal em todas as figuras
escala_x_tempo <- function(inicio = "2000-01-01", fim = "2024-12-31", passo = "4 years") {
  inicio <- as.Date(inicio)
  fim <- as.Date(fim)
  scale_x_date(
    limits = c(inicio, fim),
    breaks = seq(inicio, fim, by = passo),
    date_labels = "%Y",
    expand = expansion(mult = 0)
  )
}

# ggiraph no HTML; ggplot estático em qualquer outra saída
interativo <- function(p, largura = 8, altura = 5) {
  if (!knitr::is_html_output()) return(p)

  girafe(
    ggobj = p,
    width_svg = largura,
    height_svg = altura,
    options = list(
      opts_tooltip(
        css = "background:#ffffff;color:#1a1a1a;border:1px solid #c9d6e8;
               border-radius:4px;padding:6px 8px;font-size:12px;max-width:280px;
               white-space:normal;box-shadow:0 2px 6px rgba(0,0,0,.12);",
        opacity = 1
      ),
      # realce separado: pontos acendem; faixas (rect) só ganham contorno
      opts_hover(css = girafe_css(
        css   = "",
        point = "fill:#08519C;fill-opacity:1;stroke:#ffffff;stroke-width:1px;",
        area  = "stroke:#7a7a7a;stroke-width:1.5px;"
      )),
      opts_toolbar(saveaspng = TRUE, position = "topright"),
      opts_sizing(rescale = TRUE)
    )
  )
}
