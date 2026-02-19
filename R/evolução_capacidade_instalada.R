#' Gera gráficos anuais da evolução da capacidade instalada
#'
#' Consulta os dados abertos do ONS de capacidade de geração, consolida os valores
#' por tipo de usina em base anual e devolve dois gráficos:
#' \itemize{
#'   \item área empilhada com capacidade instalada absoluta (GW);
#'   \item colunas empilhadas em participação relativa (% do total).
#' }
#'
#' O eixo temporal é anual e termina em 31/12/2024 por padrão.
#'
#' @param ano_inicio Ano inicial para exibição dos gráficos.
#' @param data_fim Data final da série (padrão: "2024-12-31").
#'
#' @return Uma lista com `dados`, `grafico_absoluto` e `grafico_participacao`.
#' @export
#'
#' @examples
#' \dontrun{
#' resultado <- evolucao_capacidade_instalada()
#' resultado$grafico_absoluto
#' resultado$grafico_participacao
#' }
evolucao_capacidade_instalada <- function(ano_inicio = 2000,
                                          data_fim = as.Date("2024-12-31")) {
  cap_inst_ons <- readr::read_delim(
    "https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/capacidade-geracao/CAPACIDADE_GERACAO.csv",
    delim = ";",
    locale = readr::locale(decimal_mark = "."),
    show_col_types = FALSE
  )

  gd <- tibble::tibble(
    ano = 2015:2024,
    nom_tipousina = "GD",
    potencia = c(17, 72, 246, 670, 2162, 4768, 8965, 17325, 26627, 36168)
  )

  ano_fim <- lubridate::year(data_fim)

  cap_por_ano <- cap_inst_ons |>
    dplyr::mutate(
      dat_entradaoperacao = as.Date(dat_entradaoperacao),
      dat_desativacao = as.Date(dat_desativacao),
      ano_inicio = lubridate::year(dat_entradaoperacao),
      ano_fim = dplyr::if_else(
        is.na(dat_desativacao),
        ano_fim,
        lubridate::year(dat_desativacao)
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(ano = list(seq(ano_inicio, ano_fim))) |>
    tidyr::unnest(ano) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      potencia = sum(val_potenciaefetiva, na.rm = TRUE),
      .by = c(ano, nom_tipousina)
    ) |>
    dplyr::bind_rows(gd) |>
    dplyr::filter(ano >= ano_inicio, ano <= ano_fim) |>
    dplyr::mutate(
      nom_tipousina = dplyr::recode(nom_tipousina, "EOLIELÉTRICA" = "EÓLICA"),
      nom_tipousina = forcats::fct_relevel(
        nom_tipousina,
        "HIDROELÉTRICA",
        "TÉRMICA",
        "NUCLEAR",
        "EÓLICA",
        "FOTOVOLTAICA",
        "GD"
      ),
      ano_data = as.Date(sprintf("%s-12-31", ano))
    )

  pal_usinas <- c(
    "HIDROELÉTRICA" = "#4DA3D9",
    "TÉRMICA" = "#6B3E26",
    "EÓLICA" = "#8FD19E",
    "FOTOVOLTAICA" = "#F2C94C",
    "NUCLEAR" = "#C0392B",
    "GD" = "#F29900"
  )

  escala_tempo <- ggplot2::scale_x_date(
    limits = c(as.Date(sprintf(
      "%s-01-01", ano_inicio
    )), data_fim),
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = ggplot2::expansion(mult = c(0, 0))
  )

  grafico_absoluto <- ggplot2::ggplot(cap_por_ano,
                                      ggplot2::aes(x = ano_data, y = potencia / 1000, fill = nom_tipousina)) +
    ggplot2::geom_area(color = "white",
                       linewidth = 0.25,
                       alpha = 0.95) +
    ggplot2::scale_fill_manual(values = pal_usinas, drop = FALSE) +
    escala_tempo +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        big.mark = ".",
        decimal.mark = ",",
        accuracy = 1
      ),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(
      title = "Evolução da Capacidade Instalada por Tipo de Usina",
      x = "Ano",
      y = "Capacidade instalada (GW)",
      fill = "Tipo de usina"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  grafico_participacao <- ggplot2::ggplot(cap_por_ano,
                                          ggplot2::aes(x = ano_data, y = potencia, fill = nom_tipousina)) +

    ggplot2::geom_col(
      position = "fill",
      width = 300,
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::scale_fill_manual(values = pal_usinas, drop = FALSE) +
    escala_tempo +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "Participação da Capacidade Instalada por Tipo de Usina",
      x = "Ano",
      y = "Participação no total (%)",
      fill = "Tipo de usina"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  list(
    dados = cap_por_ano,
    grafico_absoluto = grafico_absoluto,
    grafico_participacao = grafico_participacao
  )
}
