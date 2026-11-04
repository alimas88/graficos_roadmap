# Leitura dos dados abertos do ONS com cache local em parquet.
#
# O render do documento só lê o cache (sem rede, quando o cache está completo).
# Para baixar dados novos, rodar no console: source("atualizar_dados.R")

url_s3_ons <- "s3://ons-aws-prod-opendata/dataset"

eh_remoto <- function(caminho) grepl("^(s3|https?)://", caminho)

s3_para_https <- function(caminho) {
  sub("^s3://ons-aws-prod-opendata/", "https://ons-aws-prod-opendata.s3.amazonaws.com/", caminho)
}

conectar_duckdb <- function(remoto = TRUE, timeout_min = 10) {
  # duckdb >= 1.5: shared_home guarda a extensão httpfs entre sessões
  drv <- if ("shared_home" %in% names(formals(duckdb::duckdb))) {
    duckdb::duckdb(shared_home = TRUE)
  } else {
    duckdb::duckdb()
  }
  con <- DBI::dbConnect(drv)
  if (remoto) {
    DBI::dbExecute(con, "INSTALL httpfs;")
    DBI::dbExecute(con, "LOAD httpfs;")
    configurar_http(con, timeout_min)
  }
  con
}

# O padrão do DuckDB (30 s por requisição) é curto para os arquivos anuais.
# A unidade de http_timeout mudou entre versões (ms nas antigas, s nas novas):
# o valor atual indica qual é.
configurar_http <- function(con, timeout_min) {
  tryCatch({
    atual <- as.numeric(DBI::dbGetQuery(con, "SELECT current_setting('http_timeout') AS v")$v)
    valor <- if (atual >= 1000) timeout_min * 60 * 1000 else timeout_min * 60
    DBI::dbExecute(con, sprintf("SET http_timeout = %.0f", valor))
    DBI::dbExecute(con, "SET http_retries = 5")
  }, error = \(e) message("Não foi possível ajustar o timeout HTTP do DuckDB: ", conditionMessage(e)))
}

# Datasets leves (ENA, EAR): arquivo único com todas as colunas.
# Baixa só se o cache não existir ou se atualizar = TRUE.
cache_ons <- function(dataset, padrao = "*.csv", atualizar = FALSE, dir_cache = "dados/cache") {
  arquivo <- file.path(dir_cache, paste0(dataset, ".parquet"))

  if (atualizar || !file.exists(arquivo)) {
    dir.create(dir_cache, recursive = TRUE, showWarnings = FALSE)
    con <- conectar_duckdb()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

    leitor <- if (grepl("parquet$", padrao)) "read_parquet" else "read_csv_auto"
    tmp <- paste0(arquivo, ".tmp")

    DBI::dbExecute(con, sprintf(
      "COPY (SELECT * FROM %s('%s/%s/%s', union_by_name = true)) TO '%s' (FORMAT parquet)",
      leitor, url_s3_ons, dataset, padrao, normalizePath(tmp, winslash = "/", mustWork = FALSE)
    ))
    file.rename(tmp, arquivo)
    message("Cache atualizado: ", arquivo, " (", format(Sys.time(), "%d/%m/%Y %H:%M"), ")")
  }

  arrow::read_parquet(arquivo)
}

# Cache incremental de dados já agregados, para datasets pesados ----------------
#
# Cada arquivo de origem (anual até 2021, mensal a partir de 2022) é lido pelo
# DuckDB direto do S3 e agregado; só o resultado vai para o disco, em
# dados/cache/<nome>/<arquivo de origem>.parquet. O DuckDB baixa apenas as
# colunas usadas, e os dados brutos nunca são gravados.
#
# Robustez:
# - cada agregado é gravado em .tmp e renomeado no fim (falha não deixa arquivo pela metade);
# - se a leitura remota falhar, o arquivo de origem é baixado inteiro para uma pasta
#   temporária, agregado e apagado;
# - um arquivo com falha não interrompe os demais; no fim, a função informa quais
#   falharam. Basta rodar de novo: o que já foi agregado fica no cache.
#
# consulta: SQL com o marcador {arquivo}, que recebe o caminho de cada origem.

arquivo_valido <- function(f) {
  file.exists(f) && !inherits(try(arrow::read_parquet(f, as_data_frame = FALSE), silent = TRUE), "try-error")
}

agregar_arquivo <- function(con, origem, consulta, destino) {
  tmp <- paste0(destino, ".tmp")
  on.exit(unlink(tmp), add = TRUE)

  copiar <- function(caminho) {
    sql <- gsub("{arquivo}", caminho, consulta, fixed = TRUE)
    DBI::dbExecute(con, sprintf("COPY (%s) TO '%s' (FORMAT parquet)", sql,
                                normalizePath(tmp, winslash = "/", mustWork = FALSE)))
  }

  ok <- tryCatch({ copiar(origem); TRUE }, error = \(e) {
    message("  leitura remota falhou: ", conditionMessage(e))
    FALSE
  })

  if (!ok) {
    if (!eh_remoto(origem)) stop("falha ao ler ", origem)
    message("  baixando o arquivo inteiro e agregando localmente")
    local <- tempfile(fileext = ".parquet")
    on.exit(unlink(local), add = TRUE)
    op <- options(timeout = max(3600, getOption("timeout")))
    on.exit(options(op), add = TRUE)
    utils::download.file(s3_para_https(origem), local, mode = "wb", quiet = TRUE)
    copiar(local)
  }

  file.rename(tmp, destino)
}

atualizar_cache_incremental <- function(dataset, consulta, nome = dataset, padrao = "*.parquet",
                                        reprocessar_ultimos = 1, refazer = FALSE,
                                        dir_cache = "dados/cache", base = url_s3_ons) {
  dir_destino <- file.path(dir_cache, nome)
  dir.create(dir_destino, recursive = TRUE, showWarnings = FALSE)
  manifesto <- file.path(dir_destino, "_manifesto.csv")

  con <- conectar_duckdb(remoto = eh_remoto(base))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  origens <- DBI::dbGetQuery(
    con, sprintf("SELECT file FROM glob('%s/%s/%s') ORDER BY file", base, dataset, padrao)
  )$file
  if (length(origens) == 0) stop("Nenhum arquivo encontrado em ", base, "/", dataset)

  destinos <- file.path(dir_destino, basename(origens))
  pendentes <- refazer | !vapply(destinos, arquivo_valido, logical(1))
  pendentes[tail(seq_along(origens), reprocessar_ultimos)] <- TRUE

  falhas <- character()
  for (i in which(pendentes)) {
    message("Agregando ", basename(origens[i]), " (", sum(pendentes[seq_len(i)]), "/", sum(pendentes), ")")
    tryCatch(
      agregar_arquivo(con, origens[i], consulta, destinos[i]),
      error = \(e) {
        message("  FALHOU: ", conditionMessage(e))
        falhas <<- c(falhas, basename(origens[i]))
      }
    )
  }

  if (length(falhas) > 0) {
    unlink(manifesto)
    stop(
      length(falhas), " arquivo(s) não agregado(s): ", paste(falhas, collapse = ", "),
      ". Os demais estão no cache; rode de novo para tentar só os que faltam.",
      call. = FALSE
    )
  }

  # manifesto: lista completa da origem nesta data; permite ler o cache sem rede
  readr::write_csv(
    tibble::tibble(arquivo = basename(origens), atualizado_em = format(Sys.time(), "%Y-%m-%d %H:%M")),
    manifesto
  )
  message("Cache completo: ", nome, " (", length(origens), " arquivos)")
  invisible(destinos)
}

# Usado no render: lê o cache local se estiver completo; senão, completa o que falta.
ler_cache_incremental <- function(dataset, consulta, nome = dataset, ..., dir_cache = "dados/cache") {
  dir_destino <- file.path(dir_cache, nome)
  manifesto <- file.path(dir_destino, "_manifesto.csv")

  completo <- file.exists(manifesto) && {
    esperados <- file.path(dir_destino, readr::read_csv(manifesto, show_col_types = FALSE)$arquivo)
    all(file.exists(esperados))
  }

  if (!completo) {
    message("Cache de ", nome, " incompleto; completando a partir do ONS.")
    atualizar_cache_incremental(dataset, consulta, nome, ..., reprocessar_ultimos = 0, dir_cache = dir_cache)
    esperados <- file.path(dir_destino, readr::read_csv(manifesto, show_col_types = FALSE)$arquivo)
  }

  arrow::open_dataset(esperados) |> dplyr::collect()
}

# Consultas de agregação --------------------------------------------------------

# Geração horária por usina -> média mensal (MWmed) por subsistema e tipo de usina.
# Soma as usinas em cada hora e divide pelo número de horas do mês,
# de modo que usinas sem registro em alguma hora não inflem a média.
sql_geracao_mensal <- "
WITH horaria AS (
  SELECT
    CAST(din_instante AS TIMESTAMP)                     AS instante,
    nom_subsistema,
    nom_tipousina,
    SUM(TRY_CAST(NULLIF(CAST(val_geracao AS VARCHAR), '') AS DOUBLE)) AS geracao_mw
  FROM read_parquet('{arquivo}')
  WHERE nom_tipousina <> 'BOMBEAMENTO'
  GROUP BY ALL
),
horas AS (
  SELECT date_trunc('month', instante) AS mes, COUNT(DISTINCT instante) AS n_horas
  FROM horaria
  GROUP BY ALL
)
SELECT
  CAST(date_trunc('month', h.instante) AS DATE) AS mes,
  h.nom_subsistema,
  h.nom_tipousina,
  SUM(h.geracao_mw) / ANY_VALUE(n.n_horas)     AS geracao_mwmed,
  ANY_VALUE(n.n_horas)                         AS n_horas
FROM horaria h
JOIN horas n ON date_trunc('month', h.instante) = n.mes
GROUP BY ALL
"

# CMO semanal: só as colunas usadas
sql_cmo_semanal <- "
SELECT
  CAST(CAST(din_instante AS TIMESTAMP) AS DATE)          AS semana,
  id_subsistema,
  nom_subsistema,
  TRY_CAST(CAST(val_cmomediasemanal AS VARCHAR) AS DOUBLE) AS cmo
FROM read_parquet('{arquivo}')
"
