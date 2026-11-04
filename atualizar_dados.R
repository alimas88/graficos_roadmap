# Atualiza o cache dos dados do ONS usados no documento.
# Rodar no console (com o projeto como diretório de trabalho) antes do render,
# sempre que quiser dados novos:  source("atualizar_dados.R")
#
# A primeira execução da geração horária demora (lê 25 anos de dados).
# Se algum arquivo falhar por rede, rode de novo: só os que faltam são processados.

for (arquivo in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(arquivo)

# leves: baixados inteiros
cache_ons("ena_subsistema_di", atualizar = TRUE)
cache_ons("ear_subsistema_di", atualizar = TRUE)
cache_ons("capacidade-geracao", padrao = "CAPACIDADE_GERACAO.csv", atualizar = TRUE)

# pesados: só o agregado; reprocessa o último arquivo (o ONS pode revisar o mês corrente)
atualizar_cache_incremental("geracao_usina_2_ho", sql_geracao_mensal, nome = "geracao_mensal")
atualizar_cache_incremental("cmo_se", sql_cmo_semanal, nome = "cmo_semanal")
