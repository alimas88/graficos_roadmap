# Fontes dos dados locais

Arquivos em `dados/` que não vêm do cache do ONS. Ao trocar ou atualizar um
arquivo, atualize também a entrada correspondente aqui.

## crises.csv

Faixas de crise usadas em todas as figuras. A fonte de cada data está na
coluna `fonte_data`.

## gd.csv

Micro e minigeração distribuída (GD): potência instalada **adicionada** em cada
ano (não acumulada), em MW. O acumulado é calculado no código
(`preparar_capacidade()` em `R/fig_capacidade.R`).

- Fonte: EPE, Painel de Dados de Micro e Minigeração Distribuída,
  <https://dashboard.epe.gov.br/apps/pdgd/>
- Origem primária: ANEEL, Relação de empreendimentos de Mini e Micro GD,
  <https://dadosabertos.aneel.gov.br/dataset/relacao-de-empreendimentos-de-geracao-distribuida>
- Atualização do painel: março de 2026
- Total do painel (2013–2025): 45.040,03 MW, conferido com a soma das linhas
- Pendente: confirmar se a potência é CA ou CC
- Possível evolução: gerar a série direto da base da ANEEL (cache, como os
  dados do ONS), em vez de transcrever o painel
