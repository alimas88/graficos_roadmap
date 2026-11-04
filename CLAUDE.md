# Roadmap Resiliência do SEB: capítulo "Contexto Setorial e Climático"

Documento Quarto (`contexto_setorial.qmd`) que mescla o texto da Nota Técnica com figuras interativas em R, renderizado em HTML autocontido. Tese: a evolução física do sistema (geração e transmissão) mudou a resposta do SIN às crises hídricas de 2001, 2012–2016 e 2021, olhando oferta, armazenamento, rede e demanda.

Responda e comente em português. Antes de mudar uma figura, leia o arquivo `R/fig_*.R` dela e as funções comuns em `R/tema.R` e `R/crises.R`.

## Estrutura

```
contexto_setorial.qmd   # texto da NT + chamadas das figuras
atualizar_dados.R       # baixa/atualiza o cache do ONS (rede)
R/                      # funções, carregadas pelo setup do .qmd
  00_pacotes.R          # library() dos pacotes do documento
  tema.R                # tema, escalas, paletas, formatos, interativo()
  crises.R              # ler_crises(), camada_crises(), legendas
  dados_ons.R           # leitura do ONS e cache em parquet
  fig_<tema>.R          # preparar_*() + fig_*() de cada figura
dados/
  crises.csv            # faixas de crise (versionado)
  gd.csv                # GD anual do BEN (fora do cadastro do ONS)
  cache/                # parquet do ONS (não versionado)
referencias.bib  estilo.css  DESCRIPTION  graficos.roadmap.Rproj
```

- Projeto Quarto comum, não é pacote: sem roxygen, `NAMESPACE` ou `@export`. O `DESCRIPTION` só lista dependências (`pak::local_install_deps()`); pacote novo usado no código entra nele.
- Caminhos relativos à raiz do projeto (diretório de trabalho do `.Rproj` e do render).
- Não versionar: `dados/cache/`, `*.parquet`, `*.html`, `*_files/`, `*.rmarkdown` (já no `.gitignore`).
- Figura nova: criar `R/fig_<tema>.R` com `preparar_*()` (dados) e `fig_*()` (ggplot), preparar os dados no bloco `setup` e substituir o `.fig-placeholder` correspondente no `.qmd`, mantendo o id `#fig-...` e a legenda.
- Código anterior à reorganização (capacidade instalada, transmissão, geração por fonte, `graficos_gerais_contexto.R`) está no histórico: `git show df4d487^:"R/<arquivo>.R"`. Serve de referência, mas segue a estrutura e as convenções atuais.

## Como roda

- `source("atualizar_dados.R")` no console baixa dados do ONS para `dados/cache/`. É o único passo que usa rede.
- O render (`quarto render contexto_setorial.qmd`) só lê o cache. Os datasets pesados vêm de `ler_cache_incremental()`, que só acessa a rede se o cache estiver incompleto (sem `_manifesto.csv` ou com algum arquivo listado nele ausente).
- O bloco `setup` do `.qmd` carrega todos os `R/*.R` em ordem alfabética (`00_pacotes.R` primeiro) e prepara os dados. Cada figura é uma função `fig_*()` chamada num bloco curto, envolvida em `interativo()`.

## Dados

- Fonte: ONS Open Data, `s3://ons-aws-prod-opendata/dataset/<dataset>`, lido via DuckDB + httpfs.
- Leves (ENA, EAR, capacidade de geração): `cache_ons()`, arquivo único com todas as colunas.
- Pesados (geração horária `geracao_usina_2_ho`, CMO `cmo_se`): `atualizar_cache_incremental()` agrega cada arquivo de origem com SQL no DuckDB e grava só o agregado, um parquet por arquivo de origem. Os arquivos são anuais até 2021 e mensais a partir de 2022. A agregação é local: o DuckDB baixa só as colunas usadas, sem gravar o bruto.
- A robustez a falhas de rede é parte do desenho: timeout HTTP de 10 min com retries, gravação em `.tmp` + rename, download completo como alternativa, e falha de um arquivo sem interromper os demais. Preserve isso ao mexer em `R/dados_ons.R`.
- Arquivos locais do usuário (desligamentos em xlsx, limites de intercâmbio digitalizados de figura da EPE, ondas de calor do INMET) entram em `dados/` com caminho relativo ao projeto e fonte registrada.

## Faixas de crise

`dados/crises.csv` é a fonte única das faixas em todas as figuras, lida por `ler_crises()` e desenhada por `camada_crises()`. Datas decididas com o usuário, conforme o E+ Transição Energética (2024):

| crise | faixa | base |
|---|---|---|
| 2001 | jun/2001 a fev/2002 | GCE e racionamento; suspenso em mar/2002 |
| 2012–2016 | out/2012 a mar/2016 | "a rigor, outubro de 2012 a março de 2016" |
| 2021 | out/2020 a abr/2022 | térmica fora do mérito desde out/2020; bandeira até abr/2022 |
| período crítico | jun/2012 a nov/2021 | 113 meses sem reenchimento; faixa azul, só na figura de ENA/EAR |

Datas novas entram no CSV, com a fonte na coluna `fonte_data`.

## Convenções

- Tidyverse atual: pipe `|>`, `.by =`, `across()`, `\(x)`, `forcats`, `lubridate`. Comentários curtos em português.
- Números no padrão brasileiro via `num_br()` e `fmt_br()`; datas como "jan/2021" via `mes_ano()`.
- Tema `tema_nt()`; eixo de tempo `escala_x_tempo()` (2000–2024, passo de 4 anos); legenda no rodapé. Séries mais curtas que a janela (CMO desde 2005, ENS desde 2007, desligamentos desde 2014) levam aviso na legenda.
- Cor principal `#08519C`; paletas `pal_usinas` e `paleta_tensao` em `R/tema.R`. Subsistemas na ordem SUDESTE, NORDESTE, SUL, NORTE, com rótulos de `rotulo_subsistema`.
- Métodos fixados: ENA mensal = ENA média / MLT média do mês; EAR = último dia do mês; EAR do SIN = soma verificada / soma máxima; geração = soma das usinas por hora dividida pelas horas do mês, sem BOMBEAMENTO, em GWmed.
- Figuras ainda não implementadas ficam em `::: fig-placeholder` com **Conteúdo**, **Dados** e **Interatividade**, dentro do bloco `#fig-...` com a legenda final.
- Subsistemas em abas (`::: {.panel-tabset}`) dentro do bloco `#fig-...`, com a legenda da figura após as abas. Painéis alinhados no tempo em `facet_wrap(ncol = 1)` substituem eixo duplo.

## ggiraph: armadilhas já resolvidas

- Pontos de hover usam `colour = <cor>` com `alpha = 0.01`. Com `colour = NA` o ggplot descarta os pontos como ausentes, e com `alpha = 0` o ggiraph não os desenha.
- `data_id` com o mês (`as.character(mes)`) acende o mesmo mês em todos os painéis; o CMO semanal usa `floor_date(semana, "month")` como id.
- O realce de hover é separado por tipo em `opts_hover(css = girafe_css(...))`: os pontos acendem e as faixas (`area`) só ganham contorno.
- Datas vindas do ONS como data-hora (POSIXct) são convertidas com `as_date()` antes de `scale_x_date`.

## Texto da NT

Tom técnico e impessoal, citações `[@chave]` de `referencias.bib`, referências cruzadas `@fig-...` e `@tbl-...`. Números, datas e custos batem com E+, PEN, EPE e ONS. Quando faltar fonte, marque `[trecho]{.verificar title="o que conferir"}` e avise o usuário. Entradas do `.bib` com `COMPLETAR` estão incompletas.

## Pendências

- Figuras ainda em quadro reservado: extensão de LTs + intercâmbio, EARmax/carga (métrica do PEN), comparação das três crises, desligamentos por causa e subsistema, ENS, robustez, carga e ondas de calor.
- Capacidade instalada: estoque em 31/12 a partir do cadastro atual do ONS (desativações antigas podem faltar, subestimando 2000–2002); conferir edição/tabela do BEN da série de GD.
- Limites de intercâmbio: sem série do ONS; digitalizar da figura da EPE (referência a completar).
- Critério de onda de calor (INMET) ainda a definir com o usuário.
- Saída só em HTML por enquanto; DOCX/PDF fora do escopo.
