library(rvest)
library(tidyverse)
library(lubridate)

# parse num with locale ptBR
parseNumPtBr <- function(x) parse_number(x, locale=locale(grouping_mark=".", decimal_mark=","))

# extrai a tabela resumida
read_html("http://fiis.com.br/resumo/") %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE) %>%
  as.tibble() %>%
  slice(2:nrow(.)) %>%
  setNames(c("ativo","data.base","cotacao.base","data.pgto","rend.real","rend.perc","obs")) %>%
  mutate(
    data.base = dmy(data.base),
    data.pgto = dmy(data.pgto),
    cotacao.base = parseNumPtBr(cotacao.base),
    rend.real = parseNumPtBr(rend.real),
    rend.perc = parseNumPtBr(rend.perc),
    data.importacao = now()
  ) %>%
  saveRDS("./fii/data/proventos.rds")

# extrai tabela de cotacoes do infomoney
read_html("http://www.infomoney.com.br/imoveis/fundos-imobiliarios/cotacoes") %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE) %>%
  .[,1:10] %>%
  as.tibble() %>%
  setNames(c("ativo","nome","data","val","var","min","max",
             "compra","venda","vol")) %>%
  mutate(
    data = dmy(paste0(data,year(now()))),
    val = parseNumPtBr(val),
    var = parseNumPtBr(var),
    min = parseNumPtBr(min),
    max = parseNumPtBr(max),
    compra = parseNumPtBr(compra),
    venda  = parseNumPtBr(venda),
    data.importacao = now()
  ) %>%
  saveRDS("./fii/data/cotacoes.rds")

