library(rvest)
library(tidyverse)
library(lubridate)

# parse num with locale ptBR
.parseNumPtBr <- function(x) parse_number(x, locale=locale(grouping_mark=".", decimal_mark=","))

getProventos <- function(){
  # extrai a tabela resumida
  read_html("http://fiis.com.br/resumo/") %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE) %>%
    as.tibble() %>%
    slice(2:nrow(.)) %>%
    setNames(c("ativo","data.base","cotacao.base","data.pgto","rend.valor","rend.perc","obs")) %>%
    mutate(
      data.base = dmy(data.base),
      data.pgto = dmy(data.pgto),
      cotacao.base = .parseNumPtBr(cotacao.base),
      rend.valor = .parseNumPtBr(rend.valor),
      rend.perc = .parseNumPtBr(rend.perc),
      data.importacao = now()
    ) %>% return()
}