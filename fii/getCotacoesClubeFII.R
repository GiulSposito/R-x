library(rvest)
library(tidyverse)
library(lubridate)
source("./fii/common.R")

updateCotacoes <- function(fname="./fii/data/cotacoes.rds"){
  
  # obtem pagina de cotacoes (ultimas)
  page <- read_html("https://www.clubefii.com.br/fundo_imobiliario_lista")
  
  # pega a tabela
  page %>%
    html_table() %>% 
    .[[1]] -> cotacoes
  
  # processa a table am dataframe
  cotacoes %>%
    as.tibble() %>%
    # a coluna cotacao esta misturada com variacao e datahora 
    mutate( valores = `VALOR COTA` %>% str_split("\r\n") %>% map(str_trim) %>%
              map(function(x) tibble(valor=x[1], var=x[2], datahora=x[3] )) ) %>%
    unnest() %>%
    # corrige nomes
    set_names("fii.ticker","fii.nome","cotacao","fii.data.ipo","fii.valor.ipo",
              "fii.gestor","cot.feed","cot.valor", "cot.var", "cot.datahora") %>%
    # corrige tipo e trata valores
    mutate(
      fii.gestor    = as.factor(fii.gestor),
      fii.data.ipo  = dmy(fii.data.ipo),
      fii.valor.ipo = .parseRealValue(fii.valor.ipo),
      cot.valor     = .parseRealValue(cot.valor),
      cot.var       = cot.var %>% gsub("%","",.) %>% .parseNumPtBr()/100,
      cot.datahora  = dmy_hms(cot.datahora)
    ) %>%
    # remove coluna misturada e outra com NA
    select(-cotacao, -cot.feed) %>%
    # remove linhas sem cotacao (FIIs sem movimento?)
    filter( !is.na(cot.valor) ) -> cotacoes.new
  
  # verifica se exist historico
  if(!file.exists(fname)) saveRDS(cotacoes.new, fname)
  
  # atualiza historico com valores atuais
  readRDS(fname) %>%
    bind_rows(cotacoes.new) %>%
    distinct() %T>%
    saveRDS(fname) %>%
    return()
}


getCotacoes <- function(fname="./fii/data/cotacoes.rds"){
  readRDS(fname) %>%
    return()
}


      