library(tidyverse)
library(lubridate)
library(rvest)
source("./fii/common.R")

importEmpiricus <- function(){
  
  # le pagina local (a sessão é autenticada)
  page <- read_html("./fii/files/empiricus.html")
  
  # pega as tabelas (uma para renda e outra para capital)
  page %>%
    rvest::html_table() -> tables
  
  # define colunas
  cnames <- c("fii.ticker","fii.gestor","fii.tipo","cart.peso","fii.valor.patrimonial","fii.valor.mercado",
              "cart.rank","cart.posicao","cart.preco.teto")
  
  # le a tabela de renda
  tables[[1]] %>%
    set_names(cnames) %>%
    as.tibble() %>%
    # as duas primeiras linhas e últimas duas são lixo
    slice(2:(n()-2)) %>%
    mutate( cart.tipo = "renda" ) -> carteira.renda
  
  # le a tabela de capital
  tables[[2]] %>%
    set_names(cnames) %>%
    as.tibble() %>%
    # as três primeiras linhas e a ultima são lixo
    slice(3:(n()-1)) %>%
    mutate( cart.tipo = "capital" ) -> carteira.capital
  
  # funde as carteiras (já diferenciadas na coluna cart.tipo)
  bind_rows(carteira.renda, carteira.capital) %>%
    mutate(
      fii.tipo = as.factor(fii.tipo),
      fii.gestor = as.factor(fii.gestor),
      cart.posicao = as.factor(cart.posicao),
      cart.tipo = as.factor(cart.tipo),
      cart.rank = as.integer(cart.rank),
      cart.peso = cart.peso %>% gsub(" *%","",.) %>% .parseNumPtBr()/100,
      fii.valor.patrimonial = fii.valor.patrimonial %>% .parseRealValue()
    ) %>%
    select( - fii.valor.mercado ) %>%
    return() 
}

updateEmpiricus <- function(){
  importEmpiricus() %T>%
    saveRDS("./fii/data/empiricus.rds") %>%
    return()
}

getEmpiricus <- function(){
  readRDS("./fii/data/empiricus.rds") %>%
    return()
}
