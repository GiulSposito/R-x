library(tidyverse)
library(googlesheets)
library(lubridate)

.parseRealValue <- function(x) parse_number(
  gsub(pattern = "R$ *", replacement = "", x = x), 
  locale=locale(grouping_mark=".", decimal_mark=",")
)

updatePortfolio <- function(key="1k0u_xV21AUEBzfi_e8rZtiAgEJD2OGsQu0QW-IJ_kCU", ws=1,
                            fname="./fii/data/portfolio.R"){
  gs_auth(new_user = T)
  gs_key(key) %>%
    gs_read(ws=1) %>%
    as.tibble() %>%
    select(Ativo, Qtd, Valor, Taxas, Total, Carteira, Data) %>%
    setNames(c("ativo","volume","cotacao","taxas", "valor", "carteira", "data")) %>%
    filter(complete.cases(.)) %>%
    mutate(
      cotacao.compra = .parseRealValue(cotacao),
      taxas          = .parseRealValue(taxas),
      valor.invest   = .parseRealValue(valor),
      carteira       = as.factor(carteira),
      data.compra    = ymd(data)
    ) %>%
    select(ativo, volume, cotacao.compra, taxas, valor.invest, carteira, data.compra) %T>%
    saveRDS(fname) %>%
    return()
}

getPortfolio <- function(fname="./fii/data/portfolio.R"){
  readRDS(fname) %>%
    return()
}