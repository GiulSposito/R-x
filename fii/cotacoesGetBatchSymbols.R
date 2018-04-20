library(BatchGetSymbols)
library(lubridate)
source("./fii/getPortfolio.R")

.PRICE_FILENAME <- "./fii/data/prices.rds"

updatePrices <- function(){
  
  port <- getPortfolio()
  
  if(file.exists(.PRICE_FILENAME)) {
    historic <- readRDS(.PRICE_FILENAME)
  } else {
    historic <- tibble()
  }
  
  cotacoes <- unique(paste0(port$ativo, ".SA")) %>%
    BatchGetSymbols( first.date = ymd(20170101), thresh.bad.data = 0.01)
  
  cotacoes.recall <- cotacoes$df.control %>%
    filter (threshold.decision=="OUT") %>%
    .$ticker %>%
    BatchGetSymbols(thresh.bad.data=0)
  
  cotacoes$df.tickers %>%
    bind_rows(cotacoes.recall$df.tickers) %>%
    as.tibble() %>%
    mutate( ticker = gsub("\\.SA","",ticker) ) %>%
    bind_rows(historic) %>%
    distinct() %>%
    select(8,7,1:6,9:10) %>%
    arrange(ticker, ref.date) %T>%
    saveRDS(.PRICE_FILENAME) %>%
    return()
}

getPrices <- function() readRDS(.PRICE_FILENAME)

