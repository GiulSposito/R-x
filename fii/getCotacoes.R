library(rvest)
library(tidyverse)
library(lubridate)

# parse num with locale ptBR
.parseNumPtBr <- function(x) parse_number(x, locale=locale(grouping_mark=".", decimal_mark=","))


getCotacoes <- function(){

  x <- httr::POST("http://www.infomoney.com.br/api/ativos/baixar-planilha-carteira-acompanhamento?walletName=fmob") 
  
  tfname <- tempfile()
  cotfname <- "./fii/data/cotacoes_fii.rds"
  
  httr::content(x, type = "application/ms-excel") %>%
    rawToChar(., T) %>%
    paste(collapse="") %>%
    gsub(",","\\.",.) %>%
    gsub(";",",",.) %>%
    write(file=tfname)
  
  read_csv(tfname, skip = 1, col_names = F) %>%
    select(1:7,10) %>%
    set_names(c("ativo","ativo.nome","cot.data","cot.ult","cot.var","cot.min",
                "cot.max","cot.vol")) %>%
    mutate( cot.data = dmy(paste0(cot.data, year(now()))) ) %>% 
    bind_rows(readRDS(cotfname)) %>%
    distinct() %T>%
    saveRDS(cotfname) %>%
    return()
}
