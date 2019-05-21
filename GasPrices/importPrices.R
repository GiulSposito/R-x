library(tidyverse)
library(lubridate)

dir("./GasPrices/data",pattern = ".*zip", full.names = T) %>% 
  sort() %>% 
  map(function(filename){
    filename %T>%
      print() %>% 
      read_lines(skip = 1, progress = T, locale = locale("br")) %>%
      as_tibble() %>% 
      separate(value, into=c("regiao","estado","municipio", "posto",
                             "instalacao", "produto", "data","valor_compra",
                             "valor_venda","unidade","distribuidor"), sep="  ") %>% 
      mutate_at(vars(valor_compra, valor_venda), str_replace, ",", ".") %>%
      mutate_at(vars(valor_compra, valor_venda), as.numeric) %>%
      mutate_at(vars(instalacao), as.integer) %>%
      mutate( data = dmy(data) ) %>% 
      return()
  }) -> prices

prices %>% 
  bind_rows() %>% 
  mutate(
    regiao = as.factor(regiao), 
    estado = as.factor(estado), 
    municipio = as.factor(municipio), 
    posto = as.factor(posto), 
    produto = as.factor(produto), 
    unidade = as.factor(unidade), 
    distribuidor = as.factor(distribuidor)
  ) %>% 
  saveRDS("./GasPrices/data/precos.rds")


