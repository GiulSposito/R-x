library(tidyverse)

read_lines("./GasPrices/data/2018-2_CA.csv", skip = 1) %>%
  as_tibble() %>% 
  separate(value, into=c("regiao","estado","municipio", "posto",
                         "instalacao", "produto", "data","valor_compra",
                         "valor_venda","unidade","distribuidor"), sep="  ") %>% 
  mutate_at(vars(regiao, estado, municipio, posto, produto, unidade, distribuidor), as.factor) %>% 
  mutate_at(vars(valor_compra, valor_venda), str_replace, ",", ".") %>% 
  mutate_at(vars(valor_compra, valor_venda), as.numeric) %>% 
  mutate_at(vars(instalacao), as.integer) %>% 
  saveRDS("./GasPrices/data/precos.rds")
