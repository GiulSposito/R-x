# script de scrapt do MJ
library(tidyverse)
library(lubridate)

# lendo populacao
read.table("./seguranca/data/tabela6579.csv",
           sep = ";",
           skip=3,
           nrows = 83955,
           encoding = "UTF-8",
           header = F) %>%
  setNames(c("nivel","cod.ibge","municipio","num","ano","populacao")) %>%
  select(-num) %>% 
  mutate(month=as.integer(12)) %>%
  tail(10000) -> pop

pop %>%
  mutate(month=as.integer(0), ano=ano+1) %>%
  bind_rows(pop) -> p
  


p %>%
  group_by(nivel, cod.ibge, municipio, ano) %>%
  nest() -> pn

pn[5000,]$data




# # lendo ocorrencias
# readRDS("./seguranca/data/ocorrencias.rds") %>% 
#   setNames(c("fonte","regiao","UF","estado","cod.ibge","municipio",
#           "crime","mes","mes.ano","ocorrencias","data")) -> occ
# 
# head(pop)
# head(occ)
# 
# dim (occ)
# summary(pop$nivel)
# 
# pop %>%
#   mutate(month=12) -> pop
# 
# pop %>%
#   mutate(month=0,
#          ano=ano+1) %>%
#   bind_rows(pop) %>%
#   group_by(nivel, cod.ibge, municipio, num, ano) %>%
#   nest() -> pop.nest
# 
# 
# 
# View(pop)
# View(occ)
# 
# str(occ)
# 
# 
# pop %>% 
#   filter( municipio=="São Paulo (SP)")
# 
# 
# cod <- 3550308
# ano <- 2001:2017
# mes <- 1:12
# 
# pop %>%
#   mutate( mes=12 ) %>%
#   select(cod.ibge, mes, ano, populacao) %>%
#   right_join(expand.grid(cod.ibge=cod, mes=mes, ano=ano))
#   
# 
# 
# 
# 
