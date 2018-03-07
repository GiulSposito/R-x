library(tidyverse)
library(zoo)
library(lubridate)

# lendo dados da populacao populacao
read.table("./seguranca/data/tabela6579.csv",
           sep = ";",
           skip=3,
           nrows = 83955,
           encoding = "UTF-8",
           header = F) %>%
  # arrumando o header (que tem acentos)
  setNames(c("nivel","cod.ibge","municipio","num","ano","populacao")) %>%
  # o numero do registro nao interessa
  select(-num) %>% 
  # populacao foi salva como fator! transformando o label do fator em inteiro
  mutate(populacao=as.integer(as.character(populacao))) %>% # salvo como fator!
  # definindo que o "ano" da populacao e o valor da populacao no fim (dezembro)
  mutate(date = ymd(paste0(ano,"/12/01"))) -> pop

# teste de interpolacao
pop %>%
  filter(cod.ibge==3550308) %>%
  (function(dt=.){
    dt %>%
      as.tibble() %>%
      select(date, populacao) %>%
      right_join(tibble(date=seq.Date(from=min(dt$date),to=max(dt$date),by="month"))) %>%
      arrange(date) %>%
      mutate( populacao = na.approx(populacao),
              ano = year(date) )
  }) -> x

# plotando
plot(x$date, x$populacao, type="l") 

# function to interpolate date x population by month
interPopByMonth <- function(dt=.){
  dt %>%
    as.tibble() %>%
    select(date, populacao) %>%
    right_join(tibble(
      date=seq.Date(from=min(dt$date),to=max(dt$date),by="month")
      ), by="date") %>%
    arrange(date) %>%
    mutate( populacao = na.approx(populacao),
            ano = year(date) ) %>%
    return()
}

# testando nest/unest capitais
pop %>%
  filter( nivel=="MU", 
          grepl("São Paulo [/(].*", municipio) | 
          grepl("Rio de Janeiro [/(].*", municipio) |
          grepl("Belo Horizonte [/(].*", municipio) |
          grepl("Recife [/(].*", municipio) | 
          grepl("Salvador [/(].*", municipio) |
          grepl("Porto Alegre [/(].*", municipio) | 
          grepl("Aracaju [/(].*", municipio) | 
          grepl("Florianópolis [/(].*", municipio) ) %>%
  select(-ano) %>%
  group_by(nivel, cod.ibge, municipio) %>%
  nest() %>%
  mutate( data = data %>% map(interPopByMonth) ) %>%
  unnest() -> x

# # lendo ocorrencias
readRDS("./seguranca/data/ocorrencias.rds") %>%
  setNames(c("fonte","regiao","UF","estado","cod.ibge","municipio",
          "crime","mes","mes.ano","ocorrencias","data")) -> occ

