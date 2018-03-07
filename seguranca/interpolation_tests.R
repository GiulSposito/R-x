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

# function to interpolate date x population by month
interPopByMonth <- function(dt=.){
  dt %>%
    as.tibble() %>% #garante ser tibble
    # cria um tibble indo da menor data a maior data mes a mes e faz join
    right_join(tibble(
      date=seq.Date(from=min(dt$date),to=max(dt$date),by="month")
      ), by="date") %>%
    # ordena por data
    arrange(date) %>%
    # interpola linearmente e reconstroi ano
    mutate( populacao = na.approx(populacao),
            ano = year(date) ) %>%
    return()
}

# testando nest/unest capitais
pop %>%
  filter( nivel=="MU", 
          grepl("São Paulo [/(].*", municipio) | 
          grepl("^Campinas [/(]SP.*", municipio) | 
          grepl("^Belém [/(]PA.*", municipio) |
          grepl("^Natal [/(]RN.*", municipio) |
          grepl("^Campo Grande [/(]MS.*", municipio) |
          grepl("Rio de Janeiro [/(].*", municipio) |
          grepl("Belo Horizonte [/(].*", municipio) |
          grepl("Recife [/(].*", municipio) | 
          grepl("Salvador [/(].*", municipio) |
          grepl("Porto Alegre [/(].*", municipio) | 
          grepl("Aracaju [/(].*", municipio) | 
          grepl("Florianópolis [/(].*", municipio) ) %>% 
  # remove ano (sera refeito na interpolacao)
  select(-ano) %>%
  # para cada "municipio" agrupa os dados em dataframes diferentes
  group_by(nivel, cod.ibge, municipio) %>%
  nest() %>%
  # aplica a interpolacao
  mutate( data = data %>% map(interPopByMonth) ) %>%
  # recompoe a tabela
  unnest() -> pop
