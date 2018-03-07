library(tidyverse)
library(zoo)
library(lubridate)

# # lendo ocorrencias
readRDS("./seguranca/data/ocorrencias.rds") %>%
  as.tibble() %>%
  setNames(c("fonte","regiao","UF","estado","cod.ibge","municipio",
             "crime","mes","mes.ano","ocorrencias","data")) %>%
  filter( cod.ibge != "NI" ) %>%
  mutate( cod.ibge = as.integer(as.character(cod.ibge)),
          ocorrencias = as.integer(ocorrencias)) %>%
  filter( cod.ibge %in% unique(pop$cod.ibge) ) -> occ

# join
occ %>%
  filter(data>=ymd("20070101")) %>%
  select(cod.ibge, crime, date=data, ocorrencias) %>%
  group_by(cod.ibge, date) %>%
  summarise( ocorrencias = sum(ocorrencias, na.rm = T)) %>%
  inner_join(pop) %>%
  mutate( taxa = 100000 * ocorrencias/populacao ) %>%
  ggplot(aes(date,taxa)) + 
    geom_point(aes(colour=municipio)) +
    stat_smooth(aes(colour=municipio)) + 
    facet_wrap(~municipio)
  

# join
occ %>%
  filter(data>=ymd("20070101"),
         cod.ibge %in% c(3550308,3304557,3106200,2927408)) %>%
  select(cod.ibge, crime, date=data, ocorrencias) %>%
  group_by(cod.ibge, date) %>%
  summarise( ocorrencias = sum(ocorrencias, na.rm = T)) %>%
  inner_join(pop) %>%
  mutate( taxa = 100000 * ocorrencias/populacao ) %>%
  ggplot(aes(date,taxa)) + 
  geom_point(aes(colour=municipio)) +
  stat_smooth(method="lm", aes(colour=municipio))



pop %>%
  filter(date>=ymd("20070101")) %>%
  ggplot(aes(date,populacao)) +
  geom_point(aes(colour=municipio)) +
  facet_wrap(~municipio)

occ %>%
  filter(data>=ymd("20070101")) %>%
  select(cod.ibge, municipio, crime, date=data, ocorrencias) %>%
  group_by(cod.ibge, municipio, date) %>%
  summarise( ocorrencias = sum(ocorrencias, na.rm = T)) %>%
  ggplot(aes(date,ocorrencias)) +
  geom_point(aes(colour=municipio)) +
  stat_smooth() + 
  facet_wrap(~municipio)


pop %>%
  select(cod.ibge, municipio) %>%
  distinct()
