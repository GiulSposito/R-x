# script de scrapt do MJ
library(rvest)
library(tidyverse)
library(lubridate)

# url que lista as páginas de estatistica
base.url <- "http://dados.mj.gov.br"
stat.list <- "/dataset/sistema-nacional-de-estatisticas-de-seguranca-publica"

# pesquisa população IBGE
# https://sidra.ibge.gov.br/tabela/6579

# 
html <- read_html(paste0(base.url,stat.list))

html %>%
  html_nodes(".heading") %>%
  html_attr("href") %>%
  na.omit() %>%
  paste0(base.url, .) -> page

html %>%
  html_nodes(".heading") %>%
  html_attr("title") %>%
  na.omit() -> title

tibble( title, page ) %>%
  filter(grepl("ocorrencias.+", title)) -> stat.pages

stat.pages$page %>%
  map(function(url){
    read_html(url) %>%
      html_node("p.muted a") %>%
      html_attr("href")    
  }) %>% 
  unlist() -> csv.link

stat.pages <- bind_cols(stat.pages, csv.link=csv.link)

stat.pages$csv.link %>%
  map(function(href){
    read.table(url(href), skip = 4, header = T, sep=";", stringsAsFactors = F) %>%
      as.tibble()
  }) -> csvs.download

saveRDS(csvs.download, "./seguranca/data/csv_downloaded.rds")

csvs.download %>%
  setNames(stat.pages$title) %>%
  bind_rows(.id = "dataset") %>% 
  mutate( month = dmy(paste0("01/",`Mês.Ano`)) ) %>%
  mutate_if(is.character, as.factor) -> csvs
