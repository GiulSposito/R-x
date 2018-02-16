library(stringr)
library(tidyverse)
library(rvest)

read_html("http://nomesportugueses.blogspot.com.br/2009/04/nomes-femininos-de-a-z.html") %>%
  html_nodes("div #post-body-7362496277394638851 ul li") %>%
  html_text() %>%
  as.tibble() %>%
  setNames("name") %>%
  mutate(gender = "F") -> dt.f.names

read_html("http://nomesportugueses.blogspot.com.br/2009/04/nomes-masculinos-de-a-z.html") %>%
  html_nodes("div #post-body-8282332538944376214 ul li") %>%
  html_text() %>%
  as.tibble() %>%
  setNames("name") %>%
  mutate(gender = "M") -> dt.m.names

dt.names <- bind_rows(dt.f.names, dt.m.names)
    
saveRDS(dt.names, "./ele_ela_analise/data/nomes_proprios.rds")
