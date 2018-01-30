# script to get eval from site cervanossa
library(rvest)
library(stringr)
library(tidyverse)

base.url <- "https://cervanossa.wordpress.com/"
pages <- 1:50

html_doc <- read_html(base.url)

html_doc %>% 
  html_nodes("div .post") %>%
  html_nodes("h2 a:first-child") %>%
  html_text() %>%
  as.tibble() %>%
  rename(nome=value) -> beers.name

html_doc %>% 
  html_nodes(".main p:first-child img") %>%
  html_attr("src") %>%
  str_replace("\\?.*","") %>%
  as.tibble() %>% 
  rename(image=value) -> beers.image

html_doc %>%
  html_nodes(".main p:first-child") %>%
  html_text() %>% str_split("\n") %>%
  map(function(texts){
    str_replace(texts, ".+: ", "") %>%
      str_replace(.,"\\.+$","") 
  }) %>% unlist() %>% as.vector() %>%
  matrix(ncol=9, byrow = T) %>%
  as.tibble() %>%
  setNames(c("pais","tipo","alcool",
             "cor","sabor","malte",
             "avaliacao","preco","volume")) -> beers.eval

html_doc %>%
  html_nodes(".main p:last-of-type a") %>%
  html_attr("href") %>%
  as.tibble() %>%
  rename(url=value) -> beers.url

beers <- bind_cols(beers.name, beers.eval, beers.image, beers.url)

View(beers)
