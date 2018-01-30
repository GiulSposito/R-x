# script to get eval from site cervanossa
library(rvest)
library(stringr)
library(tidyverse)

base.url <- "https://cervanossa.wordpress.com/"
pages <- 1:50

html_doc <- read_html(base.url)

html_doc %>%

html_doc %>%
  html_nodes(".main p:first-child") %>%
  html_text() %>% str_split("\n") %>%
  map(function(texts){
    str_replace(texts, ".+: ", "")
  }) %>% unlist() %>% as.vector() %>%
  matrix(ncol=9, byrow = T) -> atributos




str()

matrix( unlist(beers.eval) )

str(beers.eval)

dimnames = list("pais","tipo","alcool",
                "cor","sabor","malte",
                "avaliacao","preco","volume")