# script to get eval from site cervanossa
library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)

base.url <- "https://cervanossa.wordpress.com/"
pages <- 1:8

scrapBeerPage <- function(base.url) {

  print(paste0("Scrapping: ", base.url))
  
  html_doc <- read_html(base.url)
  
  html_doc %>% 
    html_nodes("div .post") %>%
    html_nodes("h2 a:first-child") %>%
    html_text() %>%
    as.tibble() %>%
    rename(nome.completo=value) %>%
    mutate( 
      cervejaria = str_split(nome.completo," - ", simplify = T)[,1],
      cerveja = str_split(nome.completo," - ", simplify = T)[,2] 
    ) -> beers.name
  
  html_doc %>% 
    html_nodes("div .post") %>%
    html_nodes("h2 a:first-child") %>%
    html_attr("href") %>%
    as.tibble() %>%
    rename(link.avaliacao=value) -> beers.eval_link
  
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
    }) %>% 
    unlist() %>% as.vector() %>%
    matrix(ncol=9, byrow = T) %>%
    as.tibble() %>%
    setNames(c("pais","tipo","alcool",
               "cor","sabor","malte",
               "avaliacao","preco","volume")) -> beers.eval
  
  # html_doc %>%
  #   html_nodes(".main p:last-of-type a") %>%
  #   html_attr("href") %>%
  #   as.tibble() %>%
  #   rename(url=value) -> beers.url
  
  html_doc %>%
    html_nodes(".signature p:last-of-type") %>%
    html_text() %>%
    dmy_hm() %>%
    as.tibble() %>%
    rename(data.avalicao=value) -> beers.eval_date
  
  bind_cols( beers.name, beers.eval, beers.eval_date,
             beers.eval_link, beers.image) %>% return()

}

pages %>%
  paste0(base.url, "page/", .) %>%
  map_df(scrapBeerPage) -> beers


View(beers)

base.url <- "https://cervanossa.wordpress.com/page/9"
