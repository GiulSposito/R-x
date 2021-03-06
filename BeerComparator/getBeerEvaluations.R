# script to get eval from site cervanossa
library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)
library(purrr)

base.url <- "https://cervanossa.wordpress.com/"
pages <- 1:203

scrapBeerPage <- function(base.url) {

  print(paste0("Scrapping: ", base.url))
  
  html_doc <- read_html(base.url)
  
  html_doc %>% 
    html_nodes("div .post") %>%
    html_nodes("h2 a:first-child") %>%
    html_text() %>%
    str_replace("\u00A0"," ") %>%
    str_replace("\u2011","_") %>%
    as.tibble() %>%
    rename(nome.completo=value) %>%
    mutate(
      cervejaria = str_split(nome.completo, " . ", simplify = T)[,1],
      cerveja = str_split(nome.completo," . ", simplify = T)[,2]
    ) -> beers.name

  html_doc %>% 
    html_nodes("div .post") %>%
    html_nodes("h2 a:first-child") %>%
    html_attr("href") %>%
    as.tibble() %>%
    rename(link.avaliacao=value) -> beers.eval_link
  
  html_doc %>% 
    html_nodes(".main") %>% 
    map(function(x){
      html_node(x,"img") %>%
        html_attr("src") %>%
        head(1)
    }) %>%
    str_replace("\\?.*","") %>%
    as.tibble() %>% 
    rename(image=value) -> beers.image
  
  html_doc %>%
    html_nodes(".main p") %>%
    html_text() %>%
    map( ~Filter(function(x) str_count(x,"Pa�s: ")>0,.) ) %>% 
    unlist() %>% str_split("\n") %>%
    map(function(texts){
      str_replace(texts, ".+: ", "") %>%
        str_replace(.,"\\.+$","") %>%
        head(9) # somente sete atributos
    }) %>%
    unlist() %>% as.vector() %>%
    matrix(ncol=9, byrow = T) %>%
    as.tibble() %>%
    setNames(c("pais","tipo","alcool",
               "cor","sabor","malte",
               "avaliacao","preco","volume")) -> beers.eval
  
  html_doc %>%
    html_nodes(".main p:last-of-type") %>% 
    map(function(x){
      link <- html_nodes(x,"a:first-child")
    }) %>%
    map(function(x){
      if (length(x)>0) { html_attr(x,"href") }
        else {return(NA)}
    }) %>% unlist() %>%
    as.tibble() %>% 
    rename(url=value) -> beers.url
  
  html_doc %>%
    html_nodes(".signature p:last-of-type") %>%
    html_text() %>%
    dmy_hm() %>%
    as.tibble() %>%
    rename(data.avalicao=value) -> beers.eval_date
  
  bind_cols( beers.name, beers.eval, beers.eval_date,
             beers.eval_link, beers.url, beers.image) %>% return()

}

pages %>%
  paste0(base.url, "page/", .) %>%
  map_df(possibly(scrapBeerPage,NULL)) -> beers

beers %>%
  mutate(
    cervejaria = as.factor(cervejaria),
    pais = as.factor(pais),
    tipo = as.factor(tipo),
    avaliacao = as.integer(avaliacao),
    alcool = as.numeric(str_replace(str_replace(alcool,"%",""),",","."))/100   
  ) %>%
  filter( !is.na(alcool) ) -> beers

saveRDS(beers,"./BeerComparator/data/beers.rds")
beers <- readRDS("./BeerComparator/data/raw_beers.rds")


