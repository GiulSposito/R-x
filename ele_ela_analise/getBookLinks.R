# scrap web pages to get clear links

library(rvest)
library(tidyverse)
library(stringr)

books.index <- "http://noticias.universia.com.br/destaque/noticia/2011/10/07/876201/50-livros-classicos-em-portugues-download-gratis.html"


html.doc <- read_html(books.index)

html.doc %>%
  html_nodes("p strong a") -> a.tags

a.tags %>%
  html_attr("href") -> links

a.tags %>%
  html_text() -> title.author

title.author %>%
  str_match(". (.+),") -> titles

title.author %>%
  str_match(", de (.+)") -> authors

books <- tibble(id=1:length(links), title=titles[,2], author=authors[,2], link=links)

saveRDS(books,"./ele_ela_analise/data/book_links.rds")
