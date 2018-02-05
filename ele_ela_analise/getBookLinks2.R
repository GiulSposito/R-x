# scrap web pages to get clear links

library(rvest)
library(tidyverse)
library(stringr)

n <- "2112"
url.books.index <- "http://www.dominiopublico.gov.br/pesquisa/ResultadoPesquisaObraForm.do?first=50&skip=0&ds_titulo=&co_autor=&no_autor=&co_categoria=2&pagina=2&select_action=Submit&co_midia=2&co_obra=&co_idioma=1&colunaOrdenar=null&ordem=null"

# url.book.detail <- "http://www.dominiopublico.gov.br/pesquisa/DetalheObraForm.do?select_action=&co_obra=81912"
# url.book.download <- "http://www.dominiopublico.gov.br/pesquisa/DetalheObraDownload.do?select_action=&co_midia=2&co_obra=81912"
url.book.download <- "http://www.dominiopublico.gov.br/pesquisa/DetalheObraDownload.do?select_action=&co_midia=2&co_obra="


html.doc <- read_html(url.books.index)

html.doc %>%
  html_node("#res") %>%
  html_table(fill=T, trim=T, header=F) -> books.table

books.table[8:nrow(books.table), 3:8] %>%
  setNames(c("title","author","source","format","size","access")) -> books.attribs

html.doc %>%
  html_nodes("#res tbody tr td a") %>%
  html_attr("href") -> links

html.doc %>%
  html_nodes("#res tbody tr td a") %>%
  html_text(trim=T) -> titles

books.attribs %>% 
  inner_join(tibble(title=titles, link=links)) %>%
  mutate(book.id = str_match(link,".+co_obra=(.+)")[,2]) %>%
  mutate(download = paste0(url.book.download,book.id)) -> books

saveRDS(books,"./ele_ela_analise/data/book_links.rds")
