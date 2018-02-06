library(pdftools)
library(dplyr)
library(tidytext)



getBookPages <- function(abook){
  
  tfile <- tempfile()
  download.file(abook$download, tfile, mode = "wb")
  txt  <- pdf_text(tfile)
  
  return( tibble(book.id=as.integer(abook$book.id), paragraph=txt))    
}

processBookPages <- function(pages){

  bigrams <- pages  %>%
    unnest_tokens(bigram, paragraph, token = "ngrams",
                  n = 2, collapse = FALSE)
  
  bigrams_separated <- bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  he_she_words <- bigrams_separated %>%
    filter(word1 %in% c("ele", "ela"))
}

pages <- getBookPages(abook)
he_she_words <- processBookPages(pages)
