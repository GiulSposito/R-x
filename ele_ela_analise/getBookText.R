library(pdftools)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(tidytext)

getBookPages <- function(link){
  tfile <- tempfile()
  download.file(link, tfile, mode = "wb")
  txt  <- pdf_text(tfile)
  
  return( tibble(page=1:length(txt), text=txt))
}

processBookPages <- function(pages){

  bigrams <- pages  %>%
    unnest_tokens(bigram, text, token = "ngrams",
                  n = 2, collapse = FALSE)
  
  bigrams_separated <- bigrams %>%
    tidyr::separate(bigram, c("subject", "verb"), sep = " ")
  
  bigrams_separated %>%
    filter(subject %in% c("ele", "ela")) %>%
    return()
}

books <- readRDS("./ele_ela_analise/data/book_links.rds")

n_books <- 60

books$download %>%
  head(n_books) %>%
  map(getBookPages) %>%
  setNames(books$book.id[1:n_books]) %>%
  bind_rows(text, .id = "book.id") %T>%
  saveRDS("./ele_ela_analise/data/book_texts.rds") %>%
  select(-page) %>%
  processBookPages() -> he_she_verbs

he_she_verbs %>%
  count(subject, verb, sort = TRUE) %>%
  rename(total = n) %>%
  spread(subject, total, fill=0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log2(ela / ele))%>% 
  arrange(abs(logratio)) %>%
  mutate(abslogratio = abs(logratio)) %>%
  group_by(logratio < 0) %>%
  top_n(10, abslogratio) %>%
  ungroup() %>%
  mutate(verb = reorder(verb, logratio)) %>%
  ggplot(aes(verb, logratio, color = logratio < 0)) +
  geom_segment(aes(x = verb, xend = verb,
                   y = 0, yend = logratio), 
               size = 1.1, alpha = 0.6) +
  geom_point(size = 3.5) +
  coord_flip() +
  labs(x = NULL, 
       y = "Relative appearance after 'she' compared to 'he'",
       title = "Words paired with 'he' and 'she' in Jane Austen's novels",
       subtitle = "Women remember, read, and feel while men stop, take, and reply") +
  scale_color_discrete(name = "", labels = c("More 'she'", "More 'he'")) +
  scale_y_continuous(breaks = seq(-3, 3),
                     labels = c("0.125x", "0.25x", "0.5x", 
                                "Same", "2x", "4x", "8x"))
