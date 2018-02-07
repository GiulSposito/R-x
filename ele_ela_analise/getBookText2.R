library(pdftools)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(tidytext)
library(ptstem)


downloadBookCached <- function(book.id, link, cache.dir="./ele_ela_analise/data/book/"){
  print(paste0("getting: ", book.id))
  full.path.name <- paste0(cache.dir, book.id, ".pdf")
  if (!file.exists(full.path.name)){
    print("downloading...")
    tryCatch({
      download.file(link, full.path.name, mode = "wb", quiet = T)
    }, condition = function(err) { full.path.name <<- "" })
  } 
  return(full.path.name)
}

getBookPages <- function(fname){
  print(paste0("reading: ", fname))
  if(fname=="") return(NULL)
  txt  <- tryCatch({
    pdf_text(fname)
  }, condition = function(err) { return("") })

  return( tibble(page=1:length(txt), text=txt))
}

processBookPages <- function(pages){

  bigrams <- pages  %>% 
    unnest_tokens(bigram, text, token = "ngrams",
                  n = 2, collapse = FALSE)
  
  bigrams_separated <- bigrams %>%
    tidyr::separate(bigram, c("subject", "verb"), sep = " ")
  
  
  not_verbs <- read.table("./ele_ela_analise/data/notverbs.txt",
                          col.names = "verb")
  
  bigrams_separated %>%
    filter(subject %in% c("ele", "ela")) %>%
    mutate( verb = ptstem(verb) ) %>%
    anti_join(not_verbs) %>%
    return()
}

readRDS("./ele_ela_analise/data/book_links.rds") %>%
  head(550) %>%
  assign("books",.,envir = .GlobalEnv) %>%
  apply(., 1,function(x){
    downloadBookCached(x["book.id"], x["download"])  
  }) %>%
  map(getBookPages) %>%
  setNames(books$book.id) %>% 
  bind_rows(.id = "book.id") %T>%
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
  scale_color_discrete(name = "", labels = c("Mais'ela'", "mais 'ele'")) +
  scale_y_continuous(breaks = seq(-3, 3),
                     labels = c("0.125x", "0.25x", "0.5x", 
                                "Igual", "2x", "4x", "8x"))






