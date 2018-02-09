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

extractHeSheVerbs <- function(pages){

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
  head(2200) %>% 
  group_by(title) %>%
  filter(book.id==max(book.id)) %>%
  ungroup() %>% 
  assign("books",.,envir = .GlobalEnv) %>%
  apply(., 1,function(x){
    downloadBookCached(x["book.id"], x["download"])  
  }) %>%
  map(getBookPages) %>%
  setNames(books$book.id) %>% 
  bind_rows(.id = "book.id") -> book_texts

saveRDS(book_texts, "./ele_ela_analise/data/book_texts.rds") 

book_texts %>%
  select(-page) %>%
  extractHeSheVerbs() -> he_she_verbs

saveRDS(he_she_verbs,"./ele_ela_analise/data/he_she_verbs.rds") 



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



he_she_verbs %>%
  filter(total>= 100) %>%
  ggplot(aes(total, log_ratio)) +
  geom_point() +
  scale_x_log10(breaks = c(100, 1000, 10000, 1e5),
                labels = comma_format()) +
  geom_text(aes(label = word2), vjust = 1, hjust = 1,
            check_overlap = TRUE) +
  scale_y_continuous(breaks = seq(-2, 2),
                     labels = c('4X "he"', '2X "he"', "Same", '2X "she"', '4X "she"')) +
  labs(x = 'Total uses after "he" or "she" (note log scale)',
       y = 'Relative uses after "she" to after "he"',
       title = "Gendered verbs: comparing frequency to pronoun shift",
       subtitle = "Only words occurring at least 100 times after he/she. Overlapping labels were removed.") +
  expand_limits(x = 75)
