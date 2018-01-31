library(tidyverse)
library(tidytext)

beers <- readRDS("./BeerComparator/data/beers.rds")

pt_stopwords <- read_table("./BeerComparator/data/stopwords.txt", 
                           col_names = "word")

beers %>%
  mutate( review = paste0(malte, " ", sabor) ) %>% 
  select(nome.completo, tipo, review) %>%
  unnest_tokens(word, review) %>% 
  anti_join(pt_stopwords) %>% 
  count(word, tipo) -> beer_wordc

beer_wordc %>% arrange(desc(n)) %>% head(20)

beer_wordc %>% 
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  bind_tf_idf(word, tipo, n)  %>%
  subset(tf_idf > 0) %>%
  arrange(desc(tf_idf)) -> beer_tf_idf

head(beer_tf_idf)


# create a df for plotting of 
# the top 16 beers by review count
top_beers <- aggregate(cerveja ~ tipo, beers, sum) %>% top_n(16, Reviews)

count(beers, tipo, sort=T) %>% top_n(25,n) %>% head(25) -> top_beers

beer_tidy_tfidf_10 <- beer_tf_idf %>%  
  subset(tipo %in% top_beers$tipo & word_total >= 10) %>%
  group_by(tipo) %>%
  top_n(10, tf_idf) %>%
  arrange(tipo, desc(tf_idf)) %>%
  ungroup() %>%
  mutate(Rank = rep(10:1, 25))

# styling omitted for brevity
ggplot(beer_tidy_tfidf_10, aes(x=as.factor(Rank), y=tf_idf)) +  
  geom_bar(stat="identity", fill="cadetblue", alpha=0.5) + 
  coord_flip() + facet_wrap(~ tipo,ncol=5) + 
  geom_text(aes(label=word, x=Rank), y=0,hjust=0, size=3) +
  labs(title="Top TF-IDF Terms for Selected Beer Styles\n", 
       x="", y="tf-idf") 
