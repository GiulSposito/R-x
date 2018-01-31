library(tidyverse)
library(tidytext)
library(ptstem)
library(corrplot)
library(ape)

beers <- readRDS("./BeerComparator/data/beers.rds")

pt_stopwords <- read_table("./BeerComparator/stopwords.txt", 
                           col_names = "word")
my_stopwords <- read_table("./BeerComparator/my_stopwords.txt", 
                           col_names = "word")

stopwords <- bind_rows(pt_stopwords, my_stopwords)

beers %>%
  mutate( review = paste0(malte, " ", sabor, " ", cor) ) %>% 
  select( tipo, review ) %>%
  unnest_tokens( word, review ) %>% 
  anti_join( stopwords ) %>% 
  mutate( word = ptstem(word) ) %>%
  count( word, tipo ) -> beer_wordc

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
# top_beers <- aggregate(cerveja ~ tipo, beers, sum) %>% top_n(16, Reviews)

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
       x="", y="tf-idf") + theme_bw()


# get the proportion of words in each 
# style and create a matrix with
# styles as columns and words as rows
beer_corr <- beer_wordc %>%  
  subset(!is.na(tipo)) %>%
  group_by(tipo) %>%
  mutate(prop = n / sum(n))  %>%
  subset(n >= 5) %>%
  select(-n) %>%
  spread(tipo, prop) 

# replace NAs with 0 because an NA 
# is an observation of 0 words
beer_corr[is.na(beer_corr)] <- 0 

mycol <- colorRampPalette(c("darkgrey", "grey", "white", "cadetblue1", "cadetblue"))  
corr <- cor(beer_corr[,-1], use = "pairwise.complete.obs") %>%  
  corrplot(method="color", order="hclust", diag=FALSE, 
           tl.col = "black", tl.srt = 45, tl.cex=0.6,
           #col=mycol(100), 
           type="lower",
           title="Correlation Between Beer Styles", 
           family="Avenir",
           mar=c(0,0,1,0))

# transpose the matrix to have styles
# as rows and words as columns
beer_corr_t <- t(beer_corr[,-1])

# calculate distance
beer_dist <- dist(beer_corr_t, method="euclidean")

# fit clusters
fit <- hclust(beer_dist, method="ward.D")

# plot 
plot(as.phylo(fit),main="Cluster Dendrogram of Beer Styles",  
     family="Avenir")
# rect.hclust(fit, k=8, border="cadetblue")


