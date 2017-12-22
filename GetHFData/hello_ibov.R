# reproducing https://msperlin.github.io/2017-01-18-GetHFData/


# the FTP download is downloaded at the root
# change the working dir instead
setwd("./GetHFData/")

# setup
library(GetHFData)
library(tidyverse)
library(ggplot2)

# let's get the som cotation
df.tickers <- ghfd_get_available_tickers_from_ftp("2017-12-19", "equity")

# plot 25 top volumes
ggplot( df.tickers[1:25,], aes(x=reorder(tickers,-n.obs), y=n.obs) ) +
  geom_bar( stat = "identity" ) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  labs( x="Tickers", y="Number of trades")

# lets see 6 top
my.assets <- df.tickers[1:n.assets,]
my.assets

# let's see intraday operations
df.out <- ghfd_get_HF_data(my.assets = my.assets$tickers,
                           type.market = "equity",
                           first.date = "2017-12-18",
                           last.date = "2017-12-22",
                           first.time = "10:00:00",
                           last.time = "17:00:00",
                           type.output = "agg",
                           agg.diff = "15 min")

str(df.out)

ggplot(df.out, aes(x =  Tradetime, y = n.trades)) + 
  geom_boxplot() + coord_cartesian(ylim = c(0, 3000)) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  facet_wrap(~InstrumentSymbol) +
  labs(y='Number of Trades', x = 'Time of Day')
  

