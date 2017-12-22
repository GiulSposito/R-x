# reproducing this code: https://msperlin.github.io/2017-01-18-GetHFData/


# the FTP download is downloaded at the root
# change the working dir instead
setwd("./GetHFData/")

# setup
library(GetHFData)
library(tidyverse)


n.assets <- 6
my.date <- as.Date('2017-12-19')
type.market <- 'equity'

df.tickers <- ghfd_get_available_tickers_from_ftp(my.date = my.date, 
                                                  type.market = type.market)
