# the FTP download is downloaded at the root
# change the working dir 
setwd("./GetHFData/")

# script to test the basics of GetFHData
install.packages("GetHFData")

# load libs
library(GetHFData)
library(tidyverse)

# dates to import
dates <- c("2017-12-18","2017-12-19","2017-12-20","2017-12-21")
ftp <- list()

# download the FTPs files (one for each date)
ftp <- lapply(dates, function(x) ghfd_get_available_tickers_from_ftp(x, "BMF")[1:5,1:2] )
names(ftp) <- dates

# bind the dataframes
FTP <- do.call(rbind, ftp)
FTP <- FTP %>% 
  mutate(data = row.names(.),
         data = gsub("\\.\\d", "", data),
         data = as.Date(data),
         tickers = as.character(tickers))

# plot the number of observations
ggplot(FTP, aes(x = data, y = n.obs, group = tickers)) +
  geom_line(aes(colour = tickers)) +
  theme_classic()
