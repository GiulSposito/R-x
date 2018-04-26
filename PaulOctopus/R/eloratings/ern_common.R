# package setup

# verifica a existencia do needs
if (!"needs" %in% installed.packages()) install.packages("needs")

# basic package
library(needs)
needs(rvest)
needs(tidyverse)

# url constants
ELOR = list(
  base   = "http://eloratings.net/",
  labels = "http://eloratings.net/en.labels.tsv",
  teams  = "http://eloratings.net/en.teams.tsv",
  tournaments = "http://eloratings.net/en.tournaments.tsv",
  current_ratings = "http://eloratings.net/World.tsv",
  lastest_results = "http://eloratings.net/latest.tsv",
  year_final_ratings = "http://eloratings.net/AAAA.tsv",
  year_start_ratings = "http://eloratings.net/AAAA_start.tsv",
  year_matchs        = "http://eloratings.net/AAAA_results.tsv"
)

# function to calc specific urls
.urlAddTimestamp <- function(url) paste0(url, "?_=", as.integer(Sys.time())) 
.urlAddYear <- function(url,year) url %>% gsub("AAAA",year,.)
.elo_getUrl <- function(url, year="") url %>% .urlAddTimestamp() %>% .urlAddYear(year)

# examples
# getEloUrl(ELOR$year_matchs,2011)
# getEloUrl(ELOR$teams,2011)