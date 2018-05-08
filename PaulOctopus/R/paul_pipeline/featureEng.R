# scripts para geracao de fetaures
library(zoo)
library(tidyverse)
library(lubridate)

genTournamentFeatures <- function(.tournaments){
  
  # procura por 
  .tables$tournaments %>%
    mutate( tournament.name = tolower(tournament.name) ) %>%
    filter( grepl(".*qualif.*", tournament.name) ) %>%
    filter( grepl(".*world *cup.*", tournament.name) ) %>%
    select(tournament.cod) %>% 
    unlist() -> wc.qualify
  
  .tournaments %>%
    mutate( tournament.importance = case_when(
      tournament.cod ==  "WC"               ~ 5, # world cup
      tournament.cod %in% wc.qualify        ~ 4, # eliminatorias da copa
      tournament.cod %in% c("CA","EC","CC") ~ 3, # copa america e euro copa
      tournament.cod %in% c("OG","AR",
                            "AC","CGC")     ~ 2, # africa nations cup | asian cup | olimpic games | concacaf golden cup
      tournament.cod %in% c("F","FT")       ~ 1, # amistosos
      TRUE                                  ~ 0  # qualquer outro
    ) %>% as.integer() ) %>%
    return()
  
}

genResultsFeatures <- function(.results){
  
  .results %>%
    mutate(
      match.score = as.factor(paste0( home.score, " x ", away.score )),
      home.win    = as.integer(home.score > away.score),
      away.win    = as.integer(home.score < away.score),
      match.draw  = as.integer(home.score == away.score),
      home.net.score = home.score - away.score,
      away.net.score = -home.net.score
    ) %>%
    arrange(match.date) %>%
    select(match.date, 
           home.newRank, home.deltaRank, home.rank, 
           home.newRating, home.deltaRating, home.rating, 
           home.team.cod, home.score, 
           match.score,
           away.score, away.team.cod,
           away.newRating, away.deltaRating, away.rating, 
           away.rank, away.deltaRank, away.newRank) %>%
    View()
  
}

  