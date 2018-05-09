# scripts para geracao de fetaures
library(zoo)
library(tidyverse)
library(lubridate)
source("./PaulOctopus/R/infra/bigQuery.R")

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
      away.net.score = -home.net.score,
      match.rank.diff   = home.rank - away.rank,
      match.rating.diff = home.rating - away.rating
    ) %>%
    group_by(home.team.cod) %>%
    arrange(home.team.cod, match.date) %>%
    mutate(
      # saldos de goals
      home.goals.pro.L1     = as.integer(lag(home.score,k=2, default=0)),
      home.goals.pro.L2     = as.integer(rollsum(home.goals.pro.L1, k=2, na.pad=T, fill=0, align = "right")),
      home.goals.pro.L3     = as.integer(rollsum(home.goals.pro.L1, k=3, na.pad=T, fill=0, align = "right")),
      home.goals.pro.L5     = as.integer(rollsum(home.goals.pro.L1, k=5, na.pad=T, fill=0, align = "right")),
      home.goals.pro.L10     = as.integer(rollsum(home.goals.pro.L1, k=10, na.pad=T, fill=0, align = "right")),
      home.goals.pro.L20     = as.integer(rollsum(home.goals.pro.L1, k=20, na.pad=T, fill=0, align = "right")),
      home.goals.against.L1 = as.integer(lag(away.score,k=2, default=0)),
      home.goals.against.L2 = as.integer(rollsum(home.goals.against.L1, k=2, na.pad=T, fill=0, align = "right")),
      home.goals.against.L3 = as.integer(rollsum(home.goals.against.L1, k=3, na.pad=T, fill=0, align = "right")),
      home.goals.against.L5 = as.integer(rollsum(home.goals.against.L1, k=5, na.pad=T, fill=0, align = "right")),
      home.goals.against.L10 = as.integer(rollsum(home.goals.against.L1, k=10, na.pad=T, fill=0, align = "right")),
      home.goals.against.L20 = as.integer(rollsum(home.goals.against.L1, k=20, na.pad=T, fill=0, align = "right"))
    ) %>%
    filter(home.team.cod %in% c("BR","GR")) %>%
    select(match.date, home.team.cod, match.score, away.team.cod, 
           home.goals.pro.L1, home.goals.against.L1,
           home.goals.pro.L2, home.goals.against.L2,
           home.goals.pro.L3, home.goals.against.L3,
           home.goals.pro.L5, home.goals.against.L5,
           home.goals.pro.L10, home.goals.against.L10,
           home.goals.pro.L20, home.goals.against.L20) %>% View()
    
  
hs <- .results$home.score %>% head(10)

  
  
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

  