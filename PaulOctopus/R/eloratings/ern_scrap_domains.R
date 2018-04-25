# package setup and common functions
source("./PaulOctopus/R/eloratings/ern_common.R")

charFromHell <- "\ue2"

elo_scrapTeam <- function(){
  elo_getUrl(ELOR$teams) %>%
    readr::read_tsv(col_names = c("team.cod","team.name"), quote="", col_types = "cc") %>%
    return()
}

elo_scrapTournaments <- function(){
  elo_getUrl(ELOR$tournaments) %>%
    readr::read_tsv(col_names = c("tournament.cod","tournament.name"), quote="", col_types = "cc") %>%
    return()
}

elo_scrapLabels <- function(){
  elo_getUrl(ELOR$labels) %>%
    readr::read_tsv(col_names = c("label.cod","label.name"), quote="", col_types = "cc") %>%
    return()
}

elo_scrapResults <- function(year){
  elo_getUrl(ELOR$year_matchs, year) %>%
    readr::read_tsv(
      col_names = c("match.year","match.month","match.day",
                    "home.team","away.team","home.score","away.score",
                    "tournament.cod","location",
                    "home.deltaElo", "home.newElo", "away.newElo",
                    "home.deltaRank", "away.deltaRank",
                    "home.newRank","away.newRank"),
      quote="") %>%
    mutate( match.date  = ymd(paste0(match.year, match.month, match.day)),
            match.month = as.integer(match.month),
            match.day   = as.integer(match.day),
            home.deltaRank = home.deltaRank %>% iconv() %>% gsub("â^'","-",.) %>% as.integer(),
            away.deltaRank = away.deltaRank %>% iconv() %>% gsub("â^'","-",.) %>% as.integer(),
            away.deltaElo  = -home.deltaElo ) %>%
    return()
}

