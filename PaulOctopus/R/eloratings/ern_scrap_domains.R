# package setup and common functions
source("./PaulOctopus/R/eloratings/ern_common.R")

# o 'negativo' que vem no dataset eh um caracter estranho, precisa substituir antes de passar para numero
.convertNumber <- function(x) x %>% iconv() %>% gsub("â^'","-",.) %>% as.integer()

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
                    "home.team.cod","away.team.cod","home.score","away.score",
                    "tournament.cod","location",
                    "home.deltaRating", "home.newRating", "away.newRating",
                    "home.deltaRank", "away.deltaRank",
                    "home.newRank","away.newRank"),
      quote="") %>%
    mutate( match.date  = ymd(paste0(match.year, match.month, match.day)),
            match.month = as.integer(match.month),
            match.day   = as.integer(match.day),
            location    = ifelse(is.na(location),home.team.cod,location),
            home.deltaRank = .convertNumber(home.deltaRank),
            away.deltaRank = .convertNumber(away.deltaRank),
            away.deltaRating  = -home.deltaRating,
            home.rating    = home.newRating - home.deltaRating,
            away.rating    = away.newRating - away.deltaRating,
            home.rank      = home.newRank + home.deltaRank,
            away.rank      = away.newRank + away.deltaRank,
            home.atHome    = ifelse(is.na(location),0,as.integer(home.team.cod==location))) %>% View()
    return()
}


elo_scrapRank <- function(year) {
  elo_getUrl(ELOR$year_start_ratings, year) %>%
    readr::read_tsv(col_names = c("mark","rank","team.cod","rating",
                                  "rank.highest", "rating.highest",
                                  "rank.average","rating.average",
                                  "rank.lowest","rating.lowest",
                                  "rank.delta.3m", "rating.delta.3m",
                                  "rank.delta.6m", "rating.delta.6m",
                                  "rank.delta.1y", "rating.delta.1y",
                                  "rank.delta.2y", "rating.delta.2y",
                                  "rank.delta.5y", "rating.delta.5y",
                                  "rank.delta.10y", "rating.delta.10y",
                                  "total.matchs", "total.matchs.home", "total.matchs.away", "total.matchs.neutral",
                                  "total.wins", "total.losses", "total.draws",
                                  "total.goals.for", "total.goals.against"),
                    quote = "") %>%
    select(-mark) %>%
    mutate( rank.date = ymd(paste0(year,"0101")) ) %>%
    mutate_at(.vars=vars(rank.delta.3m:rating.delta.10y), .funs=.convertNumber ) %>%
    return()
}
