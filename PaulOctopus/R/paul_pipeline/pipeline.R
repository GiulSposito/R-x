# import domain
source("./PaulOctopus/R/infra/bigQuery.R")

loadBigQuery <- function(){
  # import EloRatings Domain
  source("./PaulOctopus/R/paul_pipeline/execDataScrap.R")
  importEloRatings() -> elo.tables
  
  # import 2018 world cup matches
  source("./PaulOctopus/R/paul_pipeline/worldCupMatches.R")
  importWorldCupMatches() -> wc.matches
}

genFeatures <- function(){
  
  source("./PaulOctopus/R/paul_pipeline/featureEng.R")
  readTable("tournaments") %>%
    genTournamentFeatures() -> tournaments
  
  source("./PaulOctopus/R/paul_pipeline/featureEngTeams.R")
  readTable("results") %>%
    mutate(
      match.score = as.factor(paste0( home.score, " x ", away.score ))
    ) -> results
   
  stats <- genTeamStats(results)

  matchs <- results %>%
    select(match.date, home.team.cod, match.score, away.team.cod, tournament.cod)
  
  featset <- stats %>%
    set_names(paste0("home.", names(.))) %>%
    inner_join(matchs, by=c("home.date"="match.date","home.team"="home.team.cod"))
  
  featset <- stats %>%
    set_names(paste0("away.", names(.))) %>%
    inner_join(featset, by=c("away.date"="home.date","away.team"="away.team.cod")) %>%
    rename(match.date = away.date) %>%
    inner_join(select(tournaments, -tournament.name), by="tournament.cod") %>%
    mutate(tournament.cod=as.factor(tournament.cod))
  
  
}