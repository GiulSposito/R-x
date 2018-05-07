source("./PaulOctopus/R/eloratings/ern_scrap_domains.R")
source("./PaulOctopus/R/infra/bigQuery.R")

# faz o scrap das informações do site EloRating
scrapEloRatings <- function(scrap.years = 1999:2018){
  
  # basic domains 
  elo.labels  <- elo_scrapLabels()
  elo.teams   <- elo_scrapTeam()
  elo.tourn   <- elo_scrapTournaments()
  
  # resultados dos jogos
  scrap.years %>% 
    map(elo_scrapResults) %>%
    bind_rows() -> elo.results 
  
  # ranking anuais
  scrap.years[1:(length(scrap.years)-1)] %>%
    map(elo_scrapRank) %>%
    set_names(scrap.years[1:(length(scrap.years)-1)]) %>%
    bind_rows(.id = "rank.year") %>%
    mutate(rank.year=as.integer(rank.year)) -> elo.rank
  
  # agrupa as tabelas e retorna como um alista
  list(
    labels = elo.labels,
    teams  = elo.teams,
    tournaments = elo.tourn,
    results = elo.results,
    rank    = elo.rank
  ) %>% return()
}

# salva um conjunto de tabelas (numa lista nomeada) no BigQuery
saveTablesToBigQuery <- function(.tables){
  
  1:length(.tables) %>%
    map(function(.idx, .tblist){
      tn <- names(.tblist)[.idx]
      tb <- .tblist[[.idx]]
      print(paste0("Saving ", tn, " [", nrow(tb), ",", ncol(tb), "] to BigQuery..."))
      createTable(tn,tb)
    }, .tblist=.tables)
  
}

importEloRatings <- function(scrap.years = 1999:2018){
  scrap.years %>%
    scrapEloRatings() %T>%
    saveTablesToBigQuery() %>%
    return()
}