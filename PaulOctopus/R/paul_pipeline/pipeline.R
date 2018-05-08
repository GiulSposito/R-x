# import domain
source("./PaulOctopus/R/infra/bigQuery.R")
source("./PaulOctopus/R/paul_pipeline/worldCupMatches.R")

loadBigQuery <- function(){
  # import EloRatings Domain
  source("./PaulOctopus/R/paul_pipeline/execDataScrap.R")
  importEloRatings() -> elo.tables
  
  # import 2018 world cup matches
  importWorldCupMatches() -> wc.matches
}
