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
  
  return(featset)
}


simpleTest <- function(){
  game.stats <- genFeatures()
  team.stats <- genTeamStats(readTable("results"))
  # worldCupGames <- readTable("worldCupMatches")
  
  wc2014 <- game.stats %>%
    filter(tournament.cod=="WC", year(match.date)==2014)
  
  games.train <- game.stats %>%
    filter( year(match.date)>=2007,
            match.date < min(wc2014$match.date) ) %>%
    select( -home.team, -away.team, -match.date )
  
  games.train.y <- games.train$match.score
  games.train.x <- games.train %>% select(-match.score)
  
  games.test.x <- wc2014 %>% select( -home.team, -away.team, -match.date, -match.score )
  games.test.y <- wc2014 %>% select( match.score )
  
  library(caret)
  library(catboost)
  
  fit_control <- trainControl(method = "cv",
                              number = 4,
                              classProbs = TRUE)
  
  grid <- expand.grid(depth = c(4, 6, 8),
                      learning_rate = 0.1,
                      iterations = 20,
                      l2_leaf_reg = 1e-3,
                      rsm = 0.95,
                      border_count = 64)
  
  report <- train(games.train.x, as.factor(make.names(games.train.y)),
                  method = catboost.caret,
                  logging_level = 'Verbose', preProc = NULL,
                  tuneGrid = grid, trControl = fit_control)
  
  
  importance <- varImp(report, scale = FALSE)
  print(importance)
  
  y_hat.prob <- predict(report, games.test.x,  type = "raw")
  
  make.names(games.test.y$match.score)
  y_hat.prob

  lvl <- levels(make.names(games.train.y))
    
  confusionMatrix(factor(y_hat.prob, levels = lvl), factor(make.names(games.test.y$match.score), levels=lvl))
  
  View(data.frame(pred=y_hat.prob, ref=make.names(games.test.y$match.score)))
  
}
