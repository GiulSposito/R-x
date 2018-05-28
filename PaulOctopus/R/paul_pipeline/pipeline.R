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
  
  
  match.stats <- genFeatures()
  team.stats <- genTeamStats(readTable("results"))

  lvl.tc <- team.stats$team %>% unique() %>% as.factor() %>% levels()
  lvl.sc <- match.stats$match.score %>% unique() %>% make.names() %>% as.factor() %>% levels()
  
  match.stats %>%
    mutate( home.team = factor(home.team, levels=lvl.tc),
            away.team = factor(away.team, levels=lvl.tc), 
            match.score = match.score %>% make.names() %>% factor(levels = lvl.sc)
            ) -> game.stats
  
  # remove label dos times
  game.stats %>%
    select( -home.team, -away.team ) -> game.stats
  
  wc2014 <- game.stats %>%
    filter(tournament.cod=="WC", year(match.date)==2014)
  
  games.train <- game.stats %>%
    filter( #year(match.date)>=2000,
            match.date < min(wc2014$match.date),
            tournament.importance >=5 ) %>%
    select( -match.date )
  
  games.train.x <- games.train %>% select(-match.score)
  games.train.y <- games.train$match.score
  
  games.test.x <- wc2014 %>% select(  -match.date, -match.score )
  games.test.y <- wc2014$match.score
  
  library(caret)
  library(catboost)
  
  fit_control <- trainControl(method = "cv",
                              number = 2,
                              classProbs = TRUE)
  
  grid <- expand.grid(depth = 6,
                      learning_rate = 0.1,
                      iterations = 100,
                      l2_leaf_reg = 1e-3,
                      rsm = 0.95,
                      border_count = 64)
  
  report <- train(games.train.x, games.train.y,
                  method = catboost.caret,
                  logging_level = 'Verbose', preProc = NULL,
                  tuneGrid = grid, trControl = fit_control)
  
  
  importance <- varImp(report, scale = FALSE)
  print(importance)
  
  y_hat.prob <- predict(report, games.test.x,  type = "prob")
  y_hat <- predict(report, games.test.x,  type = "raw")
  
  confusionMatrix(y_hat,games.test.y)
  
  result <- tibble(pred=as.character(y_hat), ref=as.character(games.test.y)) %>%
    mutate( result= (pred==ref) )
  
  mean(result$result)
    
  
}
