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
    select(match.date, home.team.cod, match.score, home.score,
           away.score, away.team.cod, tournament.cod)
  
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

  match.stats %>%
    mutate( home.team = factor(home.team, levels=lvl.tc),
            away.team = factor(away.team, levels=lvl.tc)) -> game.stats
  
  # remove label dos times
  game.stats %>%
    select( -match.score ) -> game.stats
  
  wc2014 <- game.stats %>%
    filter(tournament.cod=="WC", year(match.date)==2014)
  
  games.train <- game.stats %>%
    filter( #year(match.date)>=1998,
            tournament.importance >= 5,
            match.date < min(wc2014$match.date)) %>%
    select( -match.date )
  
  games.train.x <- games.train %>% select(-home.score, -away.score)
  games.train.y.home.score <- games.train$home.score
  games.train.y.away.score <- games.train$away.score
  
  games.test.x <- wc2014 %>% select( -home.score, -away.score )
  games.test.y.home.score <- wc2014$home.score
  games.test.y.away.socre <- wc2014$away.score
  
  library(caret)
  library(catboost)
  
  fit_control <- trainControl(method = "cv",
                              number = 2)
  
  grid <- expand.grid(depth = 6,
                      learning_rate = 0.1,
                      iterations = 100,
                      l2_leaf_reg = 1e-3,
                      rsm = 0.95,
                      border_count = 64)
  
  report.home.score <- train(games.train.x, games.train.y.home.score,
                  method = catboost.caret,
                  logging_level = 'Verbose', preProc = NULL,
                  tuneGrid = grid, trControl = fit_control)
  
  
  importance <- varImp(report.home.score, scale = FALSE)
  print(importance)
  
  #prob.home.score <- predict(report.home.score, games.test.x,  type = "prob")
  y_hat <- round(predict(report.home.score, games.test.x,  type = "raw"),0)
  y_hat
  games.test.y.home.score
  
  confusionMatrix(y_hat,games.test.y)
  
  result <- tibble(pred=as.character(y_hat), ref=as.character(games.test.y)) %>%
    mutate( result= (pred==ref) )
  
  mean(result$result)
    
  
}
