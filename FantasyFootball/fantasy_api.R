library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(yaml)

config <- yaml.load_file("./FantasyFootball/config.yml")

leagueId  <- config$leagueId
teamId    <- config$teamId 
week      <- config$week
authToken <- config$authToken

url <- glue("http://api.fantasy.nfl.com/v1/league/team/matchup?leagueId={leagueId}&teamId={teamId}&week={week}&format=json&authToken={authToken}")

resp <- httr::GET(url)

resp$status_code

httr::content(resp, as = "text") %>% 
  jsonlite::fromJSON(simplifyDataFrame = T) -> matchup

matchup[[1]]$matchup$homeTeam$players[[1]] %>%
  select( src_id=id, name, position, rosterSlot, fantasyPts ) %>%
  jsonlite::flatten() %>% 
  select(-fantasyPts.week.season, -fantasyPts.week.week ) %>% 
  rename(points = fantasyPts.week.pts) -> hometeam

scrap$QB %>% names()

scrap <- readRDS("./FantasyFootball/weekly_scrap.rds")
scrap %>%
  map(function(dft){
  dft %>% 
    filter(data_src=="NFL") %>% 
    select(id, src_id, player, team, pos) %>% 
    return()
  }) %>% 
  bind_rows() %>%
  distinct() -> player_ids

saveRDS(player_ids, "./FantasyFootball/player_ids_nfl.rds")  

source("../ffanalytics/R/calc_projections.R")
source("../ffanalytics/R/stats_aggregation.R")
source("../ffanalytics/R/source_classes.R")
source("../ffanalytics/R/custom_scoring.R")
source("../ffanalytics/R/scoring_rules.R")
source("../ffanalytics/R/make_scoring.R")
source("../ffanalytics/R/recode_vars.R")
source("../ffanalytics/R/impute_funcs.R")
source("./FantasyFootball/score_settings.R")

prev <- source_points(scrap, dudes.score.settings)

points_simulation <- prev %>% 
  select(-pos) %>% 
  group_by(id) %>% 
  nest(.key="point.range") 


hometeam %>% 
  inner_join(player_ids) %>% 
  select( id, name, team, pos, rosterSlot, points ) %>% 
  inner_join(points_simulation) %>%
  as.tibble() %>% 
  filter( rosterSlot != "BN") -> teamSimulation

simPoints <- function(team_table){
  team_table$point.range %>% 
    map(function(pt.rng){
      i <- sample(1:nrow(pt.rng),1)
      return(pt.rng[i,]$points)
    }) %>% 
    unlist() %>% 
    sum(na.rm = T) %>% 
    return()
}

ff.sim.away <- vector("numeric",1000)
for(i in 1:1000) ff.sim.away[i] <- simPoints(teamSimulation)
hist(ff.sim.away, breaks = 50)

tibble(
  home = ff.sim.home,
  away = ff.sim.away
) %>% 
  mutate(
    home.win = home > away
  ) -> games

games$home.win %>% mean()

ff.sim <- tibble(
  team = c(rep("amparo bikers",1000), rep("campinas giants",1000)),
  points = c(ff.sim.home, ff.sim.away)
)

ggplot(ff.sim, aes(x=points, group=team)) +
  geom_histogram(aes(home), fill="red", alpha=.5, show.legend = T) +
  geom_histogram(aes(away), fill="blue", alpha=.5, show.legend = T) +
  theme_light()

ggplot(ff.sim, aes(x=points, color=team, fill=team, group=team)) +
  geom_density(alpha=.5) +
  theme_light()
