library(tidyverse)
library(yaml)
library(httr)
library(jsonlite)
library(glue)

# lendo liga e token do yaml (para não versionar o access token)
config <- yaml.load_file("./FantasyFootball/config.yml")
leagueId <- config$leagueId
authToken <- config$authToken

# contexto da semana
week <- 6

# obtem os matchups
url.matchups <- "http://api.fantasy.nfl.com/v1/league/matchups?leagueId={leagueId}&week={week}&format=json&authToken={authToken}"

# faz a chamada na api
httr::GET(glue(url.matchups)) -> resp

resp %>% 
  httr::content(as="text") %>%
  fromJSON(simplifyDataFrame = T) -> matchups.json

# processa o json
matchups <- matchups.json$leagues$matchups[[1]] %>% 
  jsonlite::flatten() %>% 
  as.tibble()

# para cada um dos times, faz a chamada para pegar os times escalados no matchup
url.team.matchup <- "http://api.fantasy.nfl.com/v1/league/team/matchup?leagueId={leagueId}&teamId={teamId}&week={week}&authToken={authToken}&format=json"

matchup.teams.json <- matchups$awayTeam.id %>% 
  map(function(teamId, .url){
    httr::GET(glue(.url)) %>% 
      httr::content(as = "text") %>% 
      jsonlite::fromJSON(simplifyDataFrame = T) %>% 
      return()
  },
  .url=url.team.matchup)

saveRDS(matchup.teams.json, "./FantasyFootball/week6_matchups_json.rds")

# funcao para extrar dados dos hosters dos times
extractTeam <- . %>% 
  .$players %>% 
  .[[1]] %>% 
  select( src_id=id, name, position, rosterSlot, fantasyPts ) %>%
  jsonlite::flatten() %>% 
  select(-fantasyPts.week.season, -fantasyPts.week.week ) %>% 
  rename(points = fantasyPts.week.pts)

# precessa o json de retorno
matchups.rosters <- matchup.teams.json %>% 
  map(function(.json){
    matchup <- .json$leagues$matchup
    tibble(
      home.teamId = matchup$homeTeam$id,
      home.name   = matchup$homeTeam$name,
      nome.logo   = matchup$homeTeam$logoUrl,
      home.roster = list(extractTeam(matchup$homeTeam)),
      away.teamId = matchup$awayTeam$id,
      away.name   = matchup$awayTeam$name,
      away.logo   = matchup$awayTeam$logoUrl,
      away.roster = list(extractTeam(matchup$awayTeam))
    ) %>% 
      return()
  }) %>% bind_rows()


### scrap e pontuacao

library(ffanalytics)
source("FantasyFootball/score_settings.R")

# scrap todas as fontes, nas posicoes da liga
scrap <- scrape_data(pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                     season = 2018, 
                     week = 5)

saveRDS(scrap, "./FantasyFootball/week6_scrap.rds")

# extrai o mapeamento ID do Fantasy versus ID da projecao
scrap %>%
  map(function(dft){
    dft %>% 
      filter(data_src=="NFL") %>% 
      select(id, src_id, player, team, pos) %>% 
      return()
  }) %>% 
  bind_rows() %>%
  distinct() -> player_ids

# funcao que aproveita o pacote ffanalytics para fazer a projecao de pontos por jogador
playerPointsProjections <- function(.scrap, .score.settings){
  source("../ffanalytics/R/calc_projections.R")
  source("../ffanalytics/R/stats_aggregation.R")
  source("../ffanalytics/R/source_classes.R")
  source("../ffanalytics/R/custom_scoring.R")
  source("../ffanalytics/R/scoring_rules.R")
  source("../ffanalytics/R/make_scoring.R")
  source("../ffanalytics/R/recode_vars.R")
  source("../ffanalytics/R/impute_funcs.R")
  source("./FantasyFootball/score_settings.R")
  source_points(.scrap, .score.settings) 
}

# projecao de pontos
# remove os kickers porque a projecao de K nao funciona
players.points <- playerPointsProjections(scrap, dudes.score.settings) %>% 
  filter( pos!="K")

# pega a pontuacao dos kickers do scrap
# e junta com a projecao
scrap$K %>%
  mutate(pos="K") %>% 
  select(pos, data_src, id, points=site_pts) %>% 
  bind_rows(players.points) %>% 
  group_by(id, pos) %>% 
  nest(.key="points.range") -> players.points.projections


addProjPoints <- function(.roster, .id_map, .pts_proj){
  .id_map %>%
    select(id, src_id, team ) %>% 
    inner_join(.roster, by="src_id") %>% 
    inner_join(.pts_proj, by = "id") %>% 
    select(-pos,-src_id)
}

# coloca os pontos de projecao junto aos times dos rosters
matchups.rosters.proj <- matchups.rosters %>% 
  mutate( 
    home.roster = map(
      home.roster,
      addProjPoints,
      .id_map   = player_ids,
      .pts_proj = players.points.projections),
    away.roster = map(
      away.roster,
      addProjPoints,
      .id_map   = player_ids,
      .pts_proj = players.points.projections)
    )

# sorteia uma das pontuacoes projetadas
simPoints <- function(team_table){
  team_table %>% 
    filter( rosterSlot != "BN" ) %>% 
    pull(points.range) %>% 
    map(function(pt.rng){
      i <- sample(1:nrow(pt.rng),1)
      return(pt.rng[i,]$points)
    }) %>% 
    unlist() %>% 
    sum(na.rm = T) %>% 
    return()
}

# repete o sorteio .n vezes
repSimulation <- function(.team, .n){
  sims <- vector("numeric",.n)
  for(i in 1:.n) sims[i] <- simPoints(.team)
  return(sims)
}

# numero de simulacoes
n.sim <- 2000

# retorna um summary como um data.frame
summaryAsTibble <- . %>% summary() %>% as.list() %>% as.tibble()

# incorpora a simulacao e calcula resultados
matchups.rosters.proj %>% 
  mutate(
    home.sim = map(home.roster, repSimulation, .n=n.sim),
    away.sim = map(away.roster, repSimulation, .n=n.sim),
    home.win = map2(home.sim, away.sim, function(h.scr, a.scr) (h.scr > a.scr)),
    away.win = map(home.win, function(.x) !.x),
    home.win.prob = map(home.win, function(.x) mean(.x) ),
    away.win.prob = map(away.win, function(.x) mean(.x)),
    score.diff = map2(home.sim, away.sim, function(h.scr, a.scr) (h.scr - a.scr))
  )  %>%  
  mutate(
    home.points = map(home.sim, summaryAsTibble),
    away.points = map(away.sim, summaryAsTibble)
  ) -> matchups.simulation

# salva dados da simulacao
saveRDS(matchups.simulation, "./FantasyFootball/week6_simulation.rds")
