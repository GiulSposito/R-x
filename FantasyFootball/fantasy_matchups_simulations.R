library(yaml)
library(httr)
library(jsonlite)

# lendo liga e token do yaml (para não versionar o access token)
config <- yaml.load_file("./FantasyFootball/config.yml")
leagueId <- config$leagueId
authToken <- config$authToken

# contexto da semana
week <- 6

# obtem os matchups
url.matchups <- "http://api.fantasy.nfl.com/v1/league/matchups?leagueId={leagueId}&week={week}&authToken={authToken}&format=json"

# faz a chamada na api
matchups.json <- httr::GET(glue(url.matchups)) %>% 
  httr::content(resp, as="text") %>%
  fromJSON(simplifyDataFrame = T)

# processa o json
matchups <- matchups.json$leagues$matchups[[1]] %>% 
  jsonlite::flatten() %>% 
  as.tibble()

# para cada um dos times, faz a chamada para pegar os times escalados no matchup
url.team.matchup <- "http://api.fantasy.nfl.com/v1/league/team/matchup?leagueId={leagueId}&teamId={teamId}&week={week}&authToken={authToken}&format=json"

matchup.teams.json <- matchups$awayTeam.id %>% 
  map(function(teamId, .url){
    httr::GET(glue(.url)) %>% 
      httr::content(resp, as = "text") %>% 
      jsonlite::fromJSON(simplifyDataFrame = T) %>% 
      return()
  },
  .url=url.team.matchup)

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
                     week = 6)

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

