.results <- readTable("results")


.results %>%
  mutate(
    home.win    = as.integer(home.score > away.score),
    away.win    = as.integer(home.score < away.score),
    match.draw  = as.integer(home.score == away.score),
    home.net.score = home.score - away.score,
    away.net.score = -home.net.score
  ) -> .results2

.results2 %>%
  select( date       = match.date, 
          team       = home.team.cod, 
          rank       = home.rank, 
          rating     = home.rating,
          score.pro  = home.score,
          score.agst = away.score,
          win        = home.win,
          draw       = match.draw,
          defeat     = away.win,
          delta.rank = home.deltaRank,
          delta.rating = home.deltaRating,
          atHome     = home.atHome ) -> home.teams

.results2 %>%
  mutate( atHome     = as.integer(away.team.cod==location) ) %>%
  select( date       = match.date, 
          team       = away.team.cod, 
          rank       = away.rank, 
          rating     = away.rating,
          score.pro  = away.score,
          score.agst = home.score,
          win        = away.win,
          draw       = match.draw,
          defeat     = home.win,
          delta.rank = away.deltaRank,
          delta.rating = away.deltaRating,
          atHome ) -> away.teams

teams.stats <- bind_rows(home.teams, away.teams)

teams.stats %>%
  group_by(team) %>%
  arrange(team, date) %>%
  mutate(
    # saldos de goals
    goals.pro.L1     = as.integer(    lag(score.pro,    k=2, default=0)),
    goals.pro.L2     = as.integer(rollsum(goals.pro.L1, k=2, na.pad=T, fill=0, align = "right")),
    goals.pro.L3     = as.integer(rollsum(goals.pro.L1, k=3, na.pad=T, fill=0, align = "right")),
    goals.pro.L5     = as.integer(rollsum(goals.pro.L1, k=5, na.pad=T, fill=0, align = "right")),
    goals.pro.L10    = as.integer(rollsum(goals.pro.L1, k=10, na.pad=T, fill=0, align = "right")),
    goals.pro.L20    = as.integer(rollsum(goals.pro.L1, k=20, na.pad=T, fill=0, align = "right")),
    goals.agst.L1    = as.integer(    lag(score.agst,    k=2, default=0)),
    goals.agst.L2    = as.integer(rollsum(goals.agst.L1, k=2, na.pad=T, fill=0, align = "right")),
    goals.agst.L3    = as.integer(rollsum(goals.agst.L1, k=3, na.pad=T, fill=0, align = "right")),
    goals.agst.L5    = as.integer(rollsum(goals.agst.L1, k=5, na.pad=T, fill=0, align = "right")),
    goals.agst.L10   = as.integer(rollsum(goals.agst.L1, k=10, na.pad=T, fill=0, align = "right")),
    goals.agst.L20   = as.integer(rollsum(goals.agst.L1, k=20, na.pad=T, fill=0, align = "right"))
  ) %>%
  mutate(
    win.L1     = as.integer(    lag(win,    k=2,  default=0)),
    wins.L2    = as.integer(rollsum(win.L1, k=2,  na.pad=T, fill=0, align = "right")),
    wins.L3    = as.integer(rollsum(win.L1, k=3,  na.pad=T, fill=0, align = "right")),
    wins.L5    = as.integer(rollsum(win.L1, k=5,  na.pad=T, fill=0, align = "right")),
    wins.L10   = as.integer(rollsum(win.L1, k=10, na.pad=T, fill=0, align = "right")),
    wins.L20   = as.integer(rollsum(win.L1, k=20, na.pad=T, fill=0, align = "right")),
    draw.L1    = as.integer(    lag(draw,    k=2,  default=0)),
    draws.L2   = as.integer(rollsum(draw.L1, k=2,  na.pad=T, fill=0, align = "right")),
    draws.L3   = as.integer(rollsum(draw.L1, k=3,  na.pad=T, fill=0, align = "right")),
    draws.L5   = as.integer(rollsum(draw.L1, k=5,  na.pad=T, fill=0, align = "right")),
    draws.L10  = as.integer(rollsum(draw.L1, k=10, na.pad=T, fill=0, align = "right")),
    draws.L20  = as.integer(rollsum(draw.L1, k=20, na.pad=T, fill=0, align = "right")),
    defeat.L1    = as.integer(    lag(defeat,    k=2,  default=0)),
    defeats.L2   = as.integer(rollsum(defeat.L1, k=2,  na.pad=T, fill=0, align = "right")),
    defeats.L3   = as.integer(rollsum(defeat.L1, k=3,  na.pad=T, fill=0, align = "right")),
    defeats.L5   = as.integer(rollsum(defeat.L1, k=5,  na.pad=T, fill=0, align = "right")),
    defeats.L10  = as.integer(rollsum(defeat.L1, k=10, na.pad=T, fill=0, align = "right")),
    defeats.L20  = as.integer(rollsum(defeat.L1, k=20, na.pad=T, fill=0, align = "right"))
  ) %>%
  filter(team %in% c("BR","GR")) %>%
  View()

