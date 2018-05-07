# scripts para geracao de fetaures

.tournaments <- .tables$tournaments

genTournamentFeatures <- function(.tournaments){
  
  # procura por 
  .tables$tournaments %>%
    mutate( tournament.name = tolower(tournament.name) ) %>%
    filter( grepl(".*qualif.*", tournament.name) ) %>%
    filter( grepl(".*world *cup.*", tournament.name) ) %>%
    select(tournament.cod) %>% 
    unlist() -> wc.qualify
  
  .tournaments %>%
    mutate( tournament.importance = case_when(
      tournament.cod ==  "WC"               ~ 5, # world cup
      tournament.cod %in% wc.qualify        ~ 4, # eliminatorias da copa
      tournament.cod %in% c("CA","EC","CC") ~ 3, # copa america e euro copa
      tournament.cod %in% c("OG","AR",
                            "AC","CGC")     ~ 2, # africa nations cup | asian cup | olimpic games | concacaf golden cup
      tournament.cod %in% c("F","FT")       ~ 1, # amistosos
      TRUE                                  ~ 0  # qualquer outro
    ) %>% as.integer() ) %>%
    return()
  
}


  