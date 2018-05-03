# scripts to install necessary packaghes on remote instance

setupLibrariesOnRemote <- function(X){

  require(tidyverse)
  installed.packages() %>%
    as.tibble() %>%
    return()
  
}

