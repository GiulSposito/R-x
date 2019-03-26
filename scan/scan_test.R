scan("./scan/data.txt",
     what=list(character(), character(), integer(), integer(), integer()),
     multi.line = T) -> scan.lst

scan.lst %>% 
  str()

scan.lst %>% 
  map(tibble) %>% 
  bind_cols() %>% 
  set_names(c("subject","class","val1","val2","val3"))
