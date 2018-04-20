.parseRealValue <- function(x) parse_number(
  gsub(pattern = "R$ *", replacement = "", x = x), 
  locale=locale(grouping_mark=".", decimal_mark=",")
)

# parse num with locale ptBR
.parseNumPtBr <- function(x) parse_number(x, locale=locale(grouping_mark=".", decimal_mark=","))
