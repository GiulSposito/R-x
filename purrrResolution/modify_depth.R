# script to test "modify_depth" in purr package
library(purrr)

# create some hierarchical data

sites <- c("01","02","03")

hd <- list(
  "2016" = list (
    Winter = list(
      Site = sites,
      Temp = c(15,17,20)
    ),
    Summer = list(
      Site = sites,
      Temp = c(14, 16, 19)
    )
  ), 
  "2017" = list(
    Winter = list(
      Site = sites,
      Temp = c(16,18,21)
    ),
    Summer = list(
      Site = sites,
      Temp = c(17,19,20)
    )
  )
)

str(hd)

# install.packages("data.tree")

# just to visualize
library(data.tree)
FromListSimple(hd)

# modify apply a FUN to the 2nd level (seasson) of the list
hd.dt <- modify_depth(hd, 2, as.data.frame)
hd.dt
