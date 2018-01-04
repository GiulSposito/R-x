suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(ruler))

# PROLOGO ########################

# a "rule" by column
mtcars %>% summarise_all(funs(enough_sum = sum(.) > 100))

# explicit the rule
mtcars %>% summarise_all(rules(enough_sum = sum(.) > 100))

# validation function
val_fun <- . %>% summarise_all(rules(enough_sum = sum(.) > 100))

# wrap the rule
cp <- col_packs(
  is_enough_sum = . %>% summarise_all(rules(is_enough = sum(.) > 100))
)

rexp <- expose(.tbl = mtcars, cp)

get_exposure(rexp)
get_packs_info(rexp)
get_report(rexp)

#### STAR WARS CASE ###########

# Does starwars have:
#  1) number of rows
#    1a) more than 50;
#    1b) less than 60; 
#  2) number of columns
#    2a) more than 10;
#    2b) less than 15?

check_data_dims <- data_packs(
  check_dims = . %>% summarise(
    nrow_low = nrow(.) >= 50, nrow_up = nrow(.) <= 60,
    ncol_low = ncol(.) >= 10, ncol_up = ncol(.) <= 15
  )
)

starwars %>%
  expose(check_data_dims, .remove_obeyers = F) %>%
  get_exposure()


# Does starwars have enough rows for characters
#   1) with blond hair; 
#   2) humans;
#   3) humans with blond hair?

check_enough_rows <- data_packs(
  
  # role (1)
  enough_blond = . %>% 
    filter(hair_color == "blond") %>%
    summarise(is_enough = n() > 10),
  
  # role (2)
  enough_humans = . %>% 
    summarise(is_enough = sum(species == "Human", na.rm = TRUE) > 30),
  
  # role (2)
  ehough_blond_humans = . %>%
    filter(hair_color == "blond", species == "Human") %>%
    summarise(is_enough = n() > 5)

)

starwars %>%
  expose(check_enough_rows, .remove_obeyers = F) %>%
  get_exposure()

# Does starwars have enough numeric columns?

check_enough_num_cols <- data_packs(
  enough_num_cols = . %>% summarise(
    is_enough = sum(map_lgl(., is.numeric)) > 1
  )
)

starwars %>%
  expose(check_enough_num_cols, .remove_obeyers = F) %>%
  get_report()


# Does group defined by hair color and gender have a member from Tatooine

has_hair_gender_tatooine <- group_packs(
  hair_gender_tatooine = . %>% 
    group_by( hair_color, gender ) %>%
    summarise( has_tatooine = any(homeworld=="Tattoine")),
  .group_vars = c("hair_color","gender"),
  .group_sep = "__"
)

starwars %>%
  expose(has_hair_gender_tatooine, .remove_obeyers = F) %>%
  get_report()


# Does every list-column have
#   1) enough average length;
#   2) enough unique elements?

check_list_cols <- col_packs(
  check_list_cols = . %>%
    summarise_if(
      is.list,
      rules(
        is_enough_mean = mean(map_int(., length)) >= 1,
        length(unique(unlist(.))) >= 10
      )
    )
)

starwars %>%
  expose(check_list_cols) %>%
  get_report()

# Are all values of column birth_year non-NA?

starwars %>%
  expose(
    col_packs(
      . %>% summarise_at(
        vars(birth_year = "birth_year"),
        rules(all_present = all(!is.na(.)))
      )
    )
  ) %>%
  get_report()

