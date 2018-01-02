library(dplyr)
library(tidyr)
library(ggplot2)
library(ruler)


# Z-score
# Z-score, also called a standard score, of an observation is [broadly speaking] a distance from the population center measured in number of normalization units. The default choice for center is sample mean and for normalization unit is standard deviation.

# check if a value is 3sd out
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

# Z-score with MAD
# Median Absolute Deviation is a robust normalization unit based on median as a population center. In order to use MAD "as a consistent estimator for the estimation of the standard deviation" one takes its value multiplied by a factor. This way base R function mad is implemented.

# check if z-score out with MAD 
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

# Tukey's fences
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}

# lets test these functions
isnt_out_funs <- funs(
  z = isnt_out_z,
  mad = isnt_out_mad,
  tukey = isnt_out_tukey
)


# diamonds dataset
diamonds %>%
  mutate_if(is.numeric, isnt_out_funs) %>%
  View()

  
# Mahalanobis distance
# To detect outliers in multivariate case one can use Mahalanobis distance to reduce to univariate case and then apply known techniques.

maha_dist <- . %>% select_if(is.numeric) %>%
  mahalanobis(center = colMeans(.), cov = cov(.))

# let's see
diamonds %>%
  mutate(maha = maha_dist(.)) %>%
  mutate_at(vars(maha = maha), isnt_out_funs) %>%
  View()


# lets define score functions over maha dist
isnt_out_maha <- function(tbl, isnt_out_f, ...) {
  tbl %>% maha_dist() %>% isnt_out_f(...)
}


# teste
diamonds %>% 
  unite(col="group", cut, color, clarity) %>%
  View()


# final implementation #######

# lets "group" nom numerical columns
data_tbl <- diamonds %>%
  unite(col="group", cut, color, clarity)

# to compute the outliers by group
compute_group_non_outliers <- . %>%
  # Compute per group mean values of columns
  group_by(group) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  # Detect outliers among groups
  mutate_if(is.numeric, isnt_out_funs) %>%
  # Remove unnecessary columns
  select_if(Negate(is.numeric))

# The result has outputs for 21 methods applied to the 276 groups. Their names are of the form <column name for summary function>_<method name>. So the name 'carat_z' is interpreted as result of method 'z' for summary function equal to mean value of 'carat' column. Column group defines names of the groupings.
data_tbl %>% compute_group_non_outliers() %>%
  str()


# exposure

# Column and Mahalanobis based definition of non-outlier 
# rows can be expressed with row packs and group based - as group packs.

# row_packs is a rule which converts data to logical value

row_packs_isnt_out <- row_packs(
  # Non-outliers based on some column
  column = . %>% transmute_if(is.numeric, isnt_out_funs),
  # Non-outliers based on Mahalanobis distance
  maha = . %>% transmute(maha = maha_dist(.)) %>%
    transmute_at(vars(maha = maha), isnt_out_funs)
)

group_packs_isnt_out <- group_packs(
  # Non-outliers based on grouping
  group = compute_group_non_outliers,
  .group_vars = "group"
)

full_report <- data_tbl %>%
  expose(row_packs_isnt_out, group_packs_isnt_out, 
         .remove_obeyers = F) %>%
  get_report()

# used_rules contains data about all definitions of non-outlier rows applied to data. They are encoded with combination of columns pack and rule.
used_rules <- full_report %>%
  distinct(pack, rule)

# breaker_report contains data about data units that break certain rules. Packs column and maha has actual row numbers of data_tbl listed in id column of report (for rows which should be considered as outliers).
breaker_report <- full_report %>%
  filter(!(value %in% TRUE))


group_breakers <- breaker_report %>%
  # Filter group packs
  filter(pack == "group") %>%
  # Expand rows by matching group with its rows
  select(-id) %>%
  left_join(
    y = data_tbl %>% transmute(var = group, id = 1:n()),
    by = "var"
  ) %>%
  select(pack, rule, var, id, value)

outliers <- bind_rows(
  breaker_report %>% filter(pack != "group"),
  group_breakers
) %>%
  select(pack, rule, id)

# Not all group based definitions resulted with outliers
outliers %>%
  count(pack, rule) %>%
  filter(pack == "group") %>%
  print(n = Inf)

# Tibble outliers contains data about outlier rows. Combination of columns pack and rule defines non-outlier/outlier definition approach and column id defines row number of input data frame that should be considered an outlier based on the definition.
outliers %>%
  count(pack, rule, sort = TRUE)

#Combined outlier detection score for certain row can be defined as share of applied methods that tagged it as outlier
outlier_score <- outliers %>%
  group_by(id) %>%
  # nrow(used_rules) equals total number of applied methods
  summarise(score = n() / nrow(used_rules))

# Top 10 outliers
outlier_score %>% arrange(desc(score)) %>% slice(1:10)


# Finally we will tag those rows as strong outliers which has score more than 0.2 (subjective threshold which should be researched more).
diam_tbl <- diamonds %>%
  mutate(id = 1:n()) %>%
  left_join(y = outlier_score, by = "id") %>%
  mutate(
    score = coalesce(score, 0),
    is_out = if_else(score > 0.2, "Outlier", "Not outlier")
  )

# Total number of outliers
sum(diam_tbl$score > 0.2)


# plot the outliers
theme_set(theme_bw())

plot_outliers <- function(tbl, x, y, facet_var) {
  tbl %>%
    arrange(is_out) %>%
    ggplot(aes_string(x, y, colour = "is_out")) +
    geom_point() +
    facet_wrap(facets = facet_var) +
    scale_colour_manual(values = c("#AAAAAA", "#004080")) +
    guides(colour = guide_legend(title = NULL,
                                 override.aes = list(size = 4))) +
    labs(title = paste0("Strong outliers illustration by ", facet_var)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14))
}

diam_tbl %>% plot_outliers("carat", "price", facet_var = "cut")
diam_tbl %>% plot_outliers("x", "depth", facet_var = "color")
diam_tbl %>% plot_outliers("price", "table", facet_var = "clarity")


breaker_report %>%
  filter(pack == "group") %>%
  count(var, sort = TRUE) %>%
  print(n = 10)
