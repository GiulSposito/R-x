# exemplo 1

weight.gains <- c(134, 146, 104, 119, 124, 161, 107, 83, 
                    113, 129, 97, 123, 70, 118, 101, 85, 107, 132, 94)

diet <- as.factor(c(rep(1,12), rep(0,7)))

rats <- data.frame(weight.gains = weight.gains, diet = diet)


library(BayesVarSel)

h0 <- weight.gains ~ 1
h1 <- weight.gains ~ diet


# B Factors | Probability  | Evidence against the null
# 1 to 3    | 0.5 to 0.75  | Not worth more than a bare mention
# 3 to 20   | 0.75 to 0.95 | Substantial
# 20 to 150 | 0.95 to 0.99 | Strong
# >150      | > 0.99       | Decisive

Btest(models = c(H0=h0, H1=h1), data=rats)

# exemplo 2

data("savings", package = "faraway")
str(savings)

# This dataset contains macroeconomic data on 50 different countries during 1960-1970
# and the question posed is to elucidate if dpi (per-capita disposable income in U.S), ddpi
# (percent rate of change in per capita disposable income), population under (over) 15 (75)
# pop15 (pop75) are all explanatory variables for sr, the aggregate personal saving divided by
# disposable income which is assumed to follow a normal distribution. 

# This can be written as a testing problem about the regression coefficients associated
# with the variables with hypotheses
# 
#     H0 : βdpi = βddpi = βpop15 = βpop75 = 0,
# 
# versus the alternative, say H1, that all predictors are needed. The competing models can be
# defined as

fullmodel <- sr ~ pop15 + pop75 + dpi + ddpi
nullmodel <- sr ~ 1

Btest(models=c(H0=nullmodel, H1=fullmodel), savings)

reducemodel <- sr ~ pop75 + dpi + ddpi

Btest(models=c(H0=nullmodel, H1=fullmodel, H2=reducemodel), savings)

# var selection

Bvs(formula = sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

closemodel <- sr ~ pop15 + pop75 + ddpi
Btest(models = c(H0=nullmodel, H1=closemodel, H2=fullmodel), savings)

