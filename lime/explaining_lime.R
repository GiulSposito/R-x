# script reproducing a Shirin's post in https://shiring.github.io/machine_learning/2017/04/23/lime

#  Intuition behind LIME

# Because we want to be model-agnostic, what we can do to learn the behavior of the underlying model
# is to perturb the input and see how the predictions change. This turns out to be a benefit in terms
# of interpretability, because we can perturb the input by changing components that make sense
# to humans (e.g., words or parts of an image), even if the model is using much more complicated
# components as features (e.g., word embeddings).
# 
# We generate an explanation by approximating the underlying model by an interpretable
# one (such as a linear model with only a few non-zero coefficients), learned on perturbations
# of the original instance (e.g., removing words or hiding parts of the image). The key intuition
# behind LIME is that it is much easier to approximate a black-box model by a simple model
# locally (in the neighborhood of the prediction we want to explain), as opposed to trying
# to approximate a model globally. This is done by weighting the perturbed images by their
# similarity to the instance we want to explain. Going back to our example of a flu prediction,
# the three highlighted symptoms may be a faithful approximation of the black-box model for
# patients who look like the one being inspected, but they probably do not represent how the
# model behaves for all patients.

library(tidyverse)
library(OneR)

# data from: https://www.kaggle.com/unsdsn/world-happiness
hpn <- read_csv("./lime/2017.csv") %>% 
  select(-Country, -Happiness.Rank) %>% 
  mutate( Happiness.Score = bin(Happiness.Score, nbins=3, method="content") )

# transform the happiness score in to "ranges"
hapLevels <- hpn$Happiness.Score %>% levels %>% set_names(c("low","med","high"),.)
hpn <- hpn %>%  mutate( Happiness.Score = plyr::revalue(Happiness.Score, hapLevels) )
glimpse(hpn)


library(doParallel)
cl <- makeCluster(detectCores()-1) # free one core
registerDoParallel(cl)

library(caret)
set.seed(42) # the life, the universe and everthing else

index <- createDataPartition(hpn$Happiness.Score, p=.7, list=F)
train_data <- hpn[index,]
test_data  <- hpn[-index, ]

# training an ANN
model_mlp <- train(Happiness.Score ~ .,
                   data = train_data, 
                   method = "mlp", 
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10, 
                                            repeats = 5, 
                                            verboseIter = F))

library(lime)

explainer <- lime(train_data, model_mlp, bin_continuous = T, n_bins = 5, n_permutations=1000)

data.frame(
    sample_id  = 1:nrow(test_data),
    predict(model_mlp, test_data[,-1], type="prob"),
    actual     =  test_data$Happiness.Score
  ) %>%
  mutate(
    prev = factor(colnames(pred)[2:4][apply(pred[,2:4],1, which.max)], levels(actual)),
    correct = ifelse(actual==prev, "correct","wrong")
  ) -> pred

confusionMatrix(pred$actual, pred$prev)

pred_cor <- pred %>% filter(correct=="correct")
pred_wrong <- pred %>% filter(correct=="wrong")

test_data_cor <- test_data %>% 
  mutate( sample_id = 1:nrow(test_data) ) %>% 
  filter( sample_id %in% pred_cor$sample_id ) %>% 
  sample_n(size=3) %>% 
  remove_rownames() %>% 
  column_to_rownames(var="sample_id") %>% 
  select(-Happiness.Score)

test_data_wrong <- test_data %>% 
  mutate( sample_id = 1:nrow(test_data) ) %>% 
  filter( sample_id %in% pred_wrong$sample_id ) %>% 
  sample_n(size=3) %>% 
  remove_rownames() %>% 
  column_to_rownames(var="sample_id") %>% 
  select(-Happiness.Score)

explanier      <- lime(train_data, model_mlp)
explantion_cor <- explain(train_data, explainer, n_labels=3, n_features=5)
