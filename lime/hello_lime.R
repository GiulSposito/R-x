library(caret)
library(lime)
library(tidyverse)
library(doParallel)

# dataset
iris %>% glimpse()

# partitions
index <- createDataPartition(iris$Species, p=.7, list = F)
iris.train <- iris[index,]
iris.test  <- iris[-index,]

# multicore 
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

# train
model <- train(Species ~ ., data=iris.train, method = "rf")

# predict
pred <- predict(model, iris.test[,-5])

# eval
confusionMatrix(iris.test$Species, pred)

# create an explainer object
explainer <- lime(iris.train, model)

# explain new observations
explanation <- lime::explain(sample_n(iris.test[,-5], 5), explainer, n_labels=1, n_features=3)

explanation %>% glimpse()
plot_features(explanation)
