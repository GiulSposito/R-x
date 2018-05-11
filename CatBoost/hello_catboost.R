library(catboost)

# doad dataset
pool_path <- system.file("extdata", 
                         "adult_train.1000", 
                         package = "catboost")
cd_path <- system.file("extdata", 
                       "adult.cd",
                       package = "catboost")
pool <- catboost.load_pool(pool_path, column_description = cd_path)


# training the model
fit_params <- list(iterations = 100, 
                   thread_count = 10, 
                   loss_function = 'Logloss')
model <- catboost.train(pool, pool, fit_params)


prediction <- catboost.predict(model, pool)
head(prediction)
  