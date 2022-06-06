# 3. RANDOM FOREST 
# create spec for rf
rf_model <- rand_forest() %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "impurity") #use Gini index 
# create workflow for rf
rf_wkflow <- workflow() %>%
  add_model(rf_model %>%
              set_args(mtry=tune(), trees=tune(),
              min_n = tune())) %>%
  add_recipe(movies_recipe)
# grid for different numbers of predictors 
rf_param_grid <- grid_regular(
  mtry(range=c(1,7)),
  trees(range=c(10,50)),
  min_n(range=c(1,100)),  levels=10)
rf_param_res <- tune_grid(rf_wkflow,
                          resamples = movies_fold,
                          grid = rf_param_grid)
autoplot(rf_param_res)
# model does best with all predictors 

best_predictors <- select_best(rf_param_res,
                               metric = "rmse")
rf_final <- finalize_workflow(
  rf_wkflow, best_predictors)
rf_final_fit <- fit(
  rf_final,
  data = movies_train)
# check performance
augment(rf_final_fit,
        new_data = movies_train) %>%
  rmse(truth = score, estimate = .pred)
# get value of 0.758, worst than what we had 

# plot the true versus the predicted values
augment(rf_final_fit,
        new_data = movies_train) %>%
  ggplot(aes(score, .pred)) +
  geom_abline() +
  geom_point(alpha = 0.5)
# how to save this value
save(rf_param_res, rf_wkflow, file = "random_forest_runs.rda")

