# BOOSTED TREE 
# create spec
boost_model <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# create workflow 
boost_wkflow <- workflow() %>%
  add_model(boost_model %>%
              set_args(trees = tune(), 
                       mtry = tune())) %>%
  add_recipe(movies_recipe)

# grid for different values
boost_param_grid <- grid_regular(
  mtry(range=c(1,7)),
  trees(range=c(1,500)),  levels=10)
# results for different models

boost_param_res <- tune_grid(rf_wkflow,
                          resamples = movies_fold,
                          grid = rf_param_grid)
# visual
autoplot(boost_param_res)
# select best model 
best_boost_res <- select_best(boost_param_res,
                               metric = "rmse")
boost_final <- finalize_workflow(
  boost_wkflow, best_boost_res)
boost_final_fit <- fit(
  boost_final,
  data = movies_train)
# check performance
augment(boost_final_fit,
        new_data = movies_train) %>%
  rmse(truth = score, estimate = .pred)
# plot the true versus the predicted values
augment(boost_final_fit,
        new_data = movies_train) %>%
  ggplot(aes(score, .pred)) +
  geom_abline() +
  geom_point(alpha = 0.5)
save(boost_param_res, boost_wkflow, file = "boosted_runs.rda")
load(file = "boosted_runs.rda")
