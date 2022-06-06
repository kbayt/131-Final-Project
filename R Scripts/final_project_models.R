library(rpart.plot)
library(xgboost)
library(ranger)
library(randomForest)
library(vip)



## MODELSSS
view(movies1)
# split the data
movies_split <- initial_split(movies1, prop=0.8,
                             strata = score)
movies_train <- training(movies_split)
movies_test <- testing(movies_split)
dim(movies_train)
dim(movies_test)
# create folds
movies_fold <- vfold_cv(movies_train, v=5)

# RECIPE / No Interactions
movies_recipe <- recipe(score~ runtime + votes + genre + 
                          company + star + director + writer,
                        data = movies_train) %>%
  step_other(star, threshold = 25) %>%
  step_other(company, threshold = 40) %>%
  step_other(director, threshold = 15) %>%
  step_other(writer, threshold = 5) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_predictors())

# 1. LINEAR REGRESSION
# model engine
lm_model <- linear_reg() %>%
  set_engine("lm")
# workflow
lm_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(movies_recipe)
# fit model
lm_fit <- fit(lm_wflow, movies_train)
lm_fit %>%
  extract_fit_parsnip() %>%
  tidy()
# show pred with actual
movies_train_lm_res <- predict(lm_fit, 
                               new_data = movies_train %>%
                                 select(-score))
movies_train_lm_res <- bind_cols(movies_train_lm_res,
                                 movies_train %>%
                                   select(score)) 
movies_train_lm_res %>% head()
# plot of pred vs actual
movies_train_lm_res %>%
  ggplot(aes(x=.pred, y=score)) +
  geom_point(alpha=0.2) +
  geom_abline(lty = 2) +
  theme_bw() +
  coord_obs_pred()
## LM DOES NOT PREDICT WELL!!!

# 2. RIDGE REGRESSION (or lasso)
ridge_spec <- linear_reg(mixture=0,
                         penalty=tune()) %>%
  set_mode("regression") %>%
  set_engine("glmnet")
# workflow
ridge_wflow <- workflow() %>%
  add_model(ridge_spec) %>%
  add_recipe(movies_recipe)
# penalty grid
penalty_grid <- grid_regular(penalty(range=c(-5,5)),
                             levels = 50)
penalty_grid
# fit model
tune_res <- tune_grid(
  ridge_wflow,
  resample = movies_fold,
  grid = penalty_grid
)
# select best penalty
collect_metrics(tune_res)
best_penalty <- select_best(tune_res, metric = "rsq")
best_penalty
# finalize workflow
ridge_final <- finalize_workflow(ridge_wflow,
                                 best_penalty)
ridge_final_fit <- fit(ridge_final, data=movies_train)
# assess performance
augment(ridge_final_fit, new_data = movies_test) %>%
  rsq(truth = score, estimate = .pred)
# show pred with actual
movies_train_ridge_res <- predict(ridge_final_fit, 
                               new_data = movies_train %>%
                                 select(-score))
movies_train_ridge_res <- bind_cols(movies_train_ridge_res,
                                 movies_train %>%
                                   select(score)) 
movies_train_ridge_res %>% head()
# plot of pred vs actual
movies_train_ridge_res %>%
  ggplot(aes(x=.pred, y=score)) +
  geom_point(alpha=0.2) +
  geom_abline(lty = 2) +
  theme_bw() +
  coord_obs_pred()

# 3. PRUNED TREE (no tuning)
# create spec for pruned tree
pruned_tree_model <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")
# create workflow
pruned_tree_wkflow <- workflow() %>%
  add_model(pruned_tree_model) %>%
  add_recipe(movies_recipe)
# fit
pruned_tree_fit <- fit(pruned_tree_wkflow,
                       data = movies_train)
# visualize tree
pruned_tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot()
# output rmse on training data
augment(pruned_tree_fit, new_data = movies_train) %>%
  rmse(truth = score, estimate=.pred)

# 4. PRUNED TREE WITH TUNING ON COST_COMPLEXITY 
# workflow
pruned_tree_tune_wkflow <- workflow() %>%
  add_model(pruned_tree_model %>%
              set_args(cost_complexity = tune())) %>%
  add_recipe(movies_recipe)
# create grid for values of cost complexity 
pruned_cost_grid <- grid_regular(cost_complexity(range(-3,-1)), 
                                 levels=10)
# tune
pruned_tune_res <- tune_grid(
  pruned_tree_tune_wkflow,
  resamples = movies_fold,
  grid = pruned_cost_grid)
autoplot(pruned_tune_res)
# model achieves best value at lowest cost-complexity 
# need a more complex model 
# we will fit this model, but we are going
## to need a random forest 
# get best complexity 
best_complexity <- select_best(pruned_tune_res,
                               metric = "rmse")
pruned_tree_tune_final <- finalize_workflow(
  pruned_tree_tune_wkflow, best_complexity)
pruned_tree_tune_final_fit <- fit(
  pruned_tree_tune_final,
  data = movies_train)
# visualize the model
pruned_tree_tune_final_fit %>%
  extract_fit_engine() %>%
  rpart.plot()
# check performance
augment(pruned_tree_tune_final_fit,
        new_data = movies_train) %>%
  rmse(truth = score, estimate = .pred)
# get value of 0.7 - not best, want lower
augment(pruned_tree_tune_final_fit,
        new_data = movies_train) %>%
  ggplot(aes(score, .pred)) +
  geom_abline() +
  geom_point(alpha = 0.5)
# need more complex model!

# LAST MODEL WILL BE NEAREST NEIGHBORS 



