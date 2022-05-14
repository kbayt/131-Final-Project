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

# 2. RIDGE REGRESSION
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