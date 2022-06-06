library(tidymodels)
library(ISLR)
library(ISLR2)
library(tidyverse)
library(glmnet)

# create specification and tune penalty 
ridge_spec <- 
  linear_reg(penalty = 0, mixture = 0) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

# create workflow 
ridge_workflow <- workflow() %>% 
  add_recipe(movies_recipe) %>% 
  add_model(ridge_spec)

ridge_fit <- fit(ridge_workflow, data = movies_train)

# create grid of values
penalty_grid <- grid_regular(penalty(range = c(-5, 5)), levels = 50)
penalty_grid

# tune the model
tune_ridge <- tune_grid(ridge_workflow,
                        resamples = movies_fold,
                        grid = penalty_grid)
collect_notes(tune_lasso)
view(tune_lasso)
view(tune_ridge)

## LASSO
lasso_spec <- 
  linear_reg(penalty = tune(), mixture = 1) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet") 

lasso_workflow <- workflow() %>% 
  add_recipe(movies_recipe) %>% 
  add_model(lasso_spec)

tune_lasso <- tune_grid(
  lasso_workflow,
  resamples = movies_fold, 
  grid = penalty_grid
)

