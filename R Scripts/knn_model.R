install.packages("kknn")
library(kknn)

knn_model <- 
  nearest_neighbor(
    neighbors = tune(),
    mode = "regression") %>% 
  set_engine("kknn")

knn_workflow <- workflow() %>% 
  add_model(knn_model) %>% 
  add_recipe(movies_recipe)

# define grid
knn_grid <- grid_regular(neighbors(range = c(1,10)), levels = 10)

tune_knn <- tune_grid(knn_workflow,
                        resamples = movies_fold,
                        grid = knn_grid)

autoplot(tune_knn, metric = "rmse")

# select best model 
best_knn_res <- select_best(tune_knn,
                              metric = "rmse")
knn_final <- finalize_workflow(
  knn_workflow, best_knn_res)
knn_final_fit <- fit(
  knn_final,
  data = movies_train)
# check performance
augment(knn_final_fit,
        new_data = movies_train) %>%
  rmse(truth = score, estimate = .pred)
# plot the true versus the predicted values
augment(knn_final_fit,
        new_data = movies_train) %>%
  ggplot(aes(score, .pred)) +
  geom_abline() +
  geom_point(alpha = 0.5)
