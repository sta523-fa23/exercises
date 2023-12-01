library(tidymodels)
library(tidyverse)

hotels = read_csv(
  'https://tidymodels.org/start/case-study/hotels.csv'
) |>
  mutate(
    across(where(is.character), as.factor)
  )

set.seed(123)

splits = initial_split(
  hotels, strata = children
)

hotel_train = training(splits)
hotel_test = testing(splits)


## Random forest model

rf_model = rand_forest(mtry = tune(), min_n = tune(), trees = 100) |> 
  set_engine("ranger", num.threads = 8) |> 
  set_mode("classification")


## Recipe & workflow

rf_recipe = recipe(children ~ ., data = hotel_train) |> 
  step_date(arrival_date) |> 
  step_holiday(arrival_date, holidays = holidays) |> 
  step_rm(arrival_date) |>
  step_rm(country)


rf_work = workflow() |> 
  add_model(rf_model) |> 
  add_recipe(rf_recipe)


## Tuning - automatic grid search

rf_tune = rf_work |> 
  tune_grid(
    hotel_vf,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )

rf_tune |> 
  collect_metrics() |>
  arrange(desc(mean))


  :
  
  
## "Best" parameters

rf_tune |> 
  show_best(metric = "roc_auc")


  
autoplot(rf_tune)


## Re-fitting

rf_best = rf_tune |>
  select_best(metric = "roc_auc")


rf_work_tuned = finalize_workflow(
  rf_work, 
  rf_best
)

( rf_fit = rf_work_tuned |>
    fit(data=hotel_train) )


  
## Test Performance (out-of-sample)

rf_test_perf = rf_fit |>
  augment(new_data = hotel_test) |>
  select(children, starts_with(".pred"))

conf_mat(rf_test_perf, children, .pred_class)

accuracy(rf_test_perf, children, .pred_class)

precision(rf_test_perf, children, .pred_class)


rf_roc = yardstick::roc_curve(
  rf_test_perf,
  children,
  .pred_children
) |>
  mutate(name = "RF - test")
rf_roc |>
  autoplot()

roc_auc(rf_test_perf, children, .pred_children)

  
## Comparing models

bind_rows(
  lr_test_roc,
  lasso_roc,
  dt_roc,
  rf_roc
) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = name)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal()

