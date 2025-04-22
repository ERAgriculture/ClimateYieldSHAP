estimate_metrics_simple_rf <- function(final_workflow,
                                       bootstrap_data,
                                       test_data,
                                       target_col) {
  metrics <- metric_set(rmse, mae, rsq, mape)
  
  train_predictions <- final_workflow %>% predict(new_data = bootstrap_data)
  train_metrics <- metric_set(rmse, mae, rsq)(data.frame(
    pred = train_predictions$.pred, 
    truth = bootstrap_data[[target_col]]), 
    truth = truth, 
    estimate = pred)
  
  test_predictions <- final_workflow %>% predict(new_data = test_data)
  
  test_metrics <- metric_set(rmse, mae, rsq)(data.frame(
    pred = test_predictions$.pred, 
    truth = test_data[[target_col]]), 
    truth = truth, 
    estimate = pred)
  
  oob_RMSE <- sqrt(final_workflow$fit$fit$fit$prediction.error)
  oob_R2 <- final_workflow$fit$fit$fit$r.squared
  oob_metrics <- tibble(.metric = c("rmse", "rsq"), 
                        .estimator = c("standard", "standard"),
                        .estimate = c(oob_RMSE, oob_R2))
  
  return(list("train_metrics" = train_metrics, 
              "oob_metrics" = oob_metrics, 
              "test_metrics" = test_metrics))
}