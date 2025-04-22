require("tidymodels")
require("ggplot2")

train_and_test_final_model <- function(workflow, 
                                       target_col,
                                       best_model,
                                       train_data, 
                                       test_data, 
                                       model_name,
                                       output_dir = "model_output/final_workflows") {
  # Finalize the workflow with the best model
  final_workflow <- workflow %>%
    finalize_workflow(best_model) %>%
    fit(data = train_data)
  
  # Extract model type from workflow
  model_type <- final_workflow$fit$actions$model$spec$engine
  
  # Make predictions
  predictions <- final_workflow %>% predict(new_data = test_data)
  
  # Combine predictions with actual values
  results <- bind_cols(predictions, test_data)
  
  # Calculate metrics
  metrics <- metric_set(rmse, mae, rsq, mape)
  performance_metrics <- metrics(results, truth = !!sym(target_col), estimate = .pred)
  
  # Create and save observed vs predicted plot
  plot <- ggplot(results, aes(x = !!sym(target_col), y = .pred)) +
    geom_point(alpha = 0.5) +
    geom_abline(color = "red", linetype = "dashed") +
    labs(x = "Observed", y = "Predicted", title = 
           paste(model_type, "- Observed", target_col, "vs Predicted", target_col)) 

  # Create output directory if it doesn't exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save plot
  plot_filename <- file.path(output_dir, paste0(model_name, "_obs_vs_pred.png"))
  ggsave(plot_filename, plot, width = 8, height = 6)
  
  # Save metrics to CSV
  metrics_filename <- file.path(output_dir, paste0(model_name, "_test_metrics.csv"))
  write_csv(performance_metrics, metrics_filename)
  
  # Save the final model
  model_filename <- file.path(output_dir, paste0(model_name, "_final_workflow.rds"))
  saveRDS(final_workflow, file = model_filename)
  
  return(final_workflow)
}