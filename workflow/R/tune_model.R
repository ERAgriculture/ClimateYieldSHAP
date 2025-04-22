require("tidymodels")
require("ggplot2")
require("xgboost")

tune_model <- function(workflow, 
                       resamples, 
                       grid = NULL, 
                       metrics = NULL, 
                       control = control_grid(), 
                       tune_args = list(), 
                       verbose = FALSE,
                       model_name,
                       output_dir = "model_output/tuning") {
  
  # Extract the model specification from the workflow
  model_spec <- workflow$fit$actions$model$spec
  
  # Check if the model has tunable parameters
  has_tunable_params <- length(grid) > 0
  
  if (has_tunable_params) {
    # Set default grid if not provided
    if (is.null(grid)) {
      grid <- model_spec %>%
        parameters() %>%
        grid_max_entropy(size = 10)
    }
    
    # Set default metrics based on the model mode
    if (is.null(metrics)) {
      mode <- model_spec$mode
      if (mode == "regression") {
        metrics <- metric_set(rmse, rsq, mae)
      } else if (mode == "classification") {
        metrics <- metric_set(accuracy, roc_auc, sensitivity, specificity)
      }
    }
    
    # Combine default arguments with user-provided arguments
    tune_args <- c(list(
      object = workflow,
      resamples = resamples,
      grid = grid,
      metrics = metrics,
      control = control
    ), tune_args)
    
    # Remove any NULL arguments
    tune_args <- tune_args[!sapply(tune_args, is.null)]
    tune_args <- tune_args[!duplicated(names(tune_args))]
    tune_args <- tune_args[names(tune_args) != ""]
    
    # Call tune_grid with all arguments
    tune_result <- do.call(tune_grid, 
                           tune_args)
  } else {
    # For models without tunable parameters, use fit_resamples
    tune_result <- fit_resamples(
      workflow,
      resamples = resamples,
      metrics = metrics,
      control = control
    )
  }
  
  if (verbose) {
    print(tune_result)
  }
  
  # Select the best model (or the only model for non-tunable cases)
  if (has_tunable_params) {
    best_model <- select_best(tune_result, metric = "rsq")
  } else {
    best_model <- tune_result
  }
  
  # Get the model type from the model specification
  model_type <- model_spec$engine
  
  # Create the output directory and subdirectories if they don't exist
  model_dir <- file.path(output_dir, "models")
  metrics_dir <- file.path(output_dir, "metrics")
  
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  if (!dir.exists(metrics_dir)) {
    dir.create(metrics_dir, recursive = TRUE)
  }
  
  # Generate file names based on the model type
  model_file <- file.path(model_dir, paste0(model_name, "_best_model.rds"))
  cv_metrics_file <- file.path(metrics_dir, paste0(model_name, "_cv_metrics.csv"))
  best_cv_metrics_file <- file.path(metrics_dir, paste0(model_name, "_best_cv_metrics.csv"))
  
  # Save the best model
  saveRDS(best_model, file = model_file)
  cat("Model has been saved as:", model_file, "\n")
  
  # Save the cross-validation metrics
  cv_metrics <- collect_metrics(tune_result)
  write_csv(cv_metrics, file = cv_metrics_file)
  
  # Save CV performance metrics for the best parameter set
  if (has_tunable_params) {
    best_params <- best_model$.config
    best_cv_metrics <- cv_metrics %>%
      filter(.config == best_params)
    
    write_csv(best_cv_metrics, file = best_cv_metrics_file)
    cat("Best CV metrics have been saved as:", best_cv_metrics_file, "\n")
  }
  
  return(tune_result)
}