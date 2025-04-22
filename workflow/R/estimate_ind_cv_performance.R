plot_cv_shap_importance <- function(cv_results, x_range = NULL) {
  # Prepare the data - convert to correct structure and sort
  plot_data <- cv_results$shap_importance_summary %>%
    transmute(
      feature = Variable,
      importance = mean_shap
    ) %>%
    arrange(importance) %>%  # Sort ascending for consistent display
    mutate(feature = factor(feature, levels = feature))  # Keep the order
  
  # Create the plot using same styling as shap_mean_importance_plot
  p <- ggplot(plot_data, aes(y = feature, x = importance)) +
    geom_col() +
    theme_bw() +
    labs(title = "Cross-validated SHAP Feature Importance",
         x = "Mean |SHAP value|",
         y = "Feature") +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  # If x_range is provided, use scale_x_continuous
  if (!is.null(x_range)) {
    p <- p + scale_x_continuous(limits = x_range)
  }
  
  # Save the standardized data structure with the plot
  p$data <- plot_data
  
  return(p)
}

estimate_ind_cv_performance <- function(data, target_col, formula, recipe, n_folds = 5, experiment) {
  # Create CV folds
  set.seed(123)
  cv_folds <- vfold_cv(data, v = n_folds, strata = !!sym(target_col))
  
  # Initialize storage for metrics and importance
  cv_metrics <- list()
  cv_mda_importance <- list()
  cv_shap_importance <- list()
  
  # Create metric set
  metrics <- metric_set(rmse, rsq, mae)
  
  # Perform CV
  for(fold in 1:n_folds) {
    # Get training and validation data for this fold
    train_data <- analysis(cv_folds$splits[[fold]])
    val_data <- assessment(cv_folds$splits[[fold]])
    
    # Train model on this fold
    fold_workflow <- train_simple_rf(
      data = train_data,
      target_col = target_col,
      formula = formula,
      rec = recipe,
      mtry = 3,
      trees = 1000,
      min_n = 25,
      max.depth = 7
    )
    
    # Calculate metrics for this fold
    predictions <- predict(fold_workflow, new_data = val_data)$.pred
    metrics_df <- metrics(
      data = data.frame(
        truth = val_data[[target_col]],
        estimate = predictions
      ),
      truth = truth,
      estimate = estimate
    )
    
    cv_metrics[[fold]] <- metrics_df
    
    # Calculate MDA importance for this fold
    mda_importance <- fold_workflow$fit$fit$fit$variable.importance
    cv_mda_importance[[fold]] <- tibble(
      Variable = names(mda_importance),
      MDA_Importance = mda_importance,
      Fold = fold
    )
    
    # Get SHAP importance from plot data
    shap_plot <- shap_mean_importance_plot(
      workflow = fold_workflow,
      new_data = train_data
    )
    
    # Extract the plot data - it already contains mean absolute SHAP values
    shap_importance <- shap_plot$data %>%
      rename(
        Variable = feature,
        SHAP_Importance = importance
      ) %>%
      mutate(Fold = fold)
    
    cv_shap_importance[[fold]] <- shap_importance
  }
  
  # Aggregate metrics across folds
  cv_metrics_summary <- bind_rows(cv_metrics) %>%
    group_by(.metric) %>%
    summarise(
      mean = mean(.estimate),
      sd = sd(.estimate),
      .groups = 'drop'
    )
  
  # Aggregate MDA importance across folds
  cv_mda_importance_summary <- bind_rows(cv_mda_importance) %>%
    group_by(Variable) %>%
    summarise(
      mean_mda = mean(MDA_Importance),
      sd_mda = sd(MDA_Importance),
      .groups = 'drop'
    ) %>%
    arrange(desc(mean_mda))
  
  # Aggregate SHAP importance across folds
  cv_shap_importance_summary <- bind_rows(cv_shap_importance) %>%
    group_by(Variable) %>%
    summarise(
      mean_shap = mean(SHAP_Importance),
      sd_shap = sd(SHAP_Importance),
      .groups = 'drop'
    ) %>%
    arrange(desc(mean_shap))
  
  # Create and save the CV SHAP plot
  cv_shap_plot <- plot_cv_shap_importance(list(
    shap_importance_summary = cv_shap_importance_summary
  ))
  
  # Save the plot
  ggsave(paste0("outputs/", experiment, "/full_results/shap_mean_importance.png"), 
         cv_shap_plot, width = 12, height = 8)
  save_plot_object(plot = cv_shap_plot, 
                   output_dir = paste0("outputs/", experiment, "/full_results/"),
                   filename = "shap_mean_importance")
  cv_results <- list(
    metrics = cv_metrics_summary,
    mda_importance = cv_mda_importance_summary,
    shap_importance = cv_shap_importance_summary,
    fold_metrics = cv_metrics,
    fold_mda = cv_mda_importance,
    fold_shap = cv_shap_importance,
    shap_plot = cv_shap_plot
  )
  write.csv(cv_results$metrics, 
            paste0("outputs/", experiment, "/full_results/cv_metrics.csv"))
  
  return(cv_results)
}