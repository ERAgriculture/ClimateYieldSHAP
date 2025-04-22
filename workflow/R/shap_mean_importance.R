library(tidymodels)
library(treeshap)
library(ggplot2)
library(dplyr)

shap_mean_importance <- function(workflow, 
                             new_data,
                             feature = NULL,
                             num_features = 10,
                             interactions = FALSE) {
  
  # Extract the model and recipe from the workflow
  model <- extract_fit_parsnip(workflow)$fit
  recipe <- extract_recipe(workflow)
  
  # Prepare the data
  pred_data <- bake(recipe, new_data = new_data)
  
  # Remove the target variable if present
  target_var <- outcome_names(recipe)
  pred_data <- pred_data %>% select(-all_of(target_var))
  
  # Convert ranger object to treeshap-compatible format
  unified_model <- treeshap::ranger.unify(model, pred_data)
  
  # Calculate SHAP values
  shap <- treeshap::treeshap(unified_model, pred_data, verbose = FALSE)
  
  if (!interactions) {
    # Calculate mean absolute SHAP values for each feature
    mean_shap <- shap$shaps %>%
      abs() %>%
      colMeans() %>%
      sort(decreasing = TRUE) %>%
      head(num_features)
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      feature = names(mean_shap),
      importance = mean_shap
    )
    
    # Create the standard SHAP plot
    p <- ggplot(plot_data, aes(x = reorder(feature, importance), y = importance)) +
      geom_col() +
      coord_flip() +
      theme_bw() +
      labs(title = "SHAP Feature Importance",
           x = "Feature",
           y = "Mean |SHAP value|")
  } else {
    if (is.null(feature)) {
      stop("Please specify a feature for interaction plot.")
    }
    
    # Create the SHAP interaction plot
    plot_data <- data.frame(
      feature_value = pred_data[[feature]],
      shap_value = shap$shaps[, feature]
    )
    
    p <- ggplot(plot_data, aes(x = feature_value, y = shap_value)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      theme_bw() +
      labs(title = paste("SHAP Interaction Plot for", feature),
           x = feature,
           y = "SHAP value")
  }
  
  return(p)
}