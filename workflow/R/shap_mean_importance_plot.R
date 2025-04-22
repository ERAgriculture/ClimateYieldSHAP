library(tidymodels)
library(treeshap)
library(ggplot2)
library(dplyr)

shap_mean_importance_plot <- function(workflow, 
                                 new_data,
                                 num_features = 10,
                                 x_range = NULL) {
  
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
  
  # Calculate mean absolute SHAP values for each feature
  mean_shap <- shap$shaps %>%
    abs() %>%
    colMeans() %>%
    sort(decreasing = FALSE) %>%
    head(num_features)
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    feature = factor(names(mean_shap), 
                     levels = names(mean_shap)),
    importance = mean_shap
  )
  
  # Create the SHAP importance plot
  p <- ggplot(plot_data, aes(y = feature, x = importance)) +
    geom_col() +
    theme_bw() +
    labs(title = "SHAP Feature Importance",
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
  
  return(p)
}