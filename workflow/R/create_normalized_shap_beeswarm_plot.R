library(tidymodels)
library(treeshap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbeeswarm)

create_normalized_shap_beeswarm_plot <- function(workflow, 
                                                 new_data,
                                                 num_features = 10) {
  
  # Extract the model and recipe from the workflow
  model <- extract_fit_parsnip(workflow)$fit
  recipe <- extract_recipe(workflow)
  
  # Prepare the data
  pred_data <- bake(recipe, new_data = new_data)
  
  # Remove the target variable if present
  target_var <- outcome_names(recipe)
  pred_data <- pred_data %>% select(-all_of(target_var))
  
  # Check if case_weights column exists and remove it
  if ("case_weights" %in% names(pred_data)) {
    pred_data <- pred_data %>% select(-case_weights)
  }
  
  # Convert ranger object to treeshap-compatible format
  unified_model <- treeshap::ranger.unify(model, pred_data)
  
  # Calculate SHAP values
  shap <- treeshap::treeshap(unified_model, pred_data, verbose = FALSE)
  
  # Prepare data for plotting
  shap_long <- as.data.frame(shap$shaps) %>%
    mutate(id = row_number()) %>%
    pivot_longer(-id, names_to = "feature", values_to = "shap_value") %>%
    left_join(pred_data %>% mutate(id = row_number()) %>% 
                pivot_longer(-id, names_to = "feature", values_to = "feature_value"),
              by = c("id", "feature"))
  
  # Calculate feature importance and select top features
  feature_importance <- shap_long %>%
    group_by(feature) %>%
    summarise(importance = mean(abs(shap_value))) %>%
    arrange(desc(importance)) %>%
    slice_head(n = num_features) %>%
    pull(feature)
  
  # Filter for top features
  shap_long_filtered <- shap_long %>% filter(feature %in% feature_importance)
  
  # Normalize feature values within each feature
  shap_long_normalized <- shap_long_filtered %>%
    group_by(feature) %>%
    mutate(normalized_value = (feature_value - min(feature_value)) / 
             (max(feature_value) - min(feature_value))) %>%
    ungroup()
  
  # Create the SHAP summary plot (beeswarm plot)
  p <- ggplot(shap_long_normalized, 
              aes(x = shap_value, y = reorder(feature, abs(shap_value)))) +
    ggbeeswarm::geom_quasirandom(aes(color = normalized_value), 
                                 groupOnX = FALSE, 
                                 alpha = 0.5, 
                                 width = 0.3,
                                 size = 1) +
    scale_color_gradient(low = "blue", high = "red") +
    theme_bw() +
    labs(title = "SHAP Summary Plot (Normalized Feature Values)",
         x = "SHAP value (impact on model output)",
         y = "Feature",
         color = "Normalized\nFeature Value") +
    theme(legend.position = "right")
  
  return(p)
}