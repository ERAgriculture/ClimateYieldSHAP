library(tidymodels)
library(treeshap)
library(dplyr)

# Only works for Random Forest (ranger) models

compute_shap_values <- function(workflow, train_data) {
  # Extract the model and recipe from the workflow
  model <- extract_fit_parsnip(workflow)$fit
  recipe <- extract_recipe(workflow)
  
  # Prepare the data
  pred_data <- bake(recipe, new_data = train_data)
  
  # Remove the target variable if present
  target_var <- outcome_names(recipe)
  pred_data_no_target <- pred_data %>% select(-all_of(target_var))
  
  # Convert ranger object to treeshap-compatible format
  unified_model <- treeshap::ranger.unify(model, pred_data_no_target)
  
  # Calculate SHAP values
  shap <- treeshap::treeshap(unified_model, pred_data_no_target, verbose = FALSE)
  
  # Convert SHAP values to a dataframe and add prefix to column names
  shap_df <- as.data.frame(shap$shaps)
  names(shap_df) <- paste0("SHAP_", names(shap_df))
  
  # Combine with original training data
  result <- bind_cols(train_data, shap_df)
  
  return(result)
}

# Example usage:
# results <- compute_shap_values(final_workflow, train_data)
# 
# # To get just the SHAP values:
# shap_columns <- select(results, starts_with("SHAP_"))
# 
# # To get specific feature's SHAP values:
# feature_shap <- results %>% select(starts_with("SHAP_feature_name"))
