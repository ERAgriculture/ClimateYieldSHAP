library(tidymodels)
library(treeshap)
library(ggplot2)
library(dplyr)
library(scales)

create_shap_interaction_plot <- function(workflow, 
                                         new_data,
                                         feature1,
                                         feature2,
                                         color_feature = NULL,
                                         poly_degree = NULL,
                                         plotly_additional_vars = c()) {
  
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
  
  # Extract SHAP values for the two features of interest
  shap_values <- data.frame(
    feature1_value = pred_data[[feature1]],
    feature2_value = pred_data[[feature2]],
    shap1 = shap$shaps[, feature1],
    shap2 = shap$shaps[, feature2]
  )
  
  # Add additional variables for ggplotly
  for (var in plotly_additional_vars) {
    shap_values[[var]] <- new_data[[var]]
  }
  
  # If a color_feature is specified, add it to the shap_values dataframe
  if (!is.null(color_feature)) {
    if (color_feature %in% names(new_data)) {
      shap_values$color_value <- new_data[[color_feature]]
    } else {
      stop(paste("Color feature", color_feature, "not found in the dataset"))
    }
  } else {
    # If no color_feature is specified, use feature2_value for coloring
    shap_values$color_value <- shap_values$feature2_value
    color_feature <- feature2
  }
  
  # Determine if the color feature is continuous or categorical
  is_continuous <- is.numeric(shap_values$color_value)
  
  # Create the base plot
  p <- ggplot(shap_values, aes(x = feature1_value, y = shap1, color = color_value))
  
  # Add polynomial fit if specified (now added before the points)
  if (!is.null(poly_degree) && is.numeric(poly_degree) && poly_degree > 0) {
    p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, degree = poly_degree), 
                         se = FALSE, color = "black", linetype = "dashed")
  }
  
  # Add points on top of the fit line
  p <- p + geom_point(alpha = 0.5,
                      stroke = 0.25)
  
  # Apply appropriate color scale based on the type of color feature
  if (is_continuous) {
    p <- p + scale_color_gradient(low = "yellow", high = "purple")
  } else {
    # For categorical variables, use a discrete color scale
    n_colors <- length(unique(shap_values$color_value))
    p <- p + scale_color_manual(values = hue_pal()(n_colors))
  }
  
  # Add labels and theme
  p <- p + theme_bw() +
    labs(title = paste("SHAP Impact of", feature1, "on yield, color-coded by", color_feature),
         x = feature1,
         y = paste("SHAP value for", feature1),
         color = color_feature) +
    theme(legend.position = "right")
  
  return(list(plot = p, data = shap_values))
}