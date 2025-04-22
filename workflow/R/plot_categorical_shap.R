plot_categorical_shap <- function(bootstrap_results, 
                                  full_results,
                                  feature,
                                  color_by = NULL,
                                  title = NULL) {
  
  if (is.null(color_by)) color_by <- feature
  
  # Get the SHAP values for the categorical variable
  shap_data <- data.frame(
    feature_val = full_results$shap_values[[feature]],
    shap_val = full_results$shap_values[[paste0("SHAP_", feature, "_", levels(full_results$shap_values[[feature]])[1])]],
    color_val = full_results$shap_values[[color_by]]
  )
  
  # Calculate means for each category
  category_means <- shap_data %>%
    group_by(feature_val) %>%
    summarise(
      mean_shap = mean(shap_val),
      .groups = 'drop'
    )
  
  # Create the plot
  p <- ggplot() +
    # Add individual SHAP values
    geom_jitter(data = shap_data,
                aes(x = feature_val, y = shap_val, color = color_val),
                width = 0.2,
                size = 1.5,
                alpha = 0.7) +
    # Add category means
    geom_point(data = category_means,
               aes(x = feature_val, y = mean_shap),
               color = "black",
               size = 3,
               shape = 3) +  # Shape 3 is a plus sign
    # Add horizontal line at y = 0 for reference
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
  
  # Add appropriate color scale
  if (is.factor(shap_data$color_val) || is.character(shap_data$color_val)) {
    p <- p + scale_color_discrete(name = color_by)
  } else {
    p <- p + scale_color_gradient(low = "blue", high = "red", name = color_by)
  }
  
  # Add styling
  p <- p +
    theme_minimal() +
    labs(
      title = title %||% paste("SHAP values for", feature),
      x = feature,
      y = paste("SHAP value")
    ) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1)  # Angled category labels
    )
  
  return(p)
}
