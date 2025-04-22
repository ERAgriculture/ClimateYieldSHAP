plot_bootstrap_shap <- function(bootstrap_results, 
                                feature,
                                color_by = NULL,
                                confidence_level = 0.95,
                                title = NULL) {
  
  if (is.null(color_by)) color_by <- feature
  
  # Extract bootstrap data
  extract_bootstrap_data <- function(bootstrap_index) {
    data.frame(
      bootstrap = bootstrap_index,
      feature_val = bootstrap_results$shap_values[[bootstrap_index]][[feature]],
      shap_val = bootstrap_results$shap_values[[bootstrap_index]][[paste0("SHAP_", feature)]],
      color_val = bootstrap_results$shap_values[[bootstrap_index]][[color_by]]
    )
  }
  
  # Extract and combine SHAP values across all bootstraps
  shap_data <- map_dfr(seq_along(bootstrap_results$shap_values), 
                       extract_bootstrap_data)
  
  # Base plot with individual points
  p <- ggplot() +
    geom_point(data = shap_data, 
               aes(x = feature_val, y = shap_val),
               color = "grey50",
               alpha = 0.1,
               size = 1)
  
  # Calculate rolling quantiles
  rolling_stats <- shap_data %>%
    arrange(feature_val) %>%
    mutate(
      bin = cut(feature_val, breaks = 50, labels = FALSE)
    ) %>%
    group_by(bin) %>%
    summarise(
      x = mean(feature_val),
      lower = quantile(shap_val, (1 - confidence_level) / 2),
      upper = quantile(shap_val, 1 - (1 - confidence_level) / 2),
      color = mean(color_val)
    )
  
  p <- p +
    geom_ribbon(data = rolling_stats,
                aes(x = x, ymin = lower, ymax = upper),
                fill = "navy", alpha = 0.2) +
    geom_point(data = rolling_stats,
               aes(x = x, y = (lower + upper)/2, color = color),
               size = 2)
    
  # Add final styling
  p <- p +
    scale_color_gradient(low = "blue", high = "red") +
    theme_minimal() +
    labs(
      title = title %||% paste("SHAP values for", feature),
      x = feature,
      y = paste("SHAP value for", feature),
      color = color_by
    ) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
  
  return(p)
}