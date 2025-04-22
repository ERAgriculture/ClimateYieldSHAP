plot_bootstrap_shap <- function(bootstrap_results, 
                                full_results,
                                feature,
                                color_by = NULL,
                                confidence_level = 0.95,
                                AEZ = "", 
                                title = NULL) {
  
  round_up_custom <- function(x) {
    d <- ifelse(abs(x) < 1, 2, ifelse(abs(x) < 50, 1, 0))
    factor <- 10^d
    ceiling(x * factor) / factor
  }
  
  round_down_custom <- function(x) {
    d <- ifelse(abs(x) < 1, 2, ifelse(abs(x) < 50, 1, 0))
    factor <- 10^d
    floor(x * factor) / factor
  }
  
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
  
  # Create data frame for full dataset SHAP values
  full_data <- data.frame(
    bootstrap = 0,
    feature_val = full_results$shap_values[[feature]],
    shap_val = full_results$shap_values[[paste0("SHAP_", feature)]],
    color_val = full_results$shap_values[[color_by]]
  )
  
  # The full data sample is part of the bootstrap distribution
  shap_data <- bind_rows(shap_data, full_data)
  
  # Base plot with individual bootstrap points
  p <- ggplot() +
    geom_point(data = shap_data %>% filter(bootstrap != 0), 
               aes(x = feature_val, y = shap_val),
               color = "grey80",
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
      mean_shap = mean(shap_val),
      lower = quantile(shap_val, (1 - confidence_level) / 2),
      upper = quantile(shap_val, 1 - (1 - confidence_level) / 2)
    )
  
  p <- p +
    # Add confidence interval ribbon
    geom_ribbon(data = rolling_stats,
                aes(x = x, ymin = lower, ymax = upper),
                fill = "navy", alpha = 0.2) +
    # Add means as plus signs
    geom_point(data = rolling_stats,
               aes(x = x, y = mean_shap),
               shape = 3,  
               size = 3,
               color = "black") +
    # Add full dataset SHAP values as colored points
    geom_point(data = full_data,
               aes(x = feature_val, y = shap_val, color = color_val),
               size = 1.5,
               alpha = 0.2)
  
  # Add final styling
  if (is.factor(full_data$color_val) || is.character(full_data$color_val)) {
    p <- p + scale_color_discrete(name = gsub("_", " ", color_by))
  } else {
    p <- p + scale_color_gradientn(colors = c("blue", "yellow", "red"),
                                   name = gsub("_", " ", color_by),
                                   breaks = c(round_up_custom(min(full_data$color_val)), 
                                              round_down_custom(max(full_data$color_val))))
  }
  
  p <- p +
    theme_minimal() +
    labs(
      title = title %||% paste(gsub("_", " ", feature), " Effect"),
      x = gsub("_", " ", feature),
      y = "Yield change (kg/ha)",
      color = gsub("_", " ", color_by)
    ) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
  
  return(p)
}