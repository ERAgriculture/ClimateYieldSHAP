# Plots SHAP values for a selected feature using bootstrap samples and the full dataset.
# Adds smoothed confidence bands using GAMs fitted to bootstrap SHAP ranges.
# Points are colored by a secondary variable (default is the feature itself).
# Includes full-data SHAP values, optional custom axis rounding, and styling.
# Returns a ggplot object visualizing feature effect with uncertainty.

plot_bootstrap_shap_gamCI <- function(bootstrap_results, 
                                full_results,
                                feature,
                                color_by = NULL,
                                confidence_level = 0.95,
                                title = NULL,
                                AEZ = "", 
                                geom_k = 30) {
  
  
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
  
  # Create data frame for full dataset SHAP values and add it to bootstrap data
  full_data <- data.frame(
    bootstrap = 0, # Use 0 to identify the full dataset
    feature_val = full_results$shap_values[[feature]],
    shap_val = full_results$shap_values[[paste0("SHAP_", feature)]],
    color_val = full_results$shap_values[[color_by]]
  )
  
  # Combine bootstrap and full data for calculations
  combined_data <- bind_rows(shap_data, full_data)
  
  # Base plot with individual bootstrap points
  p <- ggplot() +
    geom_point(data = combined_data %>% filter(bootstrap != 0), 
               aes(x = feature_val, y = shap_val),
               color = "grey80",
               alpha = 0.1,
               size = 1)
  
  # Calculate rolling statistics including mean and confidence intervals
  rolling_stats <- combined_data %>%
    arrange(feature_val) %>%
    mutate(
      bin = cut(feature_val, breaks = 50, labels = FALSE)
    ) %>%
    group_by(bin) %>%
    summarise(
      x = mean(feature_val),
      mean_shap = mean(shap_val),
      lower = min(shap_val),
      upper = max(shap_val)
    )
  
  # Fit GAM to confidence intervals with increased flexibility
  gam_lower <- mgcv::gam(lower ~ s(x, k = geom_k, bs = "cr"),
                         data = rolling_stats,
                         method = "REML")
  gam_upper <- mgcv::gam(upper ~ s(x, k = geom_k, bs = "cr"),
                         data = rolling_stats,
                         method = "REML")
  
  # Create smooth prediction data with more points
  smooth_x <- seq(min(rolling_stats$x), max(rolling_stats$x), length.out = 300)
  smooth_data <- data.frame(
    x = smooth_x,
    lower = predict(gam_lower, newdata = data.frame(x = smooth_x)),
    upper = predict(gam_upper, newdata = data.frame(x = smooth_x))
  )
  
  # Add a buffer to ensure all points are included
  buffer <- (max(smooth_data$upper) - min(smooth_data$lower)) * 0.1
  smooth_data$lower <- smooth_data$lower - buffer
  smooth_data$upper <- smooth_data$upper + buffer
  
  p <- p +
    # Add smoothed confidence interval lines
    geom_line(data = smooth_data,
              aes(x = x, y = lower),
              linetype = "dashed",
              color = "navy",
              alpha = 0.5) +
    geom_line(data = smooth_data,
              aes(x = x, y = upper),
              linetype = "dashed",
              color = "navy",
              alpha = 0.5) +
    # Add bootstrap means as plus signs
    geom_point(data = rolling_stats,
               aes(x = x, y = mean_shap),
               shape = 3,  
               size = 3,
               color = "black",
               alpha=0  # HIDE these symbols
               ) +
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