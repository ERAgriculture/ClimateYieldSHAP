# Create plots with SHAP values for full data and bootstrapped models using
# improved calculations for bands estimated using GAMs

library(splines)
plot_bootstrap_shap_improved <- function(bootstrap_results, 
                                         full_results,
                                         feature,
                                         color_by = NULL,
                                         confidence_level = 0.95,
                                         title = NULL,
                                         AEZ = "") {
  
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
    bootstrap = 0, # Use 0 to identify the full dataset
    feature_val = full_results$shap_values[[feature]],
    shap_val = full_results$shap_values[[paste0("SHAP_", feature)]],
    color_val = full_results$shap_values[[color_by]]
  )
  
  # Base plot with individual bootstrap points
  p <- ggplot() +
    geom_point(data = shap_data, 
               aes(x = feature_val, y = shap_val),
               color = "grey80",
               alpha = 0.1,
               size = 1)
  
  # Calculate quantile GAMs directly on bootstrap data
  alpha <- (1 - confidence_level) / 2
  
  # Create a grid of points for prediction
  pred_grid <- data.frame(
    feature_val = seq(min(shap_data$feature_val), 
                      max(shap_data$feature_val), 
                      length.out = 300)
  )
  
  # Keep fitting the median GAM for calculations, but we won't plot it
  gam_median <- mgcv::gam(
    shap_val ~ s(feature_val, bs = "cs", k = 20),
    data = shap_data,
    method = "REML"
  )
  
  # Create a density estimator for feature values
  density_est <- density(shap_data$feature_val, adjust = 1.5)
  density_func <- approxfun(density_est$x, density_est$y, rule = 2)
  
  # Assign weights based on inverse of density
  shap_data$weight <- 1 / (density_func(shap_data$feature_val) + 0.01)
  
  # Fit weighted quantile regressions
  library(quantreg)
  
  lower_q <- rq(shap_val ~ bs(feature_val, df = 15), 
                data = shap_data, 
                tau = alpha,
                weights = shap_data$weight)
  
  upper_q <- rq(shap_val ~ bs(feature_val, df = 15), 
                data = shap_data, 
                tau = 1 - alpha,
                weights = shap_data$weight)
  
  # Predict confidence intervals
  pred_grid$median <- predict(gam_median, newdata = pred_grid)
  pred_grid$lower <- predict(lower_q, newdata = pred_grid)
  pred_grid$upper <- predict(upper_q, newdata = pred_grid)
  
  # Add confidence intervals to plot - JUST REMOVED THE MEDIAN LINE HERE
  p <- p +
    # Line below is removed
    # geom_line(data = pred_grid, aes(x = feature_val, y = median), color = "navy", size = 1) +
    geom_line(data = pred_grid,
              aes(x = feature_val, y = lower),
              color = "navy", linetype = "dashed") +
    geom_line(data = pred_grid,
              aes(x = feature_val, y = upper),
              color = "navy", linetype = "dashed") +
    # Add density rug to show data concentration
    geom_rug(data = shap_data, 
             aes(x = feature_val), 
             alpha = 0.1, 
             sides = "b") +
    # Add full dataset SHAP values as colored points
    geom_point(data = full_data,
               aes(x = feature_val, y = shap_val, color = color_val),
               size = 1.5,
               alpha = 0.8)
  
  # Handle color scale as before
  if (is.factor(full_data$color_val) || is.character(full_data$color_val)) {
    p <- p + scale_color_discrete(name = gsub("_", " ", color_by))
  } else {
    p <- p + scale_color_gradientn(colors = c("blue", "purple", "red"),
                                   name = gsub("_", " ", color_by))
  }
  
  p <- p +
    theme_minimal() +
    labs(
      title = title %||% paste(gsub("_", " ", feature), "Effect"),
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