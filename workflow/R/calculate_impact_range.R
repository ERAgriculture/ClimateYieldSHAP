library(dplyr)
library(mgcv)
library(stats)

calculate_impact_range <- function(x_values, y_values, 
                                   noise_threshold = 0.3,
                                   window_size = 0.1,
                                   min_dev_explained = 0.2) {
  # Create data frame from inputs
  data <- data.frame(x = x_values, y = y_values)
  
  # Fit GAM model instead of linear regression
  gam_model <- gam(y ~ s(x, k = 3), data = data, method="REML")
  dev_explained <- summary(gam_model)$r.sq
  
  # Get fitted values for trend analysis
  fitted_vals <- fitted(gam_model)
  
  # Calculate ranges
  y_range <- diff(range(y_values))
  x_range <- diff(range(x_values))
  
  # Calculate noise metrics using residuals from GAM
  residuals <- residuals(gam_model)
  residual_iqr <- IQR(residuals)
  noise_level <- residual_iqr / y_range
  
  # Calculate local variations
  window_width <- window_size * x_range
  x_windows <- seq(min(x_values), max(x_values), by = window_width)
  
  local_stats <- lapply(x_windows, function(window_center) {
    window_data <- data %>%
      filter(x >= window_center, x < window_center + window_width)
    
    if(nrow(window_data) > 0) {
      list(
        mean = mean(window_data$y),
        sd = sd(window_data$y)
      )
    } else {
      list(mean = NA, sd = NA)
    }
  })
  
  # Extract local means and sds
  local_means <- sapply(local_stats, function(x) x$mean)
  local_sds <- sapply(local_stats, function(x) x$sd)
  
  # Remove NAs
  local_means <- local_means[!is.na(local_means)]
  local_sds <- local_sds[!is.na(local_sds)]
  
  # Calculate mean local variation
  mean_local_sd <- mean(local_sds, na.rm = TRUE)
  
  # More lenient criteria for meaningful trend
  is_meaningful <- (dev_explained > min_dev_explained &&
                      noise_level < noise_threshold &&
                      mean_local_sd < y_range * noise_threshold)
  
  # Calculate impact range using fitted values to capture nonlinear trend
  if(is_meaningful) {
    # Use 5th and 95th percentiles of fitted values to avoid edge effects
    impact_range <- diff(quantile(fitted_vals, probs = c(0.05, 0.95)))
  } else {
    impact_range <- 0
  }
  
  list(
    impact_range = impact_range,
    is_meaningful = is_meaningful,
    deviance_explained = dev_explained,
    noise_level = noise_level,
    mean_local_sd = mean_local_sd,
    y_range = y_range,
    analysis_details = list(
      gam_summary = summary(gam_model),
      local_means = local_means,
      local_sds = local_sds,
      fitted_values = fitted_vals
    )
  )
}