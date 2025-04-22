library(ggplot2)
library(patchwork)
library(dplyr)

combine_shap_plots <- function(shap_importance_plot, beeswarm_plots, main_title, subplot_titles) {
  if (length(subplot_titles) != length(beeswarm_plots) + 1) {
    stop("The number of subplot titles should be equal to the number of beeswarm plots plus one (for the feature importance plot).")
  }
  
  # Function to safely get y range
  safe_y_range <- function(plot) {
    y_values <- layer_data(plot)$y
    y_values <- y_values[is.finite(y_values)]
    if (length(y_values) == 0) return(c(NA, NA))
    quantiles <- quantile(y_values, probs = c(0.001, 0.999), na.rm = TRUE)
    c(max(min(y_values), quantiles[1]), min(max(y_values), quantiles[2]))
  }
  
  # Find the common y-axis range for beeswarm plots
  y_ranges <- lapply(beeswarm_plots, safe_y_range)
  y_ranges <- y_ranges[!is.na(y_ranges)]
  if (length(y_ranges) == 0) stop("No valid y ranges found")
  
  y_min <- min(sapply(y_ranges, `[`, 1), na.rm = TRUE)
  y_max <- max(sapply(y_ranges, `[`, 2), na.rm = TRUE)
  
  # Add a 5% buffer to the y-axis range
  y_range <- y_max - y_min
  common_y_range <- c(y_min - 0.05 * y_range, y_max + 0.05 * y_range)
  
  # Add title to the feature importance plot
  shap_importance_plot <- shap_importance_plot +
    ggtitle(subplot_titles[1]) +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  
  # Adjust y-axis limits for beeswarm plots and add subplot titles
  beeswarm_plots <- mapply(function(plot, title) {
    plot + 
      coord_cartesian(ylim = common_y_range) +
      scale_y_continuous(limits = common_y_range, oob = scales::squish) +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, size = 14))
  }, beeswarm_plots, subplot_titles[-1], SIMPLIFY = FALSE)
  
  # Combine plots using patchwork
  combined_plot <- (
    shap_importance_plot + 
      beeswarm_plots[[1]] + 
      beeswarm_plots[[2]] + 
      # beeswarm_plots[[3]] +
      plot_layout(ncol = 3)
  ) +
    plot_annotation(
      title = main_title,
      theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
    )
  
  return(combined_plot)
}