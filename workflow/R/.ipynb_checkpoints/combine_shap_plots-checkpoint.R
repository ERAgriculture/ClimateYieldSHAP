library(ggplot2)
library(patchwork)

combine_shap_plots <- function(shap_importance_plot, beeswarm_plots, title) {
  # Ensure we have exactly 3 beeswarm plots
  if (length(beeswarm_plots) != 3) {
    stop("Please provide exactly 3 beeswarm plots")
  }
  
  # Find the common y-axis range for beeswarm plots
  y_ranges <- lapply(beeswarm_plots, function(plot) {
    layer_data(plot)$y %>% range(na.rm = TRUE)
  })
  common_y_range <- c(min(sapply(y_ranges, min)), max(sapply(y_ranges, max)))
  
  # Adjust y-axis limits for beeswarm plots
  beeswarm_plots <- lapply(beeswarm_plots, function(plot) {
    plot + coord_cartesian(ylim = common_y_range)
  })
  
  # Combine plots using patchwork
  combined_plot <- (
    shap_importance_plot + 
      beeswarm_plots[[1]] + 
      beeswarm_plots[[2]] + 
      beeswarm_plots[[3]] +
      plot_layout(ncol = 2)
  ) +
    plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
    )
  
  return(combined_plot)
}