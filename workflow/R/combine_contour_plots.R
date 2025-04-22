library(ggplot2)
library(patchwork)

combine_contour_plots <- function(plot_list, row_names, col_names, ncol = 6) {
  # Remove legends and titles from individual plots without modifying the fill scale
  plot_list <- lapply(plot_list, function(p) {
    p + theme(legend.position = "none", 
              plot.title = element_blank(),
              plot.subtitle = element_blank())
  })
  
  # Combine plots using patchwork
  combined_plot <- wrap_plots(plot_list, ncol = ncol)
  
  # Add row and column labels
  if (!is.null(row_names) || !is.null(col_names)) {
    combined_plot <- combined_plot + 
      plot_annotation(
        title = if(!is.null(col_names)) paste(col_names, collapse = "   ") else NULL,
        caption = if(!is.null(row_names)) paste(row_names, collapse = "\n") else NULL,
        theme = theme(
          plot.title = element_text(hjust = 0.5, size = 12),
          plot.caption = element_text(hjust = 0, size = 12)
        )
      )
  }
  
  return(combined_plot)
}