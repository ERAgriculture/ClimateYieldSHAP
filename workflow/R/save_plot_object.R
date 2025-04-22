save_plot_object <- function(plot, output_dir, filename) {
  # Create the full path for the plot objects folder
  plot_objects_dir <- file.path(output_dir, "plot_objects")
  
  # Create the directory if it doesn't exist
  dir.create(plot_objects_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Save the plot object
  saveRDS(plot, file = file.path(plot_objects_dir, paste0(filename, ".rds")))
}