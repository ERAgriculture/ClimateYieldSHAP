# Create hexbin plots for observed vs predicted data

create_and_save_obs_pred_plot <- function(workflow, 
                                          test_data, 
                                          target_col = "yield", 
                                          experiment = " ",
                                          AEZ = "") {
  
  # Load required libraries
  require(tidymodels)
  require(ggplot2)
  require(hexbin)
  require(viridis)
  require(dplyr)
  
  # Create full experiment name
  full_experiment_name <- paste0(experiment, "_", AEZ)
  
  # Create output directories for saving plots
  plot_dir <- file.path("outputs", full_experiment_name, "hexbin_plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Generate predictions on the test data
  predictions <- workflow %>% 
    predict(new_data = test_data)
  
  # Combine predictions with actual values
  results <- bind_cols(predictions, test_data %>% select(all_of(target_col)))
  
  # Calculate performance metrics
  metrics <- metric_set(rmse, rsq)
  performance <- metrics(results, truth = !!sym(target_col), estimate = .pred)
  
  # Extract values for annotation
  rsq_value <- performance %>% 
    filter(.metric == "rsq") %>% 
    pull(.estimate) %>% 
    round(3)
  
  rmse_value <- performance %>% 
    filter(.metric == "rmse") %>% 
    pull(.estimate) %>% 
    round(3)
  
  # Get range for equal axes
  min_val <- min(min(results[[target_col]]), min(results$.pred))
  max_val <- max(max(results[[target_col]]), max(results$.pred))
  
  # Create hexbin plot
  p <- ggplot(results, aes(x = !!sym(target_col), y = .pred)) +
    # Add hexbin layer with viridis color scale
    geom_hex(bins = 30) +
    scale_fill_viridis_c(option = "viridis") +
    # Add 1:1 reference line
    geom_abline(intercept = 0, slope = 1, color = "red", 
                linetype = "dashed", linewidth = 0.5) +
    # Add annotation with performance metrics
    annotate("text", 
             x = min_val + 0.1 * (max_val - min_val), 
             y = max_val - 0.1 * (max_val - min_val),
             label = paste("R² =", rsq_value, "\nRMSE =", rmse_value),
             hjust = 0, vjust = 1, size = 4.5) +
    # Set axis labels and title
    labs(
      title = paste("Observed vs. Predicted:", full_experiment_name),
      x = "Observed Values",
      y = "Predicted Values",
      fill = "Count"
    ) +
    # Set equal scaling on both axes
    coord_equal(xlim = c(min_val, max_val), ylim = c(min_val, max_val)) +
    # Apply minimalist theme with white background
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = "right"
    )
  
  # Save the plot as an RDS object
  rds_path <- file.path(plot_dir, paste0(full_experiment_name, "_obs_vs_pred_hexbin.rds"))
  saveRDS(p, rds_path)
  
  # Save the plot as a PNG image
  png_path <- file.path(plot_dir, paste0(full_experiment_name, "_obs_vs_pred_hexbin.png"))
  ggsave(png_path, p, width = 8, height = 6, dpi = 300, bg = "white")
  
  # Display the plot
  print(p)
  
  # Return the plot object
  return(p)
}