library(dplyr)
library(tidyr)
library(purrr)

analyze_bootstrap_metrics <- function(metrics_list, experiment, set) {
  # Convert list of tibbles to a single dataframe
  all_metrics <- bind_rows(metrics_list)
  
  # Calculate summary statistics for each metric
  summary_stats <- all_metrics %>%
    group_by(.metric) %>%
    summarise(
      mean = mean(.estimate),
      sd = sd(.estimate),
      q1 = quantile(.estimate, 0.25),
      q2 = median(.estimate),
      q3 = quantile(.estimate, 0.75)
    )
  
  # Ensure the directory exists
  dir.create(file.path("model_output", experiment), recursive = TRUE, showWarnings = FALSE)
  
  # Save to CSV
  write.csv(summary_stats, 
            file = paste0("model_output/", experiment, "/", set, "_bootstrap_metrics.csv"),
            row.names = FALSE)
  
  return(summary_stats)
}

# Example usage:
# analyze_bootstrap_metrics(bootstrap_results$train_metrics, "experiment_name")