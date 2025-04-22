library(dplyr)
library(tidyr)
library(purrr)

summarize_boots_performance <- function(full_results, bootstrap_results, experiment, set) {
  # Convert list of tibbles to a single dataframe
  set_metrics <- paste0(set, "_metrics")
  all_metrics <- bind_rows(bootstrap_results[[set_metrics]])
  
  all_metrics <- all_metrics %>%
    mutate(.metric = paste0("boots_", .metric))
  
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
  
  # Get the full results for the specified set and add "full_" prefix
  full_set_results <- full_results[[set_metrics]] %>%
    mutate(.metric = paste0("full_", .metric)) %>%
    rename(
      mean = .estimate
    ) %>% 
    select(!.estimator)
  
  combined_stats <- bind_rows(full_set_results, summary_stats)
  
  
  # Ensure the directory exists
  dir.create(file.path("model_output", experiment), recursive = TRUE, showWarnings = FALSE)
  
  # Save to CSV
  write.csv(combined_stats, 
            file = paste0("model_output/", experiment, "/", set, "_bootstrap_metrics.csv"),
            row.names = FALSE)
  
  return(combined_stats)
}

# Example usage:
# summarize_boots_performance(full_results, bootstrap_results, "eco_isqrt_nosoil_Warm.Humid", "test")
