require("tidyverse")
require("fs")

process_test_metrics <- function(directory) {
  # Get all CSV files in the directory
  csv_files <- dir_ls(directory, glob = "*_metrics.csv")
  
  # Function to read and process each file
  read_metrics <- function(file) {
    model_name <- str_remove(basename(file), "_metrics\\.csv$")
    
    read_csv(file) %>%
      spread(.metric, .estimate) %>%
      mutate(model = model_name) %>%
      select(model, everything())
  }
  
  # Read and combine all files
  combined_results <- map_dfr(csv_files, read_metrics)
  
  return(combined_results)
}

