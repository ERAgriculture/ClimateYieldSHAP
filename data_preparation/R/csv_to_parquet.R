# Install and load required packages
if (!requireNamespace("arrow", quietly = TRUE)) install.packages("arrow")
library(arrow)
library(dplyr)

# Function to convert CSV to Parquet
csv_to_parquet <- function(csv_file, parquet_file) {
  # Read CSV file
  data <- read_csv_arrow(csv_file)
  
  # Write to Parquet file
  write_parquet(data, parquet_file)
  
  # Print file sizes for comparison
  csv_size <- file.size(csv_file) / 1e6  # Size in MB
  parquet_size <- file.size(parquet_file) / 1e6  # Size in MB
  
  cat("CSV file size:", round(csv_size, 2), "MB\n")
  cat("Parquet file size:", round(parquet_size, 2), "MB\n")
  cat("Compression ratio:", round(csv_size / parquet_size, 2), "\n")
}

# Example usage
csv_to_parquet("../data/carob/carob_fertilizer-cc.csv",
               "../data/carob/carob_fertilizer-cc.parquet")

# To read the Parquet file later:
# parquet_data <- read_parquet("large_file.parquet")