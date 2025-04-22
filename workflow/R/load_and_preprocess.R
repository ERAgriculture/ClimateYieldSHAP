require("tidyverse")
require("janitor")
require("naniar")
require("caret")
library(arrow)

load_and_preprocess <- function(data_path, 
                                target_col,
                                cols_to_remove = NULL,
                                imputation_method = NULL,
                                lower_quantile = 0,
                                upper_quantile = 1) {
  
  # Load data silently
  data <- suppressMessages(read_parquet(data_path))
  
  # Clean column names
  # data <- clean_names(data)
  
  # Remove specified columns
  if (!is.null(cols_to_remove)) {
    data <- data %>% select(-all_of(cols_to_remove))
  }
  
  # Remove rows with missing values in the target column
  data <- data %>%
    filter(!is.na(!!sym(target_col)))
  
  # Remove outliers in the target variable based on quantiles
  target_quantiles <- quantile(data[[target_col]], probs = c(lower_quantile, upper_quantile), na.rm = TRUE)
  data <- data %>%
    filter(!!sym(target_col) >= target_quantiles[1] & !!sym(target_col) <= target_quantiles[2])
  
  # Identify numeric, categorical, logical, and factor columns
  numeric_cols <- names(select(data, where(is.numeric)))
  categorical_cols <- names(select(data, where(is.character)))
  logical_cols <- names(select(data, where(is.logical)))
  factor_cols <- names(select(data, where(is.factor)))
  
  # Convert logical columns to factors
  data <- data %>%
    mutate(across(all_of(logical_cols), ~as.factor(.x)))
  
  # Handle missing values in numeric columns
  if (imputation_method == "median") {
    data <- data %>%
      mutate(across(all_of(numeric_cols), ~if_else(is.na(.x), median(.x, na.rm = TRUE), .x)))
  } else if (imputation_method == "mean") {
    data <- data %>%
      mutate(across(all_of(numeric_cols), ~if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)))
  } else if (imputation_method == "mode") {
    mode_impute <- function(x) {
      tab <- table(x)
      if(all(is.na(x))) return(NA)
      as.numeric(names(tab)[which.max(tab)])
    }
    data <- data %>%
      mutate(across(all_of(numeric_cols), ~if_else(is.na(.x), mode_impute(.x), .x)))
  }
  
  # Handle missing values in categorical, logical, and factor columns
  data <- data %>%
    mutate(across(c(all_of(categorical_cols), all_of(logical_cols), all_of(factor_cols)), ~if_else(is.na(.x), "Unknown", .x)))
  
  # Check for near-zero variance predictors (only for numeric columns)
  numeric_data <- data %>% select(all_of(numeric_cols))
  near_zero_var <- nearZeroVar(numeric_data, saveMetrics = TRUE)
  nzv_cols <- rownames(near_zero_var)[near_zero_var$nzv]
  
  # Check for any remaining missing values
  missing_data <- sapply(data, function(x) sum(is.na(x)))
  cols_with_missing <- names(missing_data)[missing_data > 0]
  
  # Print summary
  cat("\nData Preprocessing Summary:\n")
  cat("----------------------------\n")
  
  if (length(cols_with_missing) > 0) {
    cat("\n1. Columns with missing values after imputation:\n")
    for (col in cols_with_missing) {
      cat(sprintf("   - %s: %d missing values\n", col, missing_data[col]))
    }
  } else {
    cat("\n1. No missing values remain in the dataset.\n")
  }
  
  if (length(nzv_cols) > 0) {
    cat("\n2. Near-zero variance predictors detected (numeric only):\n")
    for (col in nzv_cols) {
      cat(sprintf("   - %s\n", col))
    }
    cat("   Consider removing these predictors.\n")
  } else {
    cat("\n2. No near-zero variance predictors detected among numeric variables.\n")
  }
  
  return(data)
}