library(dplyr)

# Helper function to handle missing Rep values and create Used_Rep
handle_rep <- function(data, max_rep = 5) {
  data %>%
    mutate(
      Rep = if_else(is.na(Rep), 1, Rep),
      Used_Rep = pmin(Rep, max_rep)
    )
}

# Inverse weighting function
inverse_weight <- function(data, column) {
  data <- handle_rep(data)
  
  counts <- data %>%
    count(!!sym(column))
  
  data %>%
    left_join(counts, by = column) %>%
    mutate(case_weight = (1 / n) * Used_Rep/2) %>%
    pull(case_weight)
}

# Log inverse weighting function
log_inverse_weight <- function(data, column) {
  data <- handle_rep(data)
  
  counts <- data %>%
    count(!!sym(column))
  
  data %>%
    left_join(counts, by = column) %>%
    mutate(case_weight = (1 / log(n + 1)) * Used_Rep/2) %>%
    pull(case_weight)
}

# Square root inverse weighting function
sqrt_inverse_weight <- function(data, column) {
  data <- handle_rep(data)
  
  counts <- data %>%
    count(!!sym(column))
  
  data %>%
    left_join(counts, by = column) %>%
    mutate(case_weight = (1 / sqrt(n)) * Used_Rep/2) %>%
    pull(case_weight)
}

# Main function to assign weight function based on experiment name
assign_weight_function <- function(weight_type) {
  # Extract the part after the first underscore
  
  if (is.na(weight_type)) {
    return(NULL)  # Return NULL if no weight type specified
  }
  
  # Assign weight function based on type
  weight_func <- switch(weight_type,
                        "inv" = inverse_weight,
                        "ilog" = log_inverse_weight,
                        "isqrt" = sqrt_inverse_weight,
                        NULL  # Default case if no match
  )
  return(weight_func)
}