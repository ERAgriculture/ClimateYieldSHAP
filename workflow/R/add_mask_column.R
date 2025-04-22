add_mask_column <- function(data, pattern, new_column_name, true_value, 
                            false_value="No ") {
  library(dplyr)
  library(stringr)
  
  # Define the specific columns to check
  columns_to_check <- c("T1", "T2", "T3", "T4", "T5", "T6", "T7")
  
  # Create the new column
  data <- data %>%
    mutate(!!new_column_name := case_when(
      rowSums(across(all_of(columns_to_check), 
                     ~str_detect(as.character(.), pattern))) > 0 ~ true_value,
      TRUE ~ paste0(false_value, true_value)
    ))
  
  return(data)
}

# Example usage:
# patterns <- c(
#   "^a",
#   "^b1[12]\\.[12]",
#   "^b(1[6-9]|2[0-3])",
#   "^b27",
#   "^b7[01]"
# )
