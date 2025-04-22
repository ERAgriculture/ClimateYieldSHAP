library(tidyverse)

climate_hierarchy <- function(df, w_name_1, w_name_2, w_name_3) {
  # Function to extract and remove observations
  extract_and_remove <- function(dataset, w_name) {
    extracted <- dataset %>%
      filter(W.Name == w_name)
    
    remaining <- dataset %>%
      anti_join(extracted %>% dplyr::select(ID, M.Year, Season, EU), 
                by = c("ID", "M.Year", "Season", "EU"))
    
    return(list(extracted = extracted, remaining = remaining))
  }
  
  # Steps 1-3: Extract observations for each W.Name
  result_1 <- extract_and_remove(df, w_name_1)
  observations_1 <- result_1$extracted
  df <- result_1$remaining
  
  result_2 <- extract_and_remove(df, w_name_2)
  observations_2 <- result_2$extracted
  df <- result_2$remaining
  
  result_3 <- extract_and_remove(df, w_name_3)
  observations_3 <- result_3$extracted
  
  # Combine all extracted observations
  result <- bind_rows(observations_1, observations_2, observations_3)
  result <- unique(result)
    
  # Identify numeric and non-numeric columns
  numeric_cols <- names(result)[sapply(result, is.numeric)]
  non_numeric_cols <- setdiff(names(result), c("ID", "M.Year", "Season", "EU", "W.Name", numeric_cols))

  # Calculate average over observations with identical ID, M.Year, Season, EU, and W.Name
  result_averaged <- result %>%
    group_by(ID, M.Year, Season, EU, W.Name) %>%
    summarise(across(all_of(numeric_cols), ~ mean(.x, na.rm = TRUE)),
              across(all_of(non_numeric_cols), ~ first(na.omit(.x))),
              .groups = "drop")
 
  return(result_averaged)
}