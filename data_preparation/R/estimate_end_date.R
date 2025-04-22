require("tidyverse")
require("lubridate")

estimate_end_date <- function(df,
                                  harvest_date_col,
                                  planting_date_col,
                                  days_to_harvest = 130,
                                  n_days_end, 
                                  end_month = 12) {
  df <- df %>%
    mutate({{ harvest_date_col }} := as.character(!!sym(harvest_date_col)))
  
  df <- df %>%
    mutate(
      estimated_end_date = case_when(
        nchar(!!sym(harvest_date_col)) == 10 ~ as.character(as.Date(!!sym(harvest_date_col)) + n_days_end),
        nchar(!!sym(harvest_date_col)) == 7 ~ as.character(as.Date(paste0(!!sym(harvest_date_col), "-01")) + n_days_end),
        nchar(!!sym(harvest_date_col)) == 4 ~ as.character(as.Date(!!sym(planting_date_col)) + days_to_harvest + n_days_end),
        TRUE ~ NA_character_
      )
    )
  
  na_indices <- is.na(df$estimated_end_date)
  
  df$estimated_end_date[na_indices] <- as.character(as.Date(df[[planting_date_col]][na_indices]) + n_days_end + days_to_harvest)
  
  return(df)
}