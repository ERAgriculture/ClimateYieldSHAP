require("tidyverse")
require("lubridate")

estimate_start_date <- function(df, 
                                   planting_date_col, 
                                   n_days_start = -15, 
                                   start_month = 1) {
  df <- df %>%
    mutate({{ planting_date_col }} := as.character(!!sym(planting_date_col)))
  
  df <- df %>%
    mutate(
      estimated_start_date = case_when(
        nchar(!!sym(planting_date_col)) == 10 ~ as.character(as.Date(!!sym(planting_date_col)) + n_days_start),
        nchar(!!sym(planting_date_col)) == 4 ~ as.character(as.Date(paste0(!!sym(planting_date_col), "-", start_month, "-01"))),
        nchar(!!sym(planting_date_col)) == 7 ~ as.character(as.Date(paste0(!!sym(planting_date_col), "-01")) + n_days_start),
        TRUE ~ as.character(!!sym(planting_date_col))
      )
    )
  df <- df %>%
    mutate(estimated_start_date = as.character(estimated_start_date))
  
  return(df)
}

