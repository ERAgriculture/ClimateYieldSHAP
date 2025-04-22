library(dplyr)
library(lubridate)
source("R/convert_dates.R")

process_date_data <- function(data, start_columns, end_columns, date_format = "%Y-%m-%d") {
  
  # Ensure start_columns and end_columns are of equal length
  if (length(start_columns) != length(end_columns)) {
    stop("start_columns and end_columns must have the same length")
  }
  
  convert_dates <- function(date_string, format = "%Y-%m-%d", output_type = "formatted") {
    if (all(suppressWarnings(!is.na(as.numeric(date_string))))) {
      parsed_date <- as.Date(as.numeric(date_string), origin = "1899-12-30")
    } else {
      parsed_date <- as.Date(rep(NA, length(date_string)))
      dmy_dates <- suppressWarnings(dmy(date_string))
      parsed_date[!is.na(dmy_dates)] <- dmy_dates[!is.na(dmy_dates)]
      is_still_na <- is.na(parsed_date)
      serial_dates <- suppressWarnings(as.Date(as.numeric(date_string[is_still_na]), origin = "1899-12-30"))
      parsed_date[is_still_na] <- serial_dates
    }
    
    if (output_type == "serial") {
      return(as.numeric(parsed_date - as.Date("1899-12-30")))
    } else {
      return(format(parsed_date, format))
    }
  }
  
  # Process the data
  processed_data <- data %>%
    mutate(across(c(all_of(start_columns), all_of(end_columns)),
                  ~ convert_dates(., output_type = "serial"),
                  .names = "{.col}_serial")) %>%
    mutate(estimated_start_date = rowMeans(select(., ends_with("_serial")) %>% 
                                             select(starts_with(start_columns)), na.rm = TRUE),
           estimated_end_date = rowMeans(select(., ends_with("_serial")) %>% 
                                           select(starts_with(end_columns)), na.rm = TRUE)) %>%
    mutate(growing_days = estimated_end_date - estimated_start_date) %>%
    mutate(across(c(estimated_start_date, estimated_end_date),
                  ~ convert_dates(., format = date_format, output_type = "formatted"),
                  .names = "{.col}_formatted")) %>%
    select(-ends_with("_serial"))  # Remove the temporary serial columns
  
  # Generate and print summary statistics
  summary(processed_data$growing_days)
  hist(processed_data$growing_days, main = "Histogram of Growing Days",
       xlab = "Growing Days")
  
  processed_data <- processed_data %>%
    mutate(estimated_start_date = estimated_start_date_formatted,
           estimated_end_date = estimated_end_date_formatted
           )
  processed_data$estimated_start_date[processed_data$estimated_start_date == "NaN"] <- NA
  processed_data$estimated_end_date[processed_data$estimated_end_date == "NaN"] <- NA
  processed_data$growing_days[processed_data$growing_days == "NaN"] <- NA
  
  processed_data$estimated_start_date_formatted <- NULL
  processed_data$estimated_end_date_formatted <- NULL
  
  return(processed_data)
}