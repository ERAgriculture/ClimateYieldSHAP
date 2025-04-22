library(dplyr)
library(lubridate)

convert_dates <- function(date_string, format = "%Y-%m-%d", output_type = "formatted", n_days = 0) {
  # Check if all inputs are numeric (potential serial dates)
  if (all(suppressWarnings(!is.na(as.numeric(date_string))))) {
    # If all are numeric, treat them as serial dates
    parsed_date <- as.Date(as.numeric(date_string), origin = "1899-12-30")
  } else {
    # Initialize with NA dates
    parsed_date <- as.Date(rep(NA, length(date_string)))
    
    # Try to parse as DD/MM/YYYY
    dmy_dates <- suppressWarnings(dmy(date_string))
    parsed_date[!is.na(dmy_dates)] <- dmy_dates[!is.na(dmy_dates)]
    
    # For remaining NAs, try to parse as Date serial numbers
    is_still_na <- is.na(parsed_date)
    serial_dates <- suppressWarnings(as.Date(as.numeric(date_string[is_still_na]), origin = "1899-12-30"))
    parsed_date[is_still_na] <- serial_dates
  }
  
  # Add the specified number of days
  parsed_date <- parsed_date + n_days
  
  # Return the result based on output_type
  if (output_type == "serial") {
    return(as.numeric(parsed_date - as.Date("1899-12-30")))
  } else {
    return(format(parsed_date, format))
  }
}

process_date_data <- function(data, start_columns, end_columns, date_format = "%Y-%m-%d", n_days_start = 0, n_days_end = 0) {
  
  # Ensure start_columns and end_columns are of equal length
  if (length(start_columns) != length(end_columns)) {
    stop("start_columns and end_columns must have the same length")
  }
  
  # Process the data
  processed_data <- data %>%
    mutate(across(all_of(start_columns),
                  ~ convert_dates(., output_type = "serial", n_days = n_days_start),
                  .names = "{.col}_serial")) %>%
    mutate(across(all_of(end_columns),
                  ~ convert_dates(., output_type = "serial", n_days = n_days_end),
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
           estimated_end_date = estimated_end_date_formatted)
  
  processed_data$estimated_start_date[processed_data$estimated_start_date == "NaN"] <- NA
  processed_data$estimated_end_date[processed_data$estimated_end_date == "NaN"] <- NA
  processed_data$growing_days[processed_data$growing_days == "NaN"] <- NA
  
  processed_data$estimated_start_date_formatted <- NULL
  processed_data$estimated_end_date_formatted <- NULL
  
  return(processed_data)
}