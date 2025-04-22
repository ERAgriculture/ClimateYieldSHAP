library(dplyr)
library(lubridate)

convert_dates <- function(date_string, format = "%Y-%m-%d", output_type = "formatted") {
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
  
  # Return the result based on output_type
  if (output_type == "serial") {
    return(as.numeric(parsed_date - as.Date("1899-12-30")))
  } else {
    return(format(parsed_date, format))
  }
}
