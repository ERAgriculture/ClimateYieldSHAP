library(lubridate)

calculate_growing_days <- function(start_date, end_date) {
  # Convert strings to Date objects if they aren't already
  start <- as_date(start_date)
  end <- as_date(end_date)
  
  # Calculate the difference in days
  days_difference <- as.numeric(end - start)
  
  return(days_difference)
}

# # Example usage:
# start <- "2023-01-01"
# end <- "2023-12-31"
# days <- calculate_days(start, end)
# print(paste("Number of days between", start, "and", end, "is:", days))