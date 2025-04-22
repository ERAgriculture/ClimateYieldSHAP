estimate_start_date_from_season <- function(year, season1, season2, n_days_start, growing_days) {
  tryCatch({
    # Extract year and season information
    year_part <- as.integer(sub("\\..*", "", year))
    season_part <- sub(".*\\.", "", year)
    
    # Determine which season to use
    if (season_part == "1" || season_part == year) {
      season <- season1
    } else if (season_part == "2") {
      season <- season2
    } else {
      return(NA_character_)  # Invalid season
    }
    
    # If season is NA, return NA
    if (is.na(season)) {
      return(NA_character_)
    }
    
    # Convert month names to numbers
    month_map <- c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6,
                   "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)
    
    # Extract month from season
    month <- substr(season, nchar(season) - 2, nchar(season))
    month_num <- month_map[month]
    
    # Handle special cases
    if (grepl("Early", season)) {
      day <- 7
    } else if (grepl("Mid", season)) {
      day <- 15
    } else if (grepl("Late", season)) {
      day <- 22
    } else {
      day <- 15
    }
    
    # Create date and adjust it
    date <- as.Date(sprintf("%04d-%02d-%02d", year_part, month_num, day))
    adjusted_date <- date + as.integer(n_days_start)
    
    # This is to account for seasons late in the year and harvest happening the 
    # year after.
    # Check if the adjusted date is less than 130 days from the start of the next year
    next_year_start <- as.Date(sprintf("%04d-01-01", year_part + 1))
    if (as.integer(next_year_start - adjusted_date) < growing_days) {
      adjusted_date <- adjusted_date - 365  # Subtract one year
    }
    
    # Return the adjusted date as a character string
    format(adjusted_date, "%Y-%m-%d")
  }, error = function(e) {
    NA_character_  # Return NA if any error occurs
  })
}
