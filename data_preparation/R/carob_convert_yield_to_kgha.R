carob_convert_yield_to_kgha <- function(df, yield_col, units_col, humidity_percent = 15) {
  # Conversion factors to kg/ha
  conv_factors <- c(
    "kg/ha" = 1,
    "t/ha" = 1000,
    "Mg/ha" = 1000,
    "Kg/ha" = 1,
    "kg/acre" = 2.47105,
    "Kg/acre" = 2.47105,
    "ton/ha" = 1000,
    "g/m2" = 10,
    "kg/ha/year" = 1
  )
  
  # Function to clean and extract units
  clean_units <- function(unit) {
    # Check whether DM is in the string
    has_dm <- grepl("DM", unit, ignore.case = TRUE)
    
    # Remove DM from the string
    unit <- gsub("DM", "", unit, ignore.case = TRUE)
    
    # Remove all whitespaces from the string
    unit <- gsub("\\s", "", unit)
    
    # Split the string using ".." as delimitation
    parts <- strsplit(unit, "\\.\\.")[[1]]
    
    # Extract the unit (second object)
    clean_unit <- if (length(parts) >= 2) parts[2] else unit
    
    list(unit = clean_unit, has_dm = has_dm, original = unit)
  }
  
  # Function to convert yield
  convert_yield <- function(yield, unit) {
    if (is.na(yield)) return(NA_real_)
    
    cleaned <- clean_units(unit)
    clean_unit <- cleaned$unit
    has_dm <- cleaned$has_dm
    original_unit <- cleaned$original
    
    if (clean_unit %in% names(conv_factors)) {
      converted <- yield * conv_factors[clean_unit]
    } else {
      warning(paste("Unknown unit:", clean_unit, "from original:", original_unit))
      return(yield)  # Return original yield
    }
    
    if (has_dm) {
      converted <- converted / (1 - humidity_percent/100)
    }
    
    return(converted)
  }
  
  # Apply conversions
  df$estimated_yield <- mapply(convert_yield, df[[yield_col]], df[[units_col]])
  df$outcome_units <- "kg/ha"
  
  return(df)
}