map_to_usda_texture <- function(texture) {
  texture <- tolower(texture)
  
  # Direct mappings
  direct_map <- c(
    "sand" = "sand",
    "loamy sand" = "loamy sand",
    "sandy loam" = "sandy loam",
    "loam" = "loam",
    "silt loam" = "silt loam",
    "sandy clay loam" = "sandy clay loam",
    "clay loam" = "clay loam",
    "sandy clay" = "sandy clay",
    "clay" = "clay"
  )
  
  # Check for direct matches first
  if (texture %in% names(direct_map)) {
    return(direct_map[texture])
  }
  
  # Handle special cases and approximations
  if (grepl("fine loamy|loamy fine|loamy", texture)) return("loam")
  if (grepl("fine sand", texture)) return("sand")
  if (grepl("sandy silt|silty loam", texture)) return("silt loam")
  if (grepl("sandy.sandy loam|sandy loamy", texture)) return("sandy loam")
  if (grepl("loamy clay|clayey", texture)) return("clay loam")
  
  # For mixed or uncertain classifications, choose the first valid option
  if (grepl("/", texture)) {
    options <- strsplit(texture, "/")[[1]]
    for (opt in options) {
      mapped <- map_to_usda_texture(trimws(opt))
      if (!is.na(mapped)) return(mapped)
    }
  }
  
  # If no match found, return NA
  return(NA)
}