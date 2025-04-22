# Normalize features and save normalization parameters

normalize_features <- function(data, features) {
  # Initialize with required columns
  normalized_data <- data.frame(
    N.obs = data$N.obs
  )
  
  norm_params <- data.frame(
    feature = character(),
    min = numeric(),
    max = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (feature in features) {
    # Skip if feature is factor or character
    if (is.factor(data[[feature]]) || is.character(data[[feature]])) {
      normalized_data[[paste0(feature, "_norm")]] <- data[[feature]]
      next
    }
    
    feat_min <- min(data[[feature]], na.rm = TRUE)
    feat_max <- max(data[[feature]], na.rm = TRUE)
    
    norm_params <- rbind(norm_params, 
                         data.frame(
                           feature = feature,
                           min = feat_min,
                           max = feat_max,
                           stringsAsFactors = FALSE
                         ))
    
    normalized_data[[paste0(feature, "_norm")]] <- 
      (data[[feature]] - feat_min) / (feat_max - feat_min)
  }
  
  result <- list(
    normalized_data = normalized_data,
    norm_params = norm_params
  )
  
  saveRDS(result, file = 'outputs/feature_normalization.rds')
  
  return(result)
}