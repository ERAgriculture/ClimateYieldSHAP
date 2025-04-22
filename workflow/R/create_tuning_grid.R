require("tidymodels")

# Create tuning grid
create_tuning_grid <- function(tune_params) {
  # Create a list of vectors
  param_list <- lapply(tune_params, unlist)
  
  # Use expand.grid to create all combinations
  grid <- expand.grid(param_list, stringsAsFactors = FALSE)
  
  # Ensure all columns are numeric
  grid[] <- lapply(grid, as.numeric)
  
  return(grid)
}
