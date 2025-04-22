predictions_contour <- function(workflow, feature1, feature2, data, n = 100) {
  library(tidymodels)
  library(ggplot2)
  
  predictor_list <- as.list(workflow$pre$actions$recipe$recipe$var_info %>%
                              filter(role == "predictor") %>%
                              select(variable))[[1]]
  
  data <- data %>% 
    select(all_of(predictor_list))
  
  # Create a grid of values for the two features
  grid <- expand.grid(
    feature1 = seq(min(data[[feature1]]), max(data[[feature1]]), length.out = n),
    feature2 = seq(min(data[[feature2]]), max(data[[feature2]]), length.out = n)
  )
  names(grid) <- c(feature1, feature2)
  
  # Add mean values for other features
  other_features <- setdiff(names(data), c(feature1, feature2, "yield"))
  for (feat in other_features) {
    grid[[feat]] <- mean(data[[feat]])
  }
  
  # Make predictions
  predictions <- predict(workflow, new_data = grid)
  grid$.pred <- predictions$.pred
  
  # Create the contour plot with observed data points
  p <- ggplot() +
    geom_contour_filled(data = grid, aes(x = .data[[feature1]], y = .data[[feature2]], z = .pred)) +
    geom_point(data = data, aes(x = .data[[feature1]], y = .data[[feature2]]), 
               shape = 3, size = 1, alpha = 0.5) +  # Add scatter plot with '+' shape
    scale_fill_viridis_d(option = "plasma") +
    labs(
      title = paste("Contour Plot of Predicted Yield with Observed Data"),
      x = feature1,
      y = feature2,
      fill = "Predicted Yield"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),  # Set white background
      plot.background = element_rect(fill = "white", colour = "white")    # Set plot background
    )
  
  return(p)
}
