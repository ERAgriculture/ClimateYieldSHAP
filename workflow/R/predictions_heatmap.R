library(tidymodels)
library(ggplot2)

create_heatmap <- function(workflow, feature1, feature2, data, n = 10) {
  # Create a grid of values
  grid <- expand.grid(
    f1 = seq(min(data[[feature1]]), max(data[[feature1]]), length.out = n),
    f2 = seq(min(data[[feature2]]), max(data[[feature2]]), length.out = n)
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
  
  # Discretize predictions
  grid$.pred_cat <- cut(grid$.pred, breaks = 5)
  
  # Create the heat map
  ggplot(grid, aes(x = .data[[feature1]], y = .data[[feature2]], fill = .pred_cat)) +
    geom_tile() +
    scale_fill_viridis_d() +
    theme_minimal() +
    labs(title = "Predicted Yield Heatmap",
         x = feature1, y = feature2, fill = "Predicted Yield")
}
