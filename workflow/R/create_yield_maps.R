create_yield_maps <- function(data, predictions, longitude, latitude, actual_yield, output_dir) {
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  
  # Create output directory without asking
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Combine data and predictions
  plot_data <- cbind(data, predicted_yield = predictions$.pred)
  
  # Convert to sf object
  plot_data_sf <- st_as_sf(plot_data, coords = c(longitude, latitude), crs = 4326)
  
  # Get world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Set common theme for all plots
  map_theme <- theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.key.width = unit(1, "cm")
    )
  
  # Custom color scale
  custom_scale <- scale_color_gradientn(
    colors = c("red", "yellow", "blue"),
    name = "Yield"
  )
  
  # 1. Predicted Yield Map
  pred_map <- ggplot() +
    geom_sf(data = world, fill = "white", color = "gray") +
    geom_sf(data = plot_data_sf, aes(color = predicted_yield)) +
    coord_sf(xlim = range(plot_data[[longitude]]), ylim = range(plot_data[[latitude]])) +
    custom_scale +
    labs(title = "Predicted Yield") +
    map_theme
  
  # 2. Actual Yield Map
  actual_map <- ggplot() +
    geom_sf(data = world, fill = "white", color = "gray") +
    geom_sf(data = plot_data_sf, aes(color = .data[[actual_yield]])) +
    coord_sf(xlim = range(plot_data[[longitude]]), ylim = range(plot_data[[latitude]])) +
    custom_scale +
    labs(title = "Actual Yield") +
    map_theme
  
  # 3. Yield Prediction Error Map
  plot_data_sf$yield_error <- plot_data_sf$predicted_yield - plot_data_sf[[actual_yield]]
  error_map <- ggplot() +
    geom_sf(data = world, fill = "white", color = "gray") +
    geom_sf(data = plot_data_sf, aes(color = yield_error)) +
    coord_sf(xlim = range(plot_data[[longitude]]), ylim = range(plot_data[[latitude]])) +
    scale_color_gradient2(
      name = "Prediction error", 
      low = "red", mid = "yellow", high = "blue", 
      midpoint = 0
    ) +
    labs(title = "Prediction error") +
    map_theme
  
  # Save plots
  ggsave(file.path(output_dir, "predicted_yield_map.png"), pred_map, width = 10, height = 8)
  ggsave(file.path(output_dir, "actual_yield_map.png"), actual_map, width = 10, height = 8)
  ggsave(file.path(output_dir, "yield_error_map.png"), error_map, width = 10, height = 8)
  
  # Return list of plots
  return(list(predicted = pred_map, actual = actual_map, error = error_map))
}