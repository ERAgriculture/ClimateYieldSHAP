# Plots the spatial distribution and yield distribution across AEZs.
# - Filters and orders AEZs with custom color mapping.
# - Creates a map with site locations and a violin plot of yields per AEZ.
# - Combines both plots into a single figure with patchwork-style layout.
# - Saves the output as PDF and PNG to the 'final_results' folder.
# Returns the combined ggplot object.

plot_aez_distribution <- function(data, experiment) {
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  library(sf)
  library(rnaturalearth)
  library(stringr)
  library(cowplot)
  
  # Set the specific order for AEZs as requested
  AEZs <- c("Warm Semiarid", "Warm Subhumid", "Warm Humid", 
            "Cool Semiarid", "Cool Subhumid", "Cool Humid")
  
  data <- data %>%
    mutate(AEZ16simple = str_replace_all(AEZ16simple, "\\.", " ")) %>%
    filter(AEZ16simple %in% AEZs) %>%
    # Apply the exact order specified
    mutate(AEZ16simple = factor(AEZ16simple, levels = AEZs))
  
  site_data <- data %>% distinct(Longitude, Latitude, AEZ16simple)
  
  lon_range <- range(site_data$Longitude, na.rm = TRUE)
  lat_range <- range(site_data$Latitude, na.rm = TRUE)
  lon_padding <- (lon_range[2] - lon_range[1]) * 0.05
  lat_padding <- (lat_range[2] - lat_range[1]) * 0.05
  
  map_xlim <- c(lon_range[1] - lon_padding, lon_range[2] + lon_padding)
  map_ylim <- c(lat_range[1] - lat_padding, lat_range[2] + lat_padding)
  
  # Define colors with the same order as AEZs
  aez_colors <- c(
    "Warm Semiarid" = "#EE7942", 
    "Warm Subhumid" = "#CD4F39",
    "Warm Humid" = "#8B1A1A",    
    "Cool Semiarid" = "#63B8FF",  
    "Cool Subhumid" = "#436EEE",  
    "Cool Humid" = "#27408B" 
  )
  
  africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")
  lakes <- ne_download(scale = "medium", type = "lakes", category = "physical", returnclass = "sf")
  african_lakes <- lakes %>%
    st_make_valid() %>%
    st_intersection(st_make_valid(africa))
  
  # Split AEZs into warm and cool for legend organization
  warm_aezs <- c("Warm Semiarid", "Warm Subhumid", "Warm Humid")
  cool_aezs <- c("Cool Semiarid", "Cool Subhumid", "Cool Humid")
  
  map_plot <- ggplot() +
    geom_sf(data = africa, fill = "white", color = "gray40", size = 0.2) +
    geom_sf(data = african_lakes, fill = "#B0E0E6", color = "#B0E0E6") +
    geom_point(data = site_data, 
               aes(x = Longitude, y = Latitude, color = AEZ16simple, fill = AEZ16simple),
               shape = 21, 
               size = 1,
               alpha = 0.35,
               stroke = 0.5) +
    # Use the breaks parameter to ensure legend items appear in the specified order
    scale_color_manual(values = aez_colors, breaks = AEZs) +
    scale_fill_manual(values = aez_colors, breaks = AEZs) +
    coord_sf(xlim = map_xlim, ylim = map_ylim) +
    theme_minimal() +
    theme(
      text = element_text(family = "Helvetica", size = 6.5),
      legend.position = "bottom",
      legend.margin = margin(4, 4, 2, 2),
      legend.title = element_text(face = "bold", hjust = 0.5),
      legend.title.position = "top",
      # Set legend to wrap with 3 items per row
      legend.nrow = 2,
      # Arrange the first row with Warm zones and second row with Cool zones
      legend.byrow = TRUE,
      legend.spacing.x = unit(0.1, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.box.spacing = unit(0.1, "cm"),
      legend.key.size = unit(0.5, "lines"),
      # Remove the black box around the legend
      legend.box.background = element_blank(),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.margin = margin(0, 0, 0, 0)
    ) + 
    labs(color = "Agroecological Zone",
         fill = "Agroecological Zone")
  
  
  violin_plot <- data %>%
    ggplot(aes(x = AEZ16simple, y = yield, fill = AEZ16simple)) +
    geom_violin(trim = FALSE, alpha = 0.8) +
    geom_boxplot(width = 0.1, alpha = 0.8, color = "black") +
    # Same here - ensure consistent order in the legend
    scale_fill_manual(values = aez_colors, breaks = AEZs) +
    # Add more y-axis ticks
    scale_y_continuous(breaks = seq(0, ceiling(max(data$yield, na.rm = TRUE)), by = 2500)) +
    scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
    labs(x = NULL, y = "Yield (kg/ha)") +
    theme_minimal() +
    theme(
      text = element_text(family = "Helvetica", size = 6.5),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  
  map_plot <- map_plot + 
    labs(subtitle = "a)") +
    theme(plot.subtitle = element_text(face = "bold", hjust = 0))
  
  violin_plot <- violin_plot + 
    labs(subtitle = "b)") +
    theme(plot.subtitle = element_text(face = "bold", hjust = 0))
  
  
  base_plot <- ggplot() + 
    theme_void() + 
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Convert plots to grobs
  map_grob <- ggplotGrob(map_plot)
  violin_grob <- ggplotGrob(violin_plot)
  
  # Place grobs at exact coordinates
  combined_plot <- base_plot +
    annotation_custom(
      map_grob,
      xmin = 0, xmax = 0.6, ymin = 0, ymax = 1
    ) +
    annotation_custom(
      violin_grob,
      xmin = 0.61, xmax = 1, ymin = 0, ymax = 1
    )
  
  
  dir.create("final_results", showWarnings = FALSE)
  
  ggsave(
    filename = paste0("final_results/", experiment, "_AEZ_distribution.pdf"),
    plot = combined_plot,
    width = 180,
    height = 85,
    dpi = 600,
    units = "mm"
  )
  
  ggsave(
    filename = paste0("final_results/", experiment, "_AEZ_distribution.png"),
    plot = combined_plot,
    width = 180,
    height = 85,
    dpi = 300,
    units = "mm"
  )
  
  return(combined_plot)
}