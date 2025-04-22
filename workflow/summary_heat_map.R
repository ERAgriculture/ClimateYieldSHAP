library(ggnewscale)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(showtext)
library(ggtext)
library(purrr)
library(stringr)
library(tibble)

source('R/calculate_impact_range.R')

text_size <- 5

plot_type <- "GAMCI_shap_plot_"

experiment_t <- "no_outliers_eco_isqrt_nosoil"

AEZ_list <- c("Warm.Semiarid", "Warm.Subhumid", "Warm.Humid",
              "Cool.Semiarid", "Cool.Subhumid", "Cool.Humid")

feature_mapping <- c(
  "Rain.Days.L.1" = "Fraction_Rainy_Days",
  "Rain.sum" = "Total_Rainfall",
  "Rain.sum.Dev.Mean" = "Total_Rainfall_Dev",
  "Tmax.mean" = "Avg_Max_Temp",
  "Rain.Days.L.1.Dev.Mean" = "Fraction_Rainy_Days_Dev",
  "Rain.Max.RSeq.5" = "Longest_Dry_Spell",
  "Tmax.mean.Dev.Mean" = "Avg_Max_Temp_Dev",
  "yield" = "Yield"
)

feature_list <- unname(feature_mapping)[-8]

impact_results <- crossing(AEZ = AEZ_list, variable = feature_list) %>%
  mutate(
    experiment = paste0(experiment_t, "_", AEZ),
    full_results = map(experiment, ~readRDS(paste0('outputs/', .x, '/full_results/full_results.rds'))),
    mean_yield = map_dbl(full_results, ~mean(.x$shap_values$Yield)),
    effect = map2(full_results, variable, ~calculate_impact_range(
      x_values = .x$shap_values %>% pull(.y),
      y_values = .x$shap_values %>% pull(paste0("SHAP_", .y)),
      noise_threshold = 0.3,
      window_size = 0.1,
      min_dev_explained = 0.2
    )),
    impact_range = map_dbl(effect, ~as.numeric(.x$impact_range)),
    deviance_explained = map_dbl(effect, ~.x$deviance_explained),
    noise_level = map_dbl(effect, ~.x$noise_level)
  ) %>%
  select(AEZ, variable, impact_range, deviance_explained, noise_level, mean_yield) %>% 
  mutate(norm_impact = impact_range/mean_yield)


# write.csv(impact_results, file = paste0("final_results/", experiment_t, "_summary_table.csv"))
# "Fraction_Rainy_Days", "Total_Rainfall", "Total_Rainfall_Dev", "Avg_Max_Temp", 
# "Fraction_Rainy_Days_Dev", "Longest_Dry_Spell", "Avg_Max_Temp_Dev"       

df_relationship_all <- data.frame("Warm.Semiarid" = c("Inverse-U", "Positive", "Positive", "Negative", 
                                                      " ", "Inverse-U", "Negative"),
                                  "Warm.Subhumid" = c("Inverse-U", "Inverse-U", "Inverse-U", "Negative", 
                                                      " ", "Inverse-U", "Inverse-U"),
                                  "Warm.Humid" =    c(" ", "Negative", "Positive", " ", 
                                                      " ", "Inverse-U", "Inverse-U"),
                                  
                                  "Cool.Semiarid" = c("Negative", "Inverse-U", "Negative", "Negative", 
                                                      "Positive", "Negative", "Inverse-U"),
                                  "Cool.Subhumid" = c("Inverse-U", "Inverse-U", "Positive", "Negative", 
                                                      "Inverse-U", "Inverse-U", "Inverse-U"),
                                  "Cool.Humid" =    c(" ", " ", "Inverse-U", " ", 
                                                      "Negative", "Negative", "Negative")
)
row.names(df_relationship_all) = feature_list

# Reshape df_relationship_all to long format
relationships_long <- df_relationship_all %>%
  rownames_to_column("variable") %>%
  pivot_longer(
    cols = -variable,
    names_to = "AEZ",
    values_to = "relationship_type"
  )

# Merge with impact_results
df_combined <- impact_results %>%
  left_join(relationships_long, by = c("AEZ", "variable"))


# Create custom labels for relationship types
relationship_labels <- c(
  "Positive" = "\u2197",  # up arrow
  "Negative" = "\u2198",  # down arrow
  "Inverse-U" = "\u2229",   # inverse U
  "No relationship" = "-"
)

AEZ_order <- c("Warm.Semiarid", "Cool.Semiarid", 
               "Warm.Subhumid", "Cool.Subhumid", 
               "Warm.Humid", "Cool.Humid")
feature_order <- c("Fraction_Rainy_Days_Dev", "Fraction_Rainy_Days", "Longest_Dry_Spell", 
                   "Total_Rainfall_Dev", "Total_Rainfall", "Avg_Max_Temp_Dev", "Avg_Max_Temp")

df_combined <- df_combined %>%
  mutate(
    AEZ = factor(AEZ, levels = AEZ_order),
    variable = factor(variable, levels = feature_order)
  )

# First, create separate datasets for each relationship type
positive_data <- df_combined %>% filter(relationship_type == "Positive")
negative_data <- df_combined %>% filter(relationship_type == "Negative") 
inverse_u_data <- df_combined %>% filter(relationship_type == "Inverse-U")
flat_data <- df_combined %>% filter(relationship_type == " ")

# Find the maximum value across all three types
max_value <- max(df_combined$impact_range, na.rm = TRUE)

# Create the plot with reordered legends
p <- ggplot(df_combined, aes(x = AEZ, y = variable)) +
  # Base white background for all cells
  geom_tile(color = "white", fill = "white") +
  
  # Positive relationship tiles
  geom_tile(data = positive_data, 
            aes(fill = impact_range), 
            color = "white") +
  scale_fill_gradient("Positive (↗)", 
                      low = "#E6E6FF", high = "#1E3A8A",
                      limits = c(0, max_value),
                      breaks = seq(0, 2000, length.out = 3),
                      guide = guide_colorbar(title.position = "top", 
                                             direction = "horizontal",
                                             barwidth = 2.2,
                                             barheight = 0.4,
                                             order = 1,
                                             title.theme = element_text(size = 4),
                                             label.theme = element_text(size = 3)
                                             )) +
  
  # New scale for Negative
  new_scale_fill() +
  geom_tile(data = negative_data, 
            aes(fill = impact_range), 
            color = "white") +
  scale_fill_gradient("Negative (↘)", 
                      low = "#FFE6E6", high = "#B22222",
                      limits = c(0, max_value),
                      breaks = seq(0, 2000, length.out = 3),
                      guide = guide_colorbar(title.position = "top", 
                                             direction = "horizontal",
                                             barwidth = 2.2,
                                             barheight = 0.4,
                                             order = 2,
                                             title.theme = element_text(size = 4),
                                             label.theme = element_text(size = 3)
                                             )) +
  
  # New scale for Inverse-U
  new_scale_fill() +
  geom_tile(data = inverse_u_data, 
            aes(fill = impact_range), 
            color = "white") +
  scale_fill_gradient("Inverse-U (∩)", 
                      low = "#FFF6E6", high = "#B8860B",
                      limits = c(0, max_value),
                      breaks = seq(0, 2000, length.out = 3),
                      guide = guide_colorbar(title.position = "top", 
                                             direction = "horizontal",
                                             barwidth = 2.2,
                                             barheight = 0.4,
                                             order = 3,
                                             title.theme = element_text(size = 4),
                                             label.theme = element_text(size = 3)
                                             )) +
  
  
  new_scale_fill() +
  geom_tile(data = flat_data,
            aes(fill = " "),
            color = "white") +
  scale_fill_manual(values = c(" " = "white"),
                    name = "No relationship (-)",
                    guide = guide_legend(title.position = "top",
                                         order = 4,
                                         title.theme = element_text(size = 4),
                                         label.theme = element_text(size = 3)
                                         )) +
  
  # Add symbols
  geom_text(aes(label = case_when(
    relationship_type == "Positive" ~ "↗",
    relationship_type == "Negative" ~ "↘",
    relationship_type == "Inverse-U" ~ "∩",
    TRUE ~ "-"
  )), size = 5, family = "Arial Unicode MS") +
  
  # Axis labels with breaks
  scale_x_discrete(labels = function(x) gsub("\\.", "\n", x)) +
  
  # Fixed y-axis labels using a named vector approach
  scale_y_discrete(labels = c(
    "Fraction_Rainy_Days_Dev" = "Fraction Rainy\nDays Deviation",
    "Fraction_Rainy_Days" = "Fraction Rainy Days",
    "Longest_Dry_Spell" = "Longest Dry Spell",
    "Total_Rainfall_Dev" = "Total Rainfall Deviation",
    "Total_Rainfall" = "Total Rainfall",
    "Avg_Max_Temp_Dev" = "Average Maximum\nTemperature Deviation",
    "Avg_Max_Temp" = "Average Maximum\nTemperature"
  )) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Unicode MS"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.spacing.x = unit(0.2, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.title = element_text(family = "Helvetica", size = text_size),
    legend.text = element_text(family = "Helvetica", size = text_size),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 0.5,
                               family = "Helvetica", size = text_size),
    axis.text.y = element_text(family = "Helvetica", size = text_size, hjust = 0.5)
  ) +
  labs(
    title = "",
    x = "",
    y = ""
  )


print(p)


# Function to create and save PNG version with larger text and white background
save_png_version <- function(plot, filename, text_scale_factor = 10, symbol_scale_factor = 2) {
  # Create a copy of the plot
  png_plot <- plot
  
  # Access the text layer (symbols) and update its size
  if(length(png_plot$layers) >= 6) {
    # The 6th layer is the geom_text with symbols
    current_size <- png_plot$layers[[6]]$aes_params$size
    if(is.null(current_size)) {
      current_size <- 5  # Default from your original code
    }
    
    # Update the size parameter in the layer
    png_plot$layers[[6]]$aes_params$size <- current_size * symbol_scale_factor * 3
  }
  
  # Add theme modifications for text elements
  png_plot <- png_plot +
    theme(
      # Adjust axis text with tighter line spacing
      axis.text.x = element_text(hjust = 0.5, 
                                 family = "Helvetica", 
                                 size = text_size * text_scale_factor,
                                 lineheight = 0.4),  # Reduced line spacing
      axis.text.y = element_text(family = "Helvetica", 
                                 size = text_size * text_scale_factor, 
                                 hjust = 0.5,
                                 lineheight = 0.4),  # Reduced line spacing
      
      # Basic text elements
      legend.title = element_text(family = "Helvetica", size = text_size * text_scale_factor),
      legend.text = element_text(family = "Helvetica", size = text_size * text_scale_factor),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Save with larger dimensions
  ggsave(filename, 
         plot = png_plot,
         width = 88 * 3, 
         height = 88 * 3, 
         units = "mm",
         dpi = 300,
         bg = "white")
}

# Save the PDF (original size)
ggsave(paste0("final_results/", experiment_t, "_climate_yield.pdf"), 
       plot = p,
       width = 88, 
       height = 88, 
       units = "mm",
       dpi = 600)

# Save the PNG (scaled up for better text visibility)
save_png_version(p, paste0("final_results/", experiment_t, "_climate_yield.png"))
