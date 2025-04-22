# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(data.table)


data <- read.csv("outputs/era/base_ecocrop.csv")
data <- unique(data)

data$Mean_ph_h2o <- data$Mean_ph_h2o/10


data_renamed <- data %>%
  rename(
    # SoilGrids variables
    soilgrids_oc = Mean_oc,
    soilgrids_ph = Mean_ph_h2o,
    soilgrids_clay = Mean_clay_tot_psa,
    soilgrids_sand = Mean_sand_tot_psa,
    soilgrids_silt = Mean_silt_tot_psa,
    
    # ERA variables
    era_oc = SOC,
    era_ph = Soil.pH,
    era_clay = CLY,
    era_sand = SND,
    era_silt = SLT
  )

# Function to create cleaner comparison plots for soil properties
compare_soil_properties <- function(data, soilgrids_var, era_var, x_label, y_label) {
  complete_data <- data %>% 
    filter(!is.na(!!sym(soilgrids_var)), !is.na(!!sym(era_var)))
  
  pearson_corr <- cor(complete_data[[soilgrids_var]], complete_data[[era_var]], 
                      use = "complete.obs", method = "pearson")
  
  p <- ggplot(complete_data, aes_string(x = soilgrids_var, y = era_var)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "red") +
    labs(
      subtitle = paste0("r = ", round(pearson_corr, 3)),
      x = x_label,
      y = y_label
    ) +
    theme_minimal() +
    theme(plot.title = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95"))
  
  return(p)
}

# Define soil property pairs to compare with clean labels
soil_comparisons <- list(
  list(soilgrids_var = "soilgrids_oc", era_var = "era_oc", 
       x_label = "SoilGrids OC", y_label = "ERA OC"),
  list(soilgrids_var = "soilgrids_ph", era_var = "era_ph", 
       x_label = "SoilGrids pH", y_label = "ERA pH"),
  list(soilgrids_var = "soilgrids_clay", era_var = "era_clay", 
       x_label = "SoilGrids Clay", y_label = "ERA Clay"),
  list(soilgrids_var = "soilgrids_sand", era_var = "era_sand", 
       x_label = "SoilGrids Sand", y_label = "ERA Sand"),
  list(soilgrids_var = "soilgrids_silt", era_var = "era_silt", 
       x_label = "SoilGrids Silt", y_label = "ERA Silt")
)

# Generate plots
scatter_plots <- list()
for (comp in soil_comparisons) {
  scatter_plots[[comp$x_label]] <- compare_soil_properties(
    data_renamed, comp$soilgrids_var, comp$era_var, comp$x_label, comp$y_label)
  print(scatter_plots[[comp$x_label]])
  ggsave(paste0("outputs/era/soil_comparison_", comp$era_var, ".png"))
}


soil_vars <- c("soilgrids_oc", "era_oc", "soilgrids_ph", "era_ph", 
               "soilgrids_clay", "era_clay", "soilgrids_sand", "era_sand", 
               "soilgrids_silt", "era_silt")


soil_data <- data_renamed %>% select(all_of(soil_vars)) %>% drop_na()

# Pearson correlation
cor_matrix <- cor(soil_data, method = "pearson")


clean_labels <- c("SG OC", "ERA OC", "SG pH", "ERA pH", 
                  "SG Clay", "ERA Clay", "SG Sand", "ERA Sand", 
                  "SG Silt", "ERA Silt")


colnames(cor_matrix) <- clean_labels
rownames(cor_matrix) <- clean_labels

png("outputs/era/soil_correlation_plot.png", width = 8, height = 8, units = "in", res = 300)

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         tl.cex = 0.8,
         diag = FALSE)

dev.off()
