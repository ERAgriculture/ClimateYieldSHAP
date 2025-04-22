# Create beeswarm plots for SHAP values to show the effects of climate factors
# on yield across three agro-ecological zones (AEZs) that share temperature
# classification. Each subplot requires a variable for coloring to show 
# interaction effects.

library(gridExtra)
library(grid)
library(scales)
library(shadowtext)
library(ggplot2)
library(patchwork)
library(dplyr)
library(stringr)
library(tidymodels)
library(tidyverse)
library(yaml)
source('R/combine_shap_plots.R')
source('R/compute_shap_values.R')
source('R/estimate_metrics_simple_rf.R')
source('R/plot_bootstrap_shap.R')
source('R/plot_bootstrap_shap_gamCI.R')
source('R/analyze_bootstrap_metrics.R')
source('R/remap_formula.R')



spell_out_features <- function (feature_name){
  feature_name <- gsub("_", " ", feature_name)
  feature_name <- gsub("Dev", "Deviation", feature_name)
  feature_name <- gsub("Avg", "Average", feature_name)
  feature_name <- gsub("Max", "Maximum", feature_name)
  feature_name <- gsub("Temp", "Temperature", feature_name)
  feature_name <- gsub("Fraction", "Fraction of", feature_name)
  feature_name <- gsub("Longest", "Duration of the Longest", feature_name)
  return(feature_name)
}


#################
### WARM AEZs ###
#################
experiment <- "no_outliers_eco_isqrt_nosoil"
AEZ_temp <- "Warm."
plot_type <- "GAMCI_shap_plot_"  # GAM fit for bands
# plot_type <- "shap_plot_"  # No GAM fit for bands


# Define the zones to process
zones <- c("Semiarid", "Subhumid", "Humid")
R2 <- numeric(length(zones))
n_obs <- numeric(length(zones))

# Process each zone
for (i in seq_along(zones)) {
  # Read the metrics file for this zone
  metrics_file <- paste0("outputs/", experiment, "_", AEZ_temp, zones[i], "/full_results/cv_metrics.csv")
  
  # Read and extract R2 value
  if (file.exists(metrics_file)) {
    metrics <- read.csv(metrics_file)
    R2[i] <- metrics$mean[metrics$.metric == "rsq"]
    all_metrics <- read.csv(paste0("final_results/", experiment, "_performance_metrics.csv"))
    n_obs[i] <- all_metrics %>% 
      filter(AEZ == paste0(AEZ_temp, zones[i])) %>%
      select(N_obs) %>% pull(N_obs)
  } else {
    warning(paste("File not found:", metrics_file))
    R2[i] <- NA
  }
}


row_list = c("Semiarid", "Subhumid", "Humid")

shap_plot1 <- readRDS(file = paste0("outputs/", experiment, "_", AEZ_temp, 
                                    "Semiarid/full_results/plot_objects/shap_mean_importance.rds"))
levels(shap_plot1$data$feature) <- gsub("_", " ", 
                                        levels(shap_plot1$data$feature))

shap_plot2 <- readRDS(file = paste0("outputs/", experiment, "_", AEZ_temp, 
                                    "Subhumid/full_results/plot_objects/shap_mean_importance.rds"))
levels(shap_plot2$data$feature) <- gsub("_", " ", 
                                        levels(shap_plot2$data$feature))

shap_plot3 <- readRDS(file = paste0("outputs/", experiment, "_", AEZ_temp, 
                                    "Humid/full_results/plot_objects/shap_mean_importance.rds"))
levels(shap_plot3$data$feature) <- gsub("_", " ", 
                                        levels(shap_plot3$data$feature))


plot_titles <- c()
### FIRST ROW ###
feature1 <- "Avg_Max_Temp"
feature1_int <- "Avg_Max_Temp_Dev"

path_beeswarm_plot11 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Semiarid/bootstrap_plots/", plot_type,
                               feature1, "_", feature1_int, ".RDS")

feature2 <- "Total_Rainfall_Dev"
feature2_int <- "Total_Rainfall"

path_beeswarm_plot12 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Semiarid/bootstrap_plots/", plot_type,
                               feature2, "_", feature2_int, ".RDS")

feature3 <- "Total_Rainfall"
feature3_int <- "Avg_Max_Temp"

path_beeswarm_plot13 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Semiarid/bootstrap_plots/", plot_type,
                               feature3, "_", feature3_int, ".RDS")

feature4 <- "Avg_Max_Temp_Dev"
feature4_int <- "Avg_Max_Temp"

path_beeswarm_plot14 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Semiarid/bootstrap_plots/", plot_type,
                               feature4, "_", feature4_int, ".RDS")

plot_titles <- append(plot_titles, paste0("", spell_out_features(feature1)))
plot_titles <- append(plot_titles, paste0("", spell_out_features(feature2)))
plot_titles <- append(plot_titles, paste0("", spell_out_features(feature3)))
plot_titles <- append(plot_titles, paste0("", spell_out_features(feature4)))
### SECOND ROW ###
feature1 <- "Avg_Max_Temp"
feature1_int <- "Avg_Max_Temp_Dev"

path_beeswarm_plot21 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Subhumid/bootstrap_plots/", plot_type,
                               feature1, "_", feature1_int, ".RDS")

feature2 <- "Avg_Max_Temp_Dev"
feature2_int <- "Avg_Max_Temp"

path_beeswarm_plot22 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Subhumid/bootstrap_plots/", plot_type,
                               feature2, "_", feature2_int, ".RDS")

feature3 <- "Longest_Dry_Spell"
feature3_int <- "Fraction_Rainy_Days_Dev"

path_beeswarm_plot23 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Subhumid/bootstrap_plots/", plot_type,
                               feature3, "_", feature3_int, ".RDS")

feature4 <- "Total_Rainfall"
feature4_int <- "Avg_Max_Temp_Dev"

path_beeswarm_plot24 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Subhumid/bootstrap_plots/", plot_type,
                               feature4, "_", feature4_int, ".RDS")

plot_titles <- append(plot_titles, paste0("", spell_out_features(feature1)))
plot_titles <- append(plot_titles, paste0("", spell_out_features(feature2)))
plot_titles <- append(plot_titles, paste0("", spell_out_features(feature3)))
plot_titles <- append(plot_titles, paste0("", spell_out_features(feature4)))

### THRID ROW ###
feature1 <- "Total_Rainfall"
feature1_int <- "Fraction_Rainy_Days_Dev"

path_beeswarm_plot31 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Humid/bootstrap_plots/", plot_type,
                               feature1, "_", feature1_int, ".RDS")

feature2 <- "Avg_Max_Temp_Dev"
feature2_int <- "Avg_Max_Temp"

path_beeswarm_plot32 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Humid/bootstrap_plots/", plot_type,
                               feature2, "_", feature2_int, ".RDS")

feature3 <- "Total_Rainfall_Dev"
feature3_int <- "Total_Rainfall"

path_beeswarm_plot33 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Humid/bootstrap_plots/", plot_type,
                               feature3, "_", feature3_int, ".RDS")

feature4 <- "Fraction_Rainy_Days"
feature4_int <- "Total_Rainfall"

path_beeswarm_plot34 <- paste0("outputs/", experiment, "_", AEZ_temp,
                               "Humid/bootstrap_plots/", plot_type,
                               feature4, "_", feature4_int, ".RDS")

plot_titles <- append(plot_titles, paste0("", spell_out_features(feature1)))
plot_titles <- append(plot_titles, paste0("", spell_out_features(feature2)))
plot_titles <- append(plot_titles, paste0("", spell_out_features(feature3)))
plot_titles <- append(plot_titles, paste0("", spell_out_features(feature4)))


beeswarm_plot11 <- readRDS(file = path_beeswarm_plot11)
beeswarm_plot12 <- readRDS(file = path_beeswarm_plot12)
beeswarm_plot13 <- readRDS(file = path_beeswarm_plot13)
beeswarm_plot14 <- readRDS(file = path_beeswarm_plot14)

beeswarm_plot21 <- readRDS(file = path_beeswarm_plot21)
beeswarm_plot22 <- readRDS(file = path_beeswarm_plot22)
beeswarm_plot23 <- readRDS(file = path_beeswarm_plot23)
beeswarm_plot24 <- readRDS(file = path_beeswarm_plot24)

beeswarm_plot31 <- readRDS(file = path_beeswarm_plot31)
beeswarm_plot32 <- readRDS(file = path_beeswarm_plot32)
beeswarm_plot33 <- readRDS(file = path_beeswarm_plot33)
beeswarm_plot34 <- readRDS(file = path_beeswarm_plot34)


# norm_params <- readRDS("outputs/feature_normalization.rds")$norm_params

y_min <- min(c(ggplot_build(beeswarm_plot11)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot12)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot13)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot14)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot21)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot22)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot23)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot24)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot31)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot32)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot33)$layout$panel_params[[1]]$y.range[1],
               ggplot_build(beeswarm_plot34)$layout$panel_params[[1]]$y.range[1]))

y_max <- max(c(ggplot_build(beeswarm_plot11)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot12)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot13)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot14)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot21)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot22)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot23)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot24)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot31)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot32)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot33)$layout$panel_params[[1]]$y.range[2],
               ggplot_build(beeswarm_plot34)$layout$panel_params[[1]]$y.range[2]))


imp_x_max <- max(c(ggplot_build(shap_plot1)$layout$panel_params[[1]]$x.range[2],
                   ggplot_build(shap_plot2)$layout$panel_params[[1]]$x.range[2],
                   ggplot_build(shap_plot3)$layout$panel_params[[1]]$x.range[2]
))


plot_list <- list(shap_plot1, beeswarm_plot11, beeswarm_plot12, beeswarm_plot13, beeswarm_plot14,
                  shap_plot2, beeswarm_plot21, beeswarm_plot22, beeswarm_plot23, beeswarm_plot24,
                  shap_plot3, beeswarm_plot31, beeswarm_plot32, beeswarm_plot33, beeswarm_plot34)


AEZ_temp <- paste0(substr(AEZ_temp, 1, 4), " ")

main_title <- ""

# Create list to store processed plots
processed_plots <- vector("list", 15)

plot1 <- plot_list[[1]]
plot1 <- plot1 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_blank(), 
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_blank()
  ) +
  ggtitle("Feature Importance") + 
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  xlim(0, imp_x_max) +
  labs(y = paste0(AEZ_temp, " ", row_list[1]),
       x = "Mean yield change (kg/ha)") +
  coord_cartesian(clip = "off") + 
  geom_shadowtext(aes(x = imp_x_max, label = feature),
                  color = "black",
                  bg.color = "white",
                  bg.r = 0.15,
                  hjust = 1,
                  size = 3.6) +
  geom_shadowtext(
    aes(x = imp_x_max * 0.22, 
        y = 0.7,
        label = sprintf("N = %d\nR² = %.2f", n_obs[1], R2[1])),
    hjust = 0,
    vjust = 0,
    size = 3.6,
    color = "black",
    bg.color = "white",
    bg.r = 0.15
  )


# Plot 2 - row 1, col 2
plot2 <- plot_list[[2]]
plot2 <- plot2 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    # legend.position = "none",
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[1]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)



# Plot 3 - row 1, col 3
plot3 <- plot_list[[3]]
plot3 <- plot3 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    # legend.position = "none",
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    axis.title.y = element_blank(),
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[2]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)

# Plot 4 - row 1, col 4
plot4 <- plot_list[[4]]
plot4 <- plot4 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    # legend.position = "none",
    legend.direction = "horizontal",
    axis.title.y = element_blank(),
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[3]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)

# Plot 5 - row 1, col 5
plot5 <- plot_list[[5]]
plot5 <- plot5 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    # legend.position = "none",
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[4]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)

# Plot 6 - row 2, col 1
plot6 <- plot_list[[6]]
plot6 <- plot6 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_blank()
  ) +
  ggtitle("Feature Importance") + 
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  xlim(0, imp_x_max) +
  labs(y = paste0(AEZ_temp, " ", row_list[2]),
       x = "Mean yield change (kg/ha)") +
  coord_cartesian(clip = "off") + 
  geom_shadowtext(aes(x = imp_x_max, label = feature),  # Fix x-position
                  color = "black",
                  bg.color = "white",
                  bg.r = 0.15,
                  hjust = 1,  # Right-align text
                  size = 3.6) +
  geom_shadowtext(
    aes(x = imp_x_max * 0.22, 
        y = 0.7,
        label = sprintf("N = %d\nR² = %.2f", n_obs[2], R2[2])),
    hjust = 0,
    vjust = 0,
    size = 3.6,
    color = "black",
    bg.color = "white",
    bg.r = 0.15
  )



# Plot 7 - row 2, col 2
plot7 <- plot_list[[7]]
plot7 <- plot7 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    # legend.position = "none",
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[5]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)

# Plot 8 - row 2, col 3
plot8 <- plot_list[[8]]
plot8 <- plot8 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    # legend.position = "none",
    axis.title.y = element_blank(),
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[6]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)

# Plot 9 - row 2, col 4
plot9 <- plot_list[[9]]
plot9 <- plot9 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    # legend.position = "none",
    axis.title.y = element_blank(),
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[7]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)

# Plot 10 - row 2, col 5
plot10 <- plot_list[[10]]
plot10 <- plot10 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    # legend.position = "none",
    axis.title.y = element_blank(),
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[8]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)

# Plot 11 - row 3, col 1
plot11 <- plot_list[[11]]
plot11 <- plot11 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 5, 5),
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_blank()
  ) +
  ggtitle("Feature Importance") + 
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  xlim(0, imp_x_max) +
  labs(y = paste0(AEZ_temp, " ", row_list[3]),
       x = "Mean yield change (kg/ha)") +
  coord_cartesian(clip = "off") + 
  geom_shadowtext(aes(x = imp_x_max, label = feature),
                  color = "black",
                  bg.color = "white",
                  bg.r = 0.15,
                  hjust = 1,
                  size = 3.6) +
  geom_shadowtext(
    aes(x = imp_x_max * 0.22, 
        y = 0.7,
        label = sprintf("N = %d\nR² = %.2f", n_obs[3], R2[3])),
    hjust = 0,
    vjust = 0,
    size = 3.6,
    color = "black",
    bg.color = "white",
    bg.r = 0.15
  )


# Plot 12 - row 3, col 2
plot12 <- plot_list[[12]]
plot12 <- plot12 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 10, 5),
    # legend.position = "none",
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[9]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)

# Plot 13 - row 3, col 3
plot13 <- plot_list[[13]]
plot13 <- plot13 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 10, 5),
    # legend.position = "none",
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    axis.title.y = element_blank(),
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[10]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)

# Plot 14 - row 3, col 4
plot14 <- plot_list[[14]]
plot14 <- plot14 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 10, 5),
    # legend.position = "none",
    axis.title.y = element_blank(),
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[11]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)

# Plot 15 - row 3, col 5
plot15 <- plot_list[[15]]
plot15 <- plot15 +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 20, 10, 5),
    # legend.position = "none",
    axis.title.y = element_blank(),
    legend.direction = "horizontal",
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0.6, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title = element_text(size = 9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  ) +
  ggtitle(plot_titles[12]) +
  theme(
    plot.title = element_text(
      size = 12,
      hjust = 0.5
    )) +
  ylim(y_min, y_max)


processed_plots <- list(plot1, plot2, plot3, plot4, plot5,
                        plot6, plot7, plot8, plot9, plot10,
                        plot11, plot12, plot13, plot14, plot15)

labels <- lapply(letters[1:length(processed_plots)], function(l) {
  textGrob(paste0(l, ")"), x = 0.02, y = 0, hjust = 0, vjust = 1, 
           gp = gpar(fontsize = 12, fontface = "bold"))
})

labeled_plots <- mapply(
  function(plot, label) arrangeGrob(plot, top = label),
  processed_plots, labels, SIMPLIFY = FALSE
)



combined_plot <- gridExtra::grid.arrange(
  grobs = labeled_plots,
  ncol = 5,
  top = textGrob(main_title, gp = gpar(fontsize = 12, fontface = "bold"))
)



ggsave(
  paste0("final_results/", plot_type, substr(AEZ_temp, 1, 4), "no_outliers_combined_shap_plot_3x5_legends.png"), 
  combined_plot,
  width = 20,
  height = 13,
  dpi = 300
)


