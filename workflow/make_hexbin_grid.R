library(ggplot2)
library(hexbin)
library(patchwork)
library(dplyr)
library(stringr)
library(ggrepel)
library(shadowtext)
source('R/hexbin_grid.R')


# Select AEZ Temperature
AEZ_temp <- "Warm"
# AEZ_temp <- "Cool"

# Create an empty list to store SHAP datasets
shap_list <- list()

experiment <- "no_outliers_eco_isqrt_nosoil"

# Load CV importance data for each climate zone
cv_importance_list <- list()

if (AEZ_temp == "Warm"){
  max_importance <- 500
  features_list <- list(
    c("Avg_Max_Temp", "Total_Rainfall_Dev", "Total_Rainfall", "Avg_Max_Temp_Dev"),
    c("Avg_Max_Temp", "Avg_Max_Temp_Dev", "Longest_Dry_Spell", "Total_Rainfall"),
    c("Total_Rainfall", "Avg_Max_Temp_Dev", "Total_Rainfall_Dev", "Fraction_Rainy_Days"))
  
  custom_log_breaks <- c(1, 4, 16, 64, 256)
  
  AEZ_names <- c("Warm Semiarid", "Warm Subhumid", "Warm Humid")
  # Read data for Warm Semiarid
  results <- readRDS(paste0("outputs/", experiment, "_Warm.Semiarid/full_results/full_results.rds"))
  cv_results <- readRDS(paste0("outputs/", experiment, "_Warm.Semiarid/full_results/cv_results.rds"))
  shap_list[[1]] <- results$shap_values
  cv_importance_list[[1]] <- cv_results$shap_plot
  
  # Read data for Warm Subhumid
  results <- readRDS(paste0("outputs/", experiment, "_Warm.Subhumid/full_results/full_results.rds"))
  cv_results <- readRDS(paste0("outputs/", experiment, "_Warm.Subhumid/full_results/cv_results.rds"))
  shap_list[[2]] <- results$shap_values
  cv_importance_list[[2]] <- cv_results$shap_plot
  
  # Read data for Warm Humid
  results <- readRDS(paste0("outputs/", experiment, "_Warm.Humid/full_results/full_results.rds"))
  cv_results <- readRDS(paste0("outputs/", experiment, "_Warm.Humid/full_results/cv_results.rds"))
  shap_list[[3]] <- results$shap_values
  cv_importance_list[[3]] <- cv_results$shap_plot
  
} else if(AEZ_temp == "Cool"){
  max_importance <- 750
  
  features_list <- list(
    c("Total_Rainfall", "Avg_Max_Temp_Dev", "Total_Rainfall_Dev", "Avg_Max_Temp"),
    c("Avg_Max_Temp", "Total_Rainfall", "Fraction_Rainy_Days", "Total_Rainfall_Dev"),
    c("Avg_Max_Temp_Dev", "Fraction_Rainy_Days_Dev", "Total_Rainfall", "Avg_Max_Temp"))
  
  custom_log_breaks <- c(1, 4, 16, 64, 150)
  
  AEZ_names <- c("Cool Semiarid", "Cool Subhumid", "Cool Humid")
  # Read data for Cool Semiarid
  results <- readRDS(paste0("outputs/", experiment, "_Cool.Semiarid/full_results/full_results.rds"))
  cv_results <- readRDS(paste0("outputs/", experiment, "_Cool.Semiarid/full_results/cv_results.rds"))
  shap_list[[1]] <- results$shap_values
  cv_importance_list[[1]] <- cv_results$shap_plot
  
  # Read data for Cool Subhumid
  results <- readRDS(paste0("outputs/", experiment, "_Cool.Subhumid/full_results/full_results.rds"))
  cv_results <- readRDS(paste0("outputs/", experiment, "_Cool.Subhumid/full_results/cv_results.rds"))
  shap_list[[2]] <- results$shap_values
  cv_importance_list[[2]] <- cv_results$shap_plot
  
  # Read data for Cool Humid
  results <- readRDS(paste0("outputs/", experiment, "_Cool.Humid/full_results/full_results.rds"))
  cv_results <- readRDS(paste0("outputs/", experiment, "_Cool.Humid/full_results/cv_results.rds"))
  shap_list[[3]] <- results$shap_values
  cv_importance_list[[3]] <- cv_results$shap_plot
  
}

performance_metrics <- read.csv(paste0("final_results/", experiment, "_performance_metrics.csv"))
performance_metrics$AEZ <- gsub("\\.", " ", performance_metrics$AEZ)


# Only show title for the first plot, and show stats for all plots
for (i in seq_along(cv_importance_list)) {
  aez_performance <- performance_metrics %>% 
    filter(AEZ == AEZ_names[i])
  cv_importance_list[[i]] <- modify_importance_plot_with_data(
    cv_importance_list[[i]],
    AEZ_name = AEZ_names[i],
    R2 = aez_performance$Full_Test_R2,
    n_obs = aez_performance$N_obs,
    max_importance = max_importance,
    show_title = (i == 1),  # Only show title for the first plot
    show_stats = TRUE
  )
}


# Create the plot grid with modified CV importance plots and custom log breaks
plot_grid <- create_shap_hexbin_grid_with_cv(
  data_list = shap_list,
  features_list = features_list,
  cv_importance_list = cv_importance_list,
  row_names = NULL,
  title = "",
  show_correlation = FALSE,
  add_trend_line = FALSE,
  n_x_breaks = 4,
  k = 3,
  custom_log_breaks = custom_log_breaks
)

# Display the plot
print(plot_grid)

# Save the figure
ggsave(
  paste0("final_results/", AEZ_temp , "_hexbinc.pdf"),
  plot = plot_grid,
  width = 180,              # Width in mm
  height = 160,             # Height in mm
  units = "mm",
  dpi = 300
)
ggsave(
  paste0("final_results/", AEZ_temp , "_hexbinc.png"),
  plot = plot_grid,
  width = 180,              # Width in mm
  height = 160,             # Height in mm
  units = "mm",
  dpi = 300
)