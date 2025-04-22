library(ggplot2)
library(hexbin)
library(patchwork)
library(dplyr)
library(stringr)
library(ggrepel) # For better text positioning
library(shadowtext) # For text with background

# First, load your functions file
# source('R/hexbin_grid.R')

# Select AEZ Temperature
AEZ_temp <- "Warm"
# AEZ_temp <- "Cool"

# Create an empty list to store SHAP datasets
shap_list <- list()

# Load CV importance data for each climate zone
cv_importance_list <- list()

if (AEZ_temp == "Warm"){
  features_list <- list(
    c("Avg_Max_Temp", "Total_Rainfall_Dev", "Total_Rainfall", "Avg_Max_Temp_Dev"),
    c("Avg_Max_Temp", "Avg_Max_Temp_Dev", "Longest_Dry_Spell", "Total_Rainfall"),
    c("Total_Rainfall", "Avg_Max_Temp_Dev", "Total_Rainfall_Dev", "Fraction_Rainy_Days"))
  
  custom_log_breaks <- c(1, 4, 16, 64, 256)
  
  AEZ_names <- c("Warm Semiarid", "Warm Subhumid", "Warm Humid")
  # Read data for Warm Semiarid
  results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Warm.Semiarid/full_results/full_results.rds")
  cv_results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Warm.Semiarid/full_results/cv_results.rds")
  shap_list[[1]] <- results$shap_values
  cv_importance_list[[1]] <- cv_results$shap_plot
  
  # Read data for Warm Subhumid
  results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Warm.Subhumid/full_results/full_results.rds")
  cv_results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Warm.Subhumid/full_results/cv_results.rds")
  shap_list[[2]] <- results$shap_values
  cv_importance_list[[2]] <- cv_results$shap_plot
  
  # Read data for Warm Humid
  results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Warm.Humid/full_results/full_results.rds")
  cv_results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Warm.Humid/full_results/cv_results.rds")
  shap_list[[3]] <- results$shap_values
  cv_importance_list[[3]] <- cv_results$shap_plot
  
} else if(AEZ_temp == "Cool"){
  features_list <- list(
    c("Total_Rainfall", "Avg_Max_Temp_Dev", "Total_Rainfall_Dev", "Avg_Max_Temp"),
    c("Avg_Max_Temp", "Total_Rainfall", "Fraction_Rainy_Days", "Total_Rainfall_Dev"),
    c("Avg_Max_Temp_Dev", "Fraction_Rainy_Days_Dev", "Total_Rainfall", "Avg_Max_Temp"))
  
  custom_log_breaks <- c(1, 4, 16, 64, 150)
  
  AEZ_names <- c("Cool Semiarid", "Cool Subhumid", "Cool Humid")
  # Read data for Cool Semiarid
  results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Cool.Semiarid/full_results/full_results.rds")
  cv_results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Cool.Semiarid/full_results/cv_results.rds")
  shap_list[[1]] <- results$shap_values
  cv_importance_list[[1]] <- cv_results$shap_plot
  
  # Read data for Cool Subhumid
  results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Cool.Subhumid/full_results/full_results.rds")
  cv_results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Cool.Subhumid/full_results/cv_results.rds")
  shap_list[[2]] <- results$shap_values
  cv_importance_list[[2]] <- cv_results$shap_plot
  
  # Read data for Cool Humid
  results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Cool.Humid/full_results/full_results.rds")
  cv_results <- readRDS("outputs/no_outliers_eco_isqrt_nosoil_Cool.Humid/full_results/cv_results.rds")
  shap_list[[3]] <- results$shap_values
  cv_importance_list[[3]] <- cv_results$shap_plot
  
}






#' Spell out feature names for better readability in plots
#'
#' @param feature_name The name of the feature to format
#' @return A formatted feature name with expanded abbreviations
spell_out_features <- function(feature_name) {
  feature_name <- gsub("_", " ", feature_name)
  feature_name <- gsub("Dev", "Deviation", feature_name)
  feature_name <- gsub("Avg", "Average", feature_name)
  feature_name <- gsub("Max", "Maximum", feature_name)
  feature_name <- gsub("Temp", "Temperature", feature_name)
  feature_name <- gsub("Fraction", "Fraction of", feature_name)
  feature_name <- gsub("Longest", "Duration of the Longest", feature_name)
  return(feature_name)
}


# Direct approach to modify importance plots using the plot's data directly
modify_importance_plot_with_data <- function(plot, AEZ_name, 
                                             n_obs = 500, 
                                             R2 = 0.7, 
                                             base_size = 8,
                                             show_title = FALSE, 
                                             show_stats = FALSE) {
  # Access the plot's data directly
  plot_data <- plot$data
  
  # Get unique feature names and make sure they're in the correct order by position
  ordered_features <- levels(plot_data$feature)
  
  # Clean the feature names by replacing underscores with spaces
  clean_features <- gsub("_", " ", ordered_features)
  
  # Get the maximum x value for positioning
  x_max <- max(layer_data(plot)$x, na.rm = TRUE)
  
  # Create a data frame for the feature labels
  feature_labels <- data.frame(
    x = rep(x_max * 0.95, length(ordered_features)),
    y = seq_along(ordered_features),
    feature = clean_features
  )
  
  # Modify the plot to use our custom labels
  modified_plot <- plot +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 20, 5, 5),
      legend.position = "none",
      axis.text.x = element_text(size = base_size),
      axis.title.x = element_text(size = base_size),
      axis.text.y = element_blank(),  # Hide original y-axis labels
      axis.title.y = element_text(size = 10, face = "bold"),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      y = AEZ_name,
      x = "Mean yield change (kg/ha)"
    ) +
    coord_cartesian(clip = "off") +
    geom_shadowtext(
      data = feature_labels,
      aes(x = x, y = y, label = feature),
      color = "black",
      bg.color = "white",
      bg.r = 0.15,
      hjust = 1,
      size = 2.6,
      fontface = "plain"
    )
  
  # Add title only if requested
  if (show_title) {
    modified_plot <- modified_plot + 
      ggtitle("Feature Importance") + 
      theme(
        plot.title = element_text(
          size = 8,
          hjust = 0.5
        )
      )
  } else{
    modified_plot <- modified_plot + 
      theme(
        plot.title = element_blank()
      )
  }
  
  # Add stats only if requested
  if (show_stats) {
    # Create data frame for stats
    stats_data <- data.frame(
      x = x_max * 0.22,
      y = 0.7,
      label = sprintf("N = %d\nR² = %.2f", n_obs, R2)
    )
    
    modified_plot <- modified_plot +
      geom_shadowtext(
        data = stats_data,
        aes(x = x, y = y, label = label),
        hjust = 0,
        vjust = 0,
        size = 2.8,
        color = "black",
        bg.color = "white",
        bg.r = 0.15
      )
  }
  
  return(modified_plot)
}

# Modify the importance plots using the new approach with data
# Only show title for the first plot, and show stats for all plots
for (i in seq_along(cv_importance_list)) {
  cv_importance_list[[i]] <- modify_importance_plot_with_data(
    cv_importance_list[[i]], 
    AEZ_name = AEZ_names[i],
    show_title = (i == 1),  # Only show title for the first plot
    show_stats = FALSE       # Set to FALSE to hide stats
  )
}


#' Create a multi-panel plot of hexbin SHAP value plots
#'
#' @param data A data frame containing the feature values and corresponding SHAP values
#' @param features A vector of feature names to plot (without "SHAP_" prefix)
#' @param bins Number of bins for hexagonal binning (default: 30)
#' @param color_palette Color palette to use (default: "viridis")
#' @param ncol Number of columns in the grid (default: 3)
#' @param nrow Number of rows in the grid (default: calculated based on number of features)
#' @param show_correlation Whether to display correlation in titles (default: TRUE)
#' @param add_trend_line Whether to add trend lines (default: TRUE)
#' @param title Main title for the combined plot (default: "SHAP Impact on Yield")
#' @param k Complexity parameter for the GAM smooth (default: 3)
#'
#' @return A combined ggplot object
# Create a modified version of create_shap_hexbin_grid that includes CV importance plots
create_shap_hexbin_grid_with_cv <- function(data_list, 
                                            features_list, 
                                            cv_importance_list, 
                                            row_names = NULL, 
                                            title = "", 
                                            show_correlation = TRUE, 
                                            add_trend_line = TRUE, 
                                            n_x_breaks = 4,
                                            bins = 30, 
                                            color_palette = "viridis", 
                                            log_scale = TRUE, 
                                            base_size = 8,
                                            k = 3,
                                            custom_log_breaks = NULL) {
  # Use the original function to create the hexbin plots
  # But we'll modify the output to include CV importance plots
  
  # Calculate all parameters needed for consistent hexbins
  all_counts <- numeric()
  all_shap_values <- numeric()
  
  # Loop through each dataset in the list
  for (dataset_idx in seq_along(data_list)) {
    data <- data_list[[dataset_idx]]
    features <- features_list[[dataset_idx]]
    
    if (is.list(features) && !is.character(features)) {
      features <- unlist(features)
    }
    
    for (feature in features) {
      feature_values <- data[[feature]]
      shap_values <- data[[paste0("SHAP_", feature)]]
      all_shap_values <- c(all_shap_values, shap_values)
      
      plot_data <- data.frame(
        feature_value = feature_values,
        shap_value = shap_values
      )
      
      temp_plot <- ggplot(plot_data, aes(x = feature_value, y = shap_value)) +
        stat_binhex(bins = bins)
      
      plot_data <- ggplot_build(temp_plot)$data[[1]]
      all_counts <- c(all_counts, plot_data$count)
    }
  }
  
  # Calculate global parameters
  y_min <- min(all_shap_values, na.rm = TRUE)
  y_max <- max(all_shap_values, na.rm = TRUE)
  y_range <- y_max - y_min
  y_buffer <- 0.05 * y_range
  global_y_limits <- c(y_min - y_buffer, y_max + y_buffer)
  
  if (log_scale) {
    global_min <- max(1, min(all_counts))
  } else {
    global_min <- min(all_counts)
  }
  global_max <- max(all_counts)
  
  # Create appropriate breaks
  if (log_scale) {
    if (!is.null(custom_log_breaks)) {
      # Use custom breaks if provided
      custom_breaks <- custom_log_breaks
    } else {
      log_min <- log10(global_min)
      log_max <- log10(global_max)
      
      if (log_max - log_min > 2) {
        log_breaks <- seq(floor(log_min), ceiling(log_max))
        custom_breaks <- 10^log_breaks
      } else {
        log_breaks <- seq(log_min, log_max, length.out = 5)
        custom_breaks <- 10^log_breaks
        custom_breaks <- round(custom_breaks)
      }
      
      custom_breaks <- sort(unique(c(ceiling(global_min), custom_breaks, floor(global_max))))
    }
  } else {
    custom_breaks <- seq(floor(global_min), ceiling(global_max), length.out = 5)
  }
  
  # Now build the combined plot with CV importance
  plot_rows <- list()
  
  for (row_idx in seq_along(data_list)) {
    data <- data_list[[row_idx]]
    features <- features_list[[row_idx]]
    cv_importance <- cv_importance_list[[row_idx]]
    
    if (is.list(features) && !is.character(features)) {
      features <- unlist(features)
    }
    
    row_plots <- list()
    
    # Add the CV importance plot as the first column
    # We're using the already modified cv_importance plot here
    row_plots[[1]] <- cv_importance
    
    # Add the hexbin plots
    for (col_idx in seq_along(features)) {
      feature <- features[col_idx]
      
      get_feature_length <- function(idx) {
        feat <- features_list[[idx]]
        if (is.list(feat) && !is.character(feat)) {
          return(length(unlist(feat)))
        } else {
          return(length(feat))
        }
      }
      
      current_plot_count <- 0
      if (row_idx > 1) {
        previous_lengths <- sapply(seq_len(row_idx-1), get_feature_length)
        if (length(previous_lengths) > 0) {
          current_plot_count <- sum(as.numeric(previous_lengths))
        }
      }
      current_plot_count <- current_plot_count + col_idx
      
      all_lengths <- sapply(seq_along(data_list), get_feature_length)
      last_plot_count <- sum(as.numeric(all_lengths))
      
      show_legend <- (current_plot_count == last_plot_count)
      
      # Always show y-axis title for second column (first hexbin) as well
      is_first_column <- (col_idx == 1)
      
      feature_values <- data[[feature]]
      shap_values <- data[[paste0("SHAP_", feature)]]
      correlation <- cor(feature_values, shap_values, use = "complete.obs")
      
      plot_data <- data.frame(
        feature_value = feature_values,
        shap_value = shap_values
      )
      
      if (show_correlation) {
        title_text <- sprintf("%s (r = %.2f)", feature, correlation)
      } else {
        title_text <- feature
      }
      
      # Clean up titles using spell_out_features
      clean_title <- spell_out_features(title_text)
      clean_x_label <- spell_out_features(feature)
      
      p <- ggplot(plot_data, aes(x = feature_value, y = shap_value)) +
        stat_binhex(bins = bins)
      
      if (log_scale) {
        p <- p + scale_fill_viridis_c(
          option = color_palette,
          name = "Count",
          trans = "log10",
          breaks = custom_breaks,
          labels = function(x) format(round(x), big.mark = ","),
          limits = c(global_min, global_max)
        )
      } else {
        p <- p + scale_fill_viridis_c(
          option = color_palette,
          name = "Count",
          breaks = custom_breaks,
          labels = function(x) format(round(x), big.mark = ","),
          limits = c(global_min, global_max)
        )
      }
      
      x_range <- range(feature_values, na.rm = TRUE)
      x_breaks <- seq(from = x_range[1], to = x_range[2], length.out = n_x_breaks)
      
      max_abs_value <- max(abs(x_breaks))
      if (max_abs_value < 1) {
        x_breaks <- round(x_breaks, 1)
      } else if (max_abs_value < 10) {
        x_breaks <- round(x_breaks, 1)
      } else if (max_abs_value < 100) {
        x_breaks <- round(x_breaks, 0)
      } else {
        x_breaks <- round(x_breaks / 10) * 10
      }
      
      # Only show y-axis title for the first hexbin column (second column overall)
      p <- p + labs(
        title = clean_title,
        x = clean_x_label,
        y = if(is_first_column) "Yield change (kg/ha)" else NULL
      ) +
        scale_y_continuous(limits = global_y_limits, breaks = pretty(global_y_limits, n = 4)) +
        scale_x_continuous(breaks = x_breaks) +
        theme_minimal(base_size = base_size) +
        theme(
          plot.title = element_text(hjust = 0.5, size = rel(0.9)),
          axis.title = element_text(size = rel(0.8)),
          axis.text = element_text(size = rel(0.7)),
          legend.title = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.7)),
          panel.grid.minor = element_blank()
        )
      
      if (add_trend_line) {
        p <- p + geom_smooth(method = "gam",
                             formula = y ~ s(x, k = k),
                             color = "red", 
                             size = 0.2, 
                             se = FALSE)
      }
      
      if (!show_legend) {
        p <- p + theme(legend.position = "none")
      } else {
        p <- p + theme(
          legend.position = "bottom",
          legend.key.height = unit(0.5, "lines"),
          legend.key.width = unit(1.5, "lines")
        )
      }
      
      row_plots[[col_idx + 1]] <- p
    }
    
    # We don't need row labels anymore since they're in the CV importance plots
    # First column (CV importance) should be wider
    row_plots <- wrap_plots(row_plots, ncol = length(row_plots),
                            widths = c(1.5, rep(1, length(row_plots)-1)))
    
    plot_rows[[row_idx]] <- row_plots
  }
  
  combined_plot <- wrap_plots(plot_rows, ncol = 1) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = title,
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = rel(1.2)),
        plot.margin = margin(5, 5, 5, 5)
      )
    ) &
    theme(
      legend.position = "bottom",
      legend.key.height = unit(0.3, "lines"),
      legend.key.width = unit(1.0, "lines"),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.margin = margin(0, 0, 0, 0),
      panel.grid.minor = element_blank()
    )
  
  return(combined_plot)
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
  paste0("final_results/", AEZ_temp , "_hexbin.pdf"),
  plot = plot_grid,
  width = 240,              # Width in mm
  height = 185,             # Height in mm
  units = "mm",
  dpi = 300
)
ggsave(
  paste0("final_results/", AEZ_temp , "_hexbin.png"),
  plot = plot_grid,
  width = 240,              # Width in mm
  height = 185,             # Height in mm
  units = "mm",
  dpi = 300
)