#' Spell out feature names for better readability in plots
#'
#' This function improves the readability of feature names by expanding common
#' abbreviations and formatting them for use in plots and visualizations.
#'
#' @param feature_name The name of the feature to format
#'
#' @return A formatted feature name with expanded abbreviations
#'
#' @examples
#' spell_out_features("Rain.Days.L.0.Dev.Mean")
#' spell_out_features("Avg_Max_Temp")
#' # Returns "Average Maximum Temperature"
#'
#' spell_out_features("Max_Temp_Fraction")
#' # Returns "Maximum Temperature Fraction of"
spell_out_features <- function(feature_name) {
  feature_name <- gsub("Avg_Max_Temp_Dev", "Average Maximum\nTemperature Deviation", feature_name)
  feature_name <- gsub("Avg_Max_Temp", "Average Maximum\nTemperature", feature_name)
  feature_name <- gsub("Total_Rainfall_Dev", "Total Rainfall\nDeviation", feature_name)
  feature_name <- gsub("Total_Rainfall", "Total Rainfall", feature_name)
  feature_name <- gsub("Longest_Dry_Spell", "Longest Dry Spell", feature_name)
  feature_name <- gsub("Fraction_Rainy_Days_Dev", "Fraction Rainy\nDays Deviation", feature_name)
  feature_name <- gsub("Fraction_Rainy_Days", "Fraction\nRainy Days", feature_name)
  return(feature_name)
}


#' Modify importance plot with enhanced formatting and labels
#'
#' This function takes an existing importance plot and enhances it with custom
#' formatting, readable feature labels, and optional statistics display.
#'
#' @param plot Original importance plot to modify
#' @param AEZ_name Agro-ecological zone name to display as y-axis label
#' @param n_obs Number of observations in the dataset (default: 500)
#' @param R2 R-squared value of the model (default: 0.7)
#' @param base_size Base font size for plot elements (default: 8)
#' @param show_title Whether to display the plot title (default: FALSE)
#' @param show_stats Whether to display observation count and R-squared (default: FALSE)
#'
#' @return A modified ggplot object with enhanced formatting and labels
#'
#' @examples
#' # Basic usage with minimal statistics
#' modified_plot <- modify_importance_plot_with_data(
#'   plot = original_importance_plot,
#'   AEZ_name = "Cool Semiarid"
#' )
#' 
#' # With full statistics display
#' modified_plot <- modify_importance_plot_with_data(
#'   plot = original_importance_plot,
#'   AEZ_name = "Warm Subhumid",
#'   show_title = TRUE,
#'   show_stats = TRUE
#' )
modify_importance_plot_with_data <- function(plot, 
                                             AEZ_name, 
                                             n_obs = 500, 
                                             max_importance = 500,
                                             R2 = 0.7, 
                                             base_size = 5,
                                             show_title = FALSE, 
                                             show_stats = FALSE) {
  # Access the plot's data directly
  plot_data <- plot$data
  
  # Extract the original data for recreating the plot
  feature_data <- plot_data %>%
    select(feature, importance) %>%
    distinct()
  
  # Get unique feature names and make sure they're in the correct order by position
  ordered_features <- levels(as.factor(feature_data$feature))
  
  # Clean the feature names by replacing underscores with spaces
  clean_features <- gsub("_", " ", ordered_features)
  
  # Create a new plotting data frame
  new_plot_data <- data.frame(
    feature = factor(feature_data$feature, levels = ordered_features),
    importance = feature_data$importance
  )
  
  # Calculate positioning for labels
  label_position <- max_importance * 0.95
  
  # Create a new plot from scratch instead of modifying existing plot
  modified_plot <- ggplot(new_plot_data, aes(x = importance, y = feature)) +
    geom_col(fill = "dimgrey") +  
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 20, 5, 5),
      legend.position = "none",
      axis.text.x = element_text(size = base_size),
      axis.title.x = element_text(size = base_size),
      axis.text.y = element_blank(),
      axis.title.y = element_text(size = 10, face = "bold"),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      y = AEZ_name,
      x = "Mean yield change (kg/ha)"
    ) +
    coord_cartesian(clip = "off", xlim = c(0, max_importance)) +
    geom_shadowtext(
      data = data.frame(
        importance = rep(label_position, length(ordered_features)),
        feature = factor(ordered_features, levels = ordered_features),
        label = clean_features
      ),
      aes(x = importance, y = feature, label = label),
      color = "black",
      bg.color = "white",
      bg.r = 0.15,
      hjust = 1,
      size = 2,
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
  } else {
    modified_plot <- modified_plot + 
      theme(
        plot.title = element_blank()
      )
  }
  
  # Add stats only if requested
  if (show_stats) {
    # Create data frame for stats
    stats_data <- data.frame(
      x = max_importance * 0.05,
      y = 6.5,
      label = sprintf("N = %d\nR² = %.2f", n_obs, R2)
    )
    
    modified_plot <- modified_plot +
      geom_shadowtext(
        data = stats_data,
        aes(x = x, y = y, label = label),
        hjust = 0.05,
        vjust = 0,
        size = 2,
        color = "black",
        bg.color = "white",
        bg.r = 0.15
      )
  }
  
  return(modified_plot)
}


#' Create a multi-panel plot of hexbin SHAP value plots with CV importance plots
#'
#' This function creates a visualization that combines cross-validation importance plots with 
#' hexbin plots showing the relationship between feature values and their SHAP values. It handles
#' multiple datasets and ensures consistent color scales and formatting across all panels.
#'
#' @param data_list List of data frames, each containing features and their corresponding SHAP values
#' @param features_list List of feature names to plot for each dataset (without "SHAP_" prefix)
#' @param cv_importance_list List of cross-validation importance plots to include in the first column
#' @param row_names Optional names for each row (default: NULL)
#' @param title Main title for the combined plot (default: "")
#' @param show_correlation Whether to display correlation coefficients in plot titles (default: TRUE)
#' @param add_trend_line Whether to add GAM trend lines to hexbin plots (default: TRUE)
#' @param n_x_breaks Number of breaks on the x-axis (default: 4)
#' @param bins Number of bins for hexagonal binning (default: 30)
#' @param color_palette Color palette to use for hexbins (default: "viridis")
#' @param log_scale Whether to use log scale for hexbin counts (default: TRUE)
#' @param base_size Base font size for the plots (default: 8)
#' @param k Complexity parameter for the GAM smooth (default: 3)
#' @param custom_log_breaks Custom breaks for log scale (default: NULL)
#'
#' @return A combined patchwork plot object with CV importance and hexbin plots arranged in rows
#'
#' @examples
# plot_grid <- create_shap_hexbin_grid_with_cv(
#   data_list = shap_list,
#   features_list = features_list,
#   cv_importance_list = cv_importance_list,
#   row_names = NULL,
#   title = "",
#   show_correlation = FALSE,
#   add_trend_line = FALSE,
#   n_x_breaks = 4,
#   k = 3,
#   custom_log_breaks = custom_log_breaks
# )

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
  # Define a mapping of variables to their units
  variable_units <- list(
    "Rain.Days.L.0" = "(days)",
    "Rain.Days.L.1" = "(days)",
    "Rain.Days.L.5" = "(days)",
    "Rain.sum" = "(mm)",
    "Rain.sum.Dev.Mean" = "(mm)",
    "Rain.Max.RSeq.5" = "(days)",
    "Tmax.mean" = "(°C)",
    "Tmax.mean.Dev.Mean" = "(°C)",
    "ETo.sum" = "(mm)",
    "WBalance" = "(mm)",
    "Tmax.sum" = "(°C)",
    "Fraction_Rainy_Days" = "",
    "Fraction_Rainy_Days_Dev" = "",
    "Longest_Dry_Spell" = "(days)",
    "Total_Rainfall" = "(mm)",
    "Total_Rainfall_Dev" = "(mm)",
    "Avg_Max_Temp" = "(°C)",
    "Avg_Max_Temp_Dev" = "(°C)"
  )
  
  # Rest of the function as before
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
      
      temp_plot <- ggplot(plot_data, 
                          aes(x = feature_value, 
                              y = shap_value)) +
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
      
      # Get unit for the feature if available
      unit <- ""
      for (var_name in names(variable_units)) {
        if (grepl(var_name, feature, fixed = TRUE)) {
          unit <- variable_units[[var_name]]
          break
        }
      }
      
      # Add unit to x-axis label if available
      if (unit != "") {
        clean_x_label <- paste(clean_x_label, unit)
      }
      
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