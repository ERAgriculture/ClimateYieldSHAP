# Combine SHAP plots in a 3 by 3 grid

library(ggplot2)
library(patchwork)
library(dplyr)
library(stringr)

# Helper function to clean labels
clean_label <- function(text) {
  str_replace_all(text, "_", " ")
}

combine_shap_plots_3x3 <- function(shap_plots, rainfall_plots, temp_plots, main_title, AEZ_temp, row_list) {
  # Input validation
  if (length(shap_plots) != 3 || length(rainfall_plots) != 3 || length(temp_plots) != 3) {
    stop("Each type of plot (SHAP, rainfall, temperature) must have exactly 3 plots for the 3 climate zones")
  }
  
  # Function to safely get y range
  safe_y_range <- function(plot) {
    y_values <- layer_data(plot)$y
    y_values <- y_values[is.finite(y_values)]
    if (length(y_values) == 0) return(c(NA, NA))
    quantiles <- quantile(y_values, probs = c(0.001, 0.999), na.rm = TRUE)
    c(max(min(y_values), quantiles[1]), min(max(y_values), quantiles[2]))
  }
  
  # Function to safely get x range
  safe_x_range <- function(plot) {
    x_values <- layer_data(plot)$x
    x_values <- x_values[is.finite(x_values)]
    if (length(x_values) == 0) return(c(NA, NA))
    range(x_values, na.rm = TRUE)
  }
  
  # Get common x-axis ranges for feature importance
  shap_x_max <- max(unlist(lapply(shap_plots, function(x) max(x$data$importance))))
  shap_x_min <- 0
  
  # Get common x-axis ranges for rainfall and temperature plots
  rainfall_x_ranges <- lapply(rainfall_plots, safe_x_range)
  rainfall_x_min <- min(sapply(rainfall_x_ranges, `[`, 1), na.rm = TRUE)
  rainfall_x_max <- max(sapply(rainfall_x_ranges, `[`, 2), na.rm = TRUE)
  
  temp_x_ranges <- lapply(temp_plots, safe_x_range)
  temp_x_min <- min(sapply(temp_x_ranges, `[`, 1), na.rm = TRUE)
  temp_x_max <- max(sapply(temp_x_ranges, `[`, 2), na.rm = TRUE)
  
  # Get common y-axis range for rainfall plots
  rainfall_ranges <- lapply(rainfall_plots, safe_y_range)
  rainfall_ranges <- rainfall_ranges[!is.na(rainfall_ranges)]
  if (length(rainfall_ranges) > 0) {
    y_min_rain <- min(sapply(rainfall_ranges, `[`, 1), na.rm = TRUE)
    y_max_rain <- max(sapply(rainfall_ranges, `[`, 2), na.rm = TRUE)
    rain_range <- y_max_rain - y_min_rain
    rain_y_range <- c(y_min_rain - 0.05 * rain_range, y_max_rain + 0.05 * rain_range)
  }
  
  # Get common y-axis range for temperature plots
  temp_ranges <- lapply(temp_plots, safe_y_range)
  temp_ranges <- temp_ranges[!is.na(temp_ranges)]
  if (length(temp_ranges) > 0) {
    y_min_temp <- min(sapply(temp_ranges, `[`, 1), na.rm = TRUE)
    y_max_temp <- max(sapply(temp_ranges, `[`, 2), na.rm = TRUE)
    temp_range <- y_max_temp - y_min_temp
    temp_y_range <- c(y_min_temp - 0.05 * temp_range, y_max_temp + 0.05 * temp_range)
  }
  
  # Process SHAP importance plots with invisible legend placeholder
  shap_datalist <- lapply(shap_plots, function(x) {
    data <- tibble(x$data)
    data$feature <- clean_label(data$feature)
    data
  })
  
  shap_plots <- lapply(seq_along(shap_datalist), function(i) {
    cleaned_plot <- ggplot(shap_datalist[[i]], 
           aes(x = importance, y = reorder(feature, importance))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "", y = "") +
      theme(axis.text.y = element_text(size = 10)) +
      labs(y = paste0(substr(AEZ_temp, 1, 4), " ", row_list[i])) +
      xlim(c(shap_x_min, shap_x_max)) + 
      labs(x = paste0(substr(AEZ_temp, 1, 4), " ", row_list[i])) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 5, if(i == 3) 30 else 5, 5)
      )
    
    if (i == 3) {
      # Add invisible legend for spacing
      cleaned_plot <- cleaned_plot + 
        theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.3, "cm"),
          legend.box.margin = margin(t = 10, b = 10),
          legend.title = element_text(size = 8, color = "white"),
          legend.text = element_text(size = 8, color = "white")
        ) +
        xlab("| Mean effect | (kg/ha)")
    } else {
      cleaned_plot <- cleaned_plot + theme(legend.position = "none")
    }
    
    if (i == 1) {
      cleaned_plot <- cleaned_plot +
        ggtitle("Feature Importance") +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
    } else {
      cleaned_plot <- cleaned_plot + ggtitle(NULL)
    }
    
    if (i != 3) {
      cleaned_plot <- cleaned_plot +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank())
    }
    
    cleaned_plot
  })
  
  # Process rainfall plots
  rainfall_plots <- lapply(seq_along(rainfall_plots), function(i) {
    cleaned_plot <- rainfall_plots[[i]] + 
      scale_color_gradient(
        low = "blue", 
        high = "red",
        name = "Total Rainfall Dev"
      ) +
      labs(x = "Total Rainfall (mm)",
           y = "Yield change (kg/ha)") +
      coord_cartesian(ylim = rain_y_range, xlim = c(rainfall_x_min, rainfall_x_max)) +
      scale_y_continuous(limits = rain_y_range, oob = scales::squish) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 5, if(i == 3) 30 else 5, 5)
      )
    
    if (i == 3) {
      cleaned_plot <- cleaned_plot + 
        theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.3, "cm"),
          legend.box.margin = margin(t = 10, b = 10),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        )
    } else {
      cleaned_plot <- cleaned_plot + theme(legend.position = "none")
    }
    
    if (i == 1) {
      cleaned_plot <- cleaned_plot +
        ggtitle("Total Rainfall Effect") +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
    } else {
      cleaned_plot <- cleaned_plot + ggtitle(NULL)
    }
    
    if (i != 3) {
      cleaned_plot <- cleaned_plot +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank())
    }
    
    cleaned_plot
  })

    
  # Process temperature plots
  temp_plots <- lapply(seq_along(temp_plots), function(i) {
    cleaned_plot <- temp_plots[[i]] + 
      scale_color_gradient(
        low = "blue", 
        high = "red",
        name = "Max Temp Dev"
      ) +
      labs(x = "Max Temp (°C)",
         y = "Yield change (kg/ha)") +
      coord_cartesian(ylim = temp_y_range, xlim = c(temp_x_min, temp_x_max)) +
      scale_y_continuous(limits = temp_y_range, oob = scales::squish) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 5, if(i == 3) 30 else 5, 5)
      )
    
    if (i == 3) {
      cleaned_plot <- cleaned_plot + 
        theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.3, "cm"),
          legend.box.margin = margin(t = 10, b = 10),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)
        )
    } else {
      cleaned_plot <- cleaned_plot + theme(legend.position = "none")
    }
    
    if (i == 1) {
      cleaned_plot <- cleaned_plot +
        ggtitle("Max Temp Effect") +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
    } else {
      cleaned_plot <- cleaned_plot + ggtitle(NULL)
    }
    
    if (i != 3) {
      cleaned_plot <- cleaned_plot +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank())
    }
    
    cleaned_plot
  })
  
  # Combine all plots using patchwork with specific layout guide
  combined_plot <- (
    (shap_plots[[1]] | rainfall_plots[[1]] | temp_plots[[1]]) /
      (shap_plots[[2]] | rainfall_plots[[2]] | temp_plots[[2]]) /
      (shap_plots[[3]] | rainfall_plots[[3]] | temp_plots[[3]])
  ) +
    plot_layout(
      guides = "keep",
      heights = c(1, 1, 1.2)  # Give slightly more height to bottom row for legends
    ) +
    plot_annotation(
      title = main_title,
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.margin = margin(20, 20, 20, 20)
      )
    )
  
  return(combined_plot)
}