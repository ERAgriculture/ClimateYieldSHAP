# Combines predicted vs observed yields for hexbin plots

combine_pred_obs_plots <- function(experiment, aez_list) {
  require(ggplot2)
  require(patchwork)
  require(dplyr)
  
  # Create empty list to store plots
  aez_plots <- list()
  
  # Create output directory for the combined plot
  output_dir <- paste0("outputs/", experiment, "/combined_plots")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # First, load all plots to determine common axis limits
  all_xlims <- c()
  all_ylims <- c()
  
  # Track if any plots were successfully loaded
  any_loaded <- FALSE
  
  for (aez in aez_list) {
    # Define the file path for the RDS file
    full_experiment_name <- paste0(experiment, "_", aez)
    rds_path <- file.path("outputs", full_experiment_name, "hexbin_plots", 
                          paste0(full_experiment_name, "_obs_vs_pred_hexbin.rds"))
    
    # Check if file exists
    if (file.exists(rds_path)) {
      any_loaded <- TRUE
      
      # Load the plot
      plot_obj <- readRDS(rds_path)
      
      # Try to extract axis limits from the plot
      plot_data <- ggplot_build(plot_obj)
      
      # Get coordinates from the plot
      if (!is.null(plot_data$layout$coord$limits)) {
        # For coord_fixed with explicit limits
        xlim <- plot_data$layout$coord$limits$x
        ylim <- plot_data$layout$coord$limits$y
      } else {
        # Fall back to panel params
        xlim <- plot_data$layout$panel_params[[1]]$x.range
        ylim <- plot_data$layout$panel_params[[1]]$y.range
      }
      
      if (!is.null(xlim) && !is.null(ylim)) {
        all_xlims <- c(all_xlims, xlim)
        all_ylims <- c(all_ylims, ylim)
      }
    }
  }
  
  # Check if any plots were loaded
  if (!any_loaded) {
    stop("No hexbin plots were found for the specified AEZs.")
  }
  
  # Determine common axis limits
  if (length(all_xlims) > 0 && length(all_ylims) > 0) {
    common_min <- min(c(all_xlims, all_ylims), na.rm = TRUE)
    common_max <- max(c(all_xlims, all_ylims), na.rm = TRUE)
    
    # Add a small buffer
    range_val <- common_max - common_min
    buffer <- range_val * 0.05
    common_min <- max(0, common_min - buffer)
    common_max <- common_max + buffer
    
    common_limits <- c(common_min, common_max)
  } else {
    # Default limits if limits can't be extracted
    common_limits <- c(0, 10000)
  }
  
  # Create round-number breaks with approximately 4 values
  step_size <- ceiling((common_limits[2] - common_limits[1]) / 3 / 1000) * 1000
  axis_breaks <- seq(0, ceiling(common_limits[2] / step_size) * step_size, by = step_size)
  
  # Now load plots again and update them with common limits
  for (aez in aez_list) {
    # Define the file path for the RDS file
    full_experiment_name <- paste0(experiment, "_", aez)
    rds_path <- file.path("outputs", full_experiment_name, "hexbin_plots", 
                          paste0(full_experiment_name, "_obs_vs_pred_hexbin.rds"))
    
    # Check if file exists
    if (file.exists(rds_path)) {
      # Load the plot
      plot_obj <- readRDS(rds_path)
      
      # Replace dot with space in AEZ name for the title
      title_text <- gsub("\\.", " ", aez)
      
      # Extract the original plot build data
      orig_build <- ggplot_build(plot_obj)
      
      # Identify which layers are text annotations
      text_layer_indices <- which(sapply(plot_obj$layers, function(x) inherits(x$geom, "GeomText")))
      
      # Create a modified version of the plot with new axis limits
      updated_plot <- plot_obj + 
        coord_fixed(ratio = 1, xlim = common_limits, ylim = common_limits) +
        scale_x_continuous(breaks = axis_breaks) +
        scale_y_continuous(breaks = axis_breaks) +
        labs(title = title_text) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12),
          axis.title.y = if (title_text %in% c("Warm Semiarid", "Cool Semiarid")) element_text(size = 10) else element_blank(),
          axis.title.x = if (title_text %in% c("Cool Semiarid", "Cool Subhumid", "Cool Humid")) element_text(size = 10) else element_blank(),
          plot.margin = margin(5, 15, 5, 5),
          axis.text.x = element_text(margin = margin(t = 5)),
          legend.position = "none"
        )
      
      # Based on the text found in the original plot
      for (i in text_layer_indices) {
        # Extract the label text
        label_text <- as.character(plot_obj$layers[[i]]$aes_params$label)
        
        updated_plot$layers[[3]] <- NULL
        

        # Check if it's an R² annotation
        if (grepl("R.*= ", label_text)) {
          # Add R² at standard position (upper right)
          updated_plot <- updated_plot +
            annotate("text", x = 0,
                     y = common_limits[2] * 0.9,
                     label = label_text, hjust = 0, size = 3.5)
        }
        # Check if it's an RMSE annotation
        else if (grepl("RMSE", label_text)) {
          # Add RMSE at standard position (below R²)
          updated_plot <- updated_plot +
            annotate("text", x = 0,
                     y = common_limits[2] * 0.9,
                     label = label_text, hjust = 0, size = 3.5)
        }
      }
      
      aez_plots[[aez]] <- updated_plot
      cat("Loaded and updated hexbin plot for", aez, "\n")
    } else {
      cat("Warning: Hexbin plot for", aez, "not found at path:", rds_path, "\n")
    }
  }
  
  # Create a common legend
  if (length(aez_plots) > 0) {
    # Extract legend from first plot with legend restored
    legend_plot <- aez_plots[[1]] + theme(legend.position = "bottom")
    legend <- get_legend(legend_plot)
  } else {
    legend <- NULL
  }
  
  # Arrange plots in a grid using patchwork
  n_plots <- length(aez_plots)
  if (n_plots <= 3) {
    ncol <- n_plots
    nrow <- 1
  } else if (n_plots <= 6) {
    ncol <- 3
    nrow <- ceiling(n_plots / 3)
  } else {
    ncol <- 4
    nrow <- ceiling(n_plots / 4)
  }
  
  # Combine plots with patchwork - without overall title
  combined_plot <- wrap_plots(aez_plots, ncol = ncol) &
    theme(plot.margin = margin(5, 15, 5, 5))
  
  # Add shared legend at the bottom with reduced spacing
  if (!is.null(legend)) {
    combined_plot <- combined_plot / legend + plot_layout(heights = c(20, 1))
  }
  
  # Save the combined plot
  ggsave(
    file.path(output_dir, paste0(experiment, "_hexbin_aez_comparison.png")),
    combined_plot,
    width = min(16, ncol * 5),
    height = min(10, nrow * 4.5 + 0.5),
    bg = "white"
  )
  
  # Also save as PDF
  ggsave(
    file.path(output_dir, paste0(experiment, "_hexbin_aez_comparison.pdf")),
    combined_plot,
    width = min(16, ncol * 5),
    height = min(10, nrow * 4.5 + 0.5),
    device = pdf
  )
  
  cat("Created and saved combined hexbin plot for all AEZs with common axis scales\n")
  
  return(combined_plot)
}

# Helper function to extract legend from a plot
get_legend <- function(p) {
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) {
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  return(NULL)
}