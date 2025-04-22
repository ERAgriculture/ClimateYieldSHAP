library(tidymodels)
library(vip)
library(ggplot2)
library(pdp)

calculate_variable_importance <- function(trained_workflow, model_name, output_dir = "outputs") {
  
  # Create model-specific output directory
  model_output_dir <- file.path(output_dir, model_name)
  dir.create(model_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (model_name %in% c("linear_reg", "ridge", "lasso")) {
    # For linear models
    model <- extract_fit_parsnip(trained_workflow)
    
    # Get coefficients
    coef_df <- tidy(model) %>%
      filter(term != "(Intercept)") %>%
      mutate(abs_estimate = abs(estimate)) %>%
      arrange(desc(abs_estimate))
    
    # Plot coefficients
    p <- ggplot(coef_df, aes(x = reorder(term, abs_estimate), y = estimate)) +
      geom_col() +
      coord_flip() +
      labs(title = paste("Coefficient Plot -", model_name),
           x = "Variables", 
           y = "Coefficient Value")
    
    print(p)
    ggsave(file.path(model_output_dir, "coefficient_plot.png"), p, width = 10, height = 8)
    
    # Save coefficients
    write_csv(coef_df, file.path(model_output_dir, "coefficients.csv"))
    
    return(coef_df)
    
  } else if (model_name %in% c("rf", "boost_tree")) {
    # For tree-based models
    
    # Calculate variable importance
    if (model_name == "boost_tree") {
      vi <- trained_workflow %>%
        extract_fit_parsnip() %>%
        vip(num_features = 20, geom = "point")
      
      vi_data <- vi$data %>% arrange(desc(Importance))
    }
    
    if (model_name == "rf") {
      vi <- trained_workflow %>%
        extract_fit_parsnip() %>%
        vip(num_features = 20, geom = "point", scale = TRUE)
      
      # Extract the data and rescale to sum to 100
      vi_data <- vi$data
      vi_data$Importance <- vi_data$Importance / sum(vi_data$Importance) * 100
      
      # Sort by importance
      vi_data <- vi_data %>% arrange(desc(Importance))
    }
    
    # Plot variable importance
    p <- ggplot(vi_data, aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_col() +
      coord_flip() +
      labs(title = paste("Variable Importance Plot -", model_name),
           x = "Variables", y = "Importance")
    
    print(p)
    ggsave(file.path(model_output_dir, "importance_plot.png"), p, width = 10, height = 8)
    
    # Save importance scores
    write_csv(vi_data, file.path(model_output_dir, "importance_scores.csv"))
    
    ### Partial dependence plots
    # Generate partial dependence plots for all variables
    vars <- vi_data$Variable
    
    # Extract the predictors data
    predictors_data <- extract_mold(trained_workflow)$predictors
    
    for (var in vars) {
      tryCatch({
        pdp <- partial(extract_fit_parsnip(trained_workflow), 
                       pred.var = var, 
                       train = predictors_data,
                       type = "regression")
        
        # Get the original data for the variable
        var_data <- predictors_data[[var]]
        
        # Calculate y-axis limits based on the actual range of partial dependence values
        y_min <- min(pdp$yhat)
        y_max <- max(pdp$yhat)
        y_range <- y_max - y_min
        lower_limit <- y_min - 0.05 * y_range  # Subtract 5% from the lowest value
        upper_limit <- y_max + 0.05 * y_range  # Add 5% to the highest value for symmetry
        
        # Create custom plot with enhanced rug and white background
        p_pdp <- ggplot() +
          # Add the partial dependence line
          geom_line(data = pdp, aes(x = .data[[var]], y = yhat)) +
          # Add an enhanced rug plot
          geom_rug(data = data.frame(x = var_data), aes(x = x), 
                   sides = "b", alpha = 0.1, length = unit(0.05, "npc"), inherit.aes = FALSE) +
          labs(title = paste("Partial Dependence Plot -", var, "-", model_name),
               x = var, 
               y = "Partial Dependence") +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(hjust = 0.5)
          ) +
          # Use coord_cartesian to set y-axis limits
          coord_cartesian(ylim = c(lower_limit, upper_limit))
        
        print(p_pdp)
        ggsave(file.path(model_output_dir, paste0("pdp_", var, ".png")), p_pdp, width = 10, height = 6, bg = "white")
      }, error = function(e) {
        warning(paste("Could not generate PDP for variable:", var, "- Error:", e$message))
      })
    }
    
    return(vi_data)
  } else {
    stop("Unsupported model type")
  }
}
