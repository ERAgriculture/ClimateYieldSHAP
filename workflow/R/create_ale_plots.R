library(tidymodels)
library(DALEXtra)
library(ggplot2)

create_ale_plots <- function(workflow, train_data, output_dir, num_features = NULL) {
  # Extract the model and recipe from the workflow
  model <- extract_fit_parsnip(workflow)
  recipe <- extract_recipe(workflow)
  
  # Prepare the data
  prepared_data <- bake(recipe, new_data = train_data)
  
  # Get the target variable name
  target_var <- outcome_names(recipe)
  
  # Create an explainer object
  explainer <- explain_tidymodels(
    model = workflow,
    data = prepared_data %>% select(-all_of(target_var)),
    y = prepared_data[[target_var]],
    label = "model"
  )
  
  # Get feature names
  feature_names <- setdiff(names(prepared_data), target_var)
  
  # If num_features is specified, select the top features based on permutation importance
  if (!is.null(num_features)) {
    vip_result <- vip::vip(model, num_features = num_features)
    top_features <- vip_result$data$Variable[1:num_features]
    feature_names <- intersect(feature_names, top_features)
  }
  
  # Create output directory
  dir.create(file.path(output_dir, "ale_plots"), showWarnings = FALSE, recursive = TRUE)
  
  # Define a custom theme with white background
  white_theme <- theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5),
      legend.background = element_rect(fill = "white", color = NA)
    )
  
  # Calculate and plot ALE for each feature
  ale_plots <- list()
  for (feature in feature_names) {
    ale <- model_profile(explainer, variables = feature, type = "accumulated")
    
    # Extract ALE data
    ale_data <- ale$agr_profiles
    
    # Create the base ALE plot
    p <- ggplot(ale_data, aes(x = `_x_`, y = `_yhat_`)) +
      geom_line() +
      ggtitle(paste("ALE Plot for", feature)) +
      xlab(feature) +
      ylab("ALE effect") +
      white_theme
    
    # Add rug plot
    feature_data <- prepared_data[[feature]]
    rug_data <- data.frame(x = feature_data)
    p <- p + geom_rug(data = rug_data, aes(x = x), 
                      inherit.aes = FALSE, alpha = 0.1, 
                      length = unit(0.03, "npc"), sides = "b")
    
    # Save the plot
    ggsave(
      filename = file.path(output_dir, "ale_plots", paste0("ale_", feature, ".png")),
      plot = p,
      width = 10,
      height = 6,
      bg = "white"  # Ensure white background when saving
    )
    
    ale_plots[[feature]] <- p
  }
  
  return(ale_plots)
}