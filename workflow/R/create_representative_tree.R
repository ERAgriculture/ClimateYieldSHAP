library(ranger)
library(rpart)
library(rpart.plot)
library(dplyr)

create_representative_tree <- function(workflow, train_data, test_data, max_depth = 5) {
  # Extract the ranger model
  rf_model <- workflow$fit$fit$fit
  
  # Get variable importance
  var_imp <- importance(rf_model)
  
  # Get tree information
  tree_info <- treeInfo(rf_model)
  
  # Summarize split variables and their average split points
  split_summary <- tree_info %>%
    group_by(splitvarName) %>%
    reframe(
      avg_splitval = mean(splitval, na.rm = TRUE),
      importance = var_imp[splitvarName[1]],  # Use the first occurrence of splitvarName
      count = n()
    ) %>%
    arrange(desc(importance)) %>%
    slice_head(n = max_depth)  # Select top variables based on importance
  
  # Create a formula for rpart, ensuring all variables exist in the data
  valid_vars <- intersect(split_summary$splitvarName, names(train_data))
  if (length(valid_vars) == 0) {
    stop("No valid variables found for creating the tree.")
  }
  formula_vars <- paste(valid_vars, collapse = " + ")
  formula_str <- paste("yield ~", formula_vars)
  
  # Fit an rpart model using the selected variables
  tryCatch({
    rpart_model <- rpart(as.formula(formula_str), 
                         data = train_data, 
                         maxdepth = max_depth,
                         control = rpart.control(minbucket = rf_model$min.node.size))
    
    # Get predictions on test data and calculate R-squared
    predictions <- predict(rpart_model, newdata = test_data)
    y_true <- test_data$yield  # Assuming 'yield' is the target variable
    ss_total <- sum((y_true - mean(y_true))^2)  # Total sum of squares
    ss_res <- sum((y_true - predictions)^2)     # Residual sum of squares
    r_squared <- 1 - (ss_res / ss_total)
    
    # Plot the tree with R-squared in the title
    rpart.plot(rpart_model, 
               extra = 101,  # Show all details
               box.palette = "auto",  # Color boxes by prediction value
               shadow.col = "gray",  # Add shadows to the boxes
               nn = TRUE,  # Show node numbers
               under = TRUE,  # Write split labels under the boxes
               tweak = 1.2,  # Expand text size
               main = paste("Representative Tree (Test R-squared:", round(r_squared, 3), ")"))
    
    return(rpart_model)
  }, error = function(e) {
    cat("Error in creating rpart model:", conditionMessage(e), "\n")
    cat("Formula used:", formula_str, "\n")
    return(NULL)
  })
}
