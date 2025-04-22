train_simple_rf <- function(data, 
                            target_col,
                            formula,
                            rec = NULL,
                            mtry = 3,
                            trees = 500,
                            min_n = 10,
                            max.depth = 15,
                            seed = 123) {
  
  # Create the recipe
  set.seed(seed)
  if (is.null(rec)) {
    rec <- recipe(formula, data = data) %>%
      step_string2factor(all_nominal()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)
  }
  
  # Create RF spec with fixed parameters
  rf_spec <- rand_forest(
    mtry = mtry,
    trees = trees,
    min_n = min_n
  ) %>%
    set_engine("ranger", 
               importance = "permutation",
               max.depth = !!max.depth,
               seed = seed) %>%
    set_mode("regression")
  
  # Create workflow
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(rf_spec)
  
  # Fit the model on all training data
  fitted_wf <- wf %>%
    fit(data = data)
  
  return(fitted_wf)
}