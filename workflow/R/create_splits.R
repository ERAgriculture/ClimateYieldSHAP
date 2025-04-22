create_splits <- function(data, target, train_prop = 0.8, n_cv_folds = 5, 
                          weighted = FALSE, weight_func = NULL, seed = NULL,
                          imp_column = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Create initial split
  initial_split <- initial_split(data, prop = train_prop, strata = !!sym(target))
  train_data <- training(initial_split)
  test_data <- testing(initial_split)
  
  # Add weights to training data if specified
  if (weighted) {
    if (is.null(weight_func)) {
      stop("weight_func must be provided when weighted is TRUE")
    }
    train_data <- train_data %>%
      mutate(
        case_weights = weight_func(train_data, imp_column),
        case_weights = importance_weights(case_weights)
      )
  }
  
  # Create CV folds
  cv_folds <- vfold_cv(train_data, v = n_cv_folds, strata = !!sym(target))
  
  # Return all split objects
  return(list(
    train_test_split = initial_split,
    train_data = train_data,
    test_data = test_data,
    cv_folds = cv_folds,
    weighted = weighted
  ))
}