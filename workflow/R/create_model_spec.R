create_model_spec <- function(model_type, 
                              mode = "regression", 
                              engine = NULL, 
                              tune_params = NULL,
                              stop_iter = NULL,
                              case_weights = NULL) {
  # Input validation
  valid_models <- c("linear_reg", "ridge", "lasso", "rf", "boost_tree", "svm", "gam")
  if (!model_type %in% valid_models) {
    stop("Invalid model_type. Choose from: ", paste(valid_models, collapse = ", "))
  }
  if (!mode %in% c("regression", "classification")) {
    stop("Invalid mode. Choose either 'regression' or 'classification'")
  }
  
  # Create model specification
  model_spec <- switch(model_type,
                       "linear_reg" = linear_reg(),
                       "ridge" = linear_reg(mixture = 0),
                       "lasso" = linear_reg(mixture = 1),
                       "rf" = rand_forest(),
                       "boost_tree" = boost_tree(),
                       "svm" = svm_rbf(),
                       "gam" = gen_additive_mod()
  )
  
  # Set mode and engine
  model_spec <- model_spec %>% set_mode(mode)
  if (is.null(engine)) {
    engine <- switch(model_type,
                     "linear_reg" = "lm", 
                     "ridge" = "glmnet", 
                     "lasso" = "glmnet",
                     "rf" = "ranger", 
                     "boost_tree" = "xgboost", 
                     "svm" = "kernlab", 
                     "gam" = "mgcv"
    )
  }
  
  # Set engine with additional parameters for specific model types
  if (model_type == "rf" && engine == "ranger") {
    model_spec <- model_spec %>% set_engine(engine, 
                                            importance = "permutation")
                                            # case.weights = case_weights)
  } else if (model_type == "boost_tree" && engine == "xgboost" && !is.null(stop_iter)) {
    model_spec <- model_spec %>% set_engine(engine, 
                                            stop_iter = stop_iter,
                                            verbose = TRUE,
                                            counts = FALSE)
  } else {
    model_spec <- model_spec %>% set_engine(engine)
  }
  
  # Set tunable parameters only if they exist
  if (!is.null(tune_params) && length(tune_params) > 0) {
    model_spec <- model_spec %>%
      set_args(!!!tune_params)
  }
  
  return(model_spec)
}
