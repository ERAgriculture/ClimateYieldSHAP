# ERA Data Analysis with Bootstrap Resampling
# 
# This script contains the main analysis used for the preparation of the 
# manuscript
#
# This script performs a comprehensive analysis of agricultural yield data across different 
# Agro-Ecological Zones (AEZs) in Africa. Parameters for the analysis are read from YAML 
# configuration files located in the config folder. Rather than performing hyperparameter tuning 
# (which was done in workflow.R), this script focuses on uncertainty quantification through 
# bootstrap resampling to:
# 
# 1. Train Random Forest models on each AEZ using pre-identified optimal parameters
# 2. Perform bootstrap sampling (n=99) to quantify uncertainty in predictions and SHAP values
# 3. Calculate model performance metrics (RMSE, R², MAE) on training, test, and OOB samples
# 4. Generate SHAP-based feature importance and interaction plots with confidence intervals
# 5. Create visualizations of yield predictions vs. observations
#
# Key components:
# - Standardized feature names for consistency across analyses
# - Weighted sampling to account for site/study representation differences
# - Comprehensive model validation using both test sets and out-of-bag estimates
# - Uncertainty quantification through bootstrap resampling
# - Normalized feature importance visualization with confidence intervals
# - Performance comparison across different AEZs
#
# Input data contains agricultural yield data with climate variables, soil properties, 
# and various agricultural management practices measured across different sites in Africa.
# Results are saved as RDS files and visualizations in the outputs directory.

# start_total_time <- Sys.time()


dir.create("logs", recursive = TRUE, showWarnings = FALSE)


## Load required libraries
if (!identical(normalizePath(getwd()), 
               normalizePath("/Users/alvaro/My Drive (acarmonacabrero@gmail.com)/Atlas Alvaro/workflow"))){
  source('R/get_required_packages.R')
}

library(tidymodels)
library(tidyverse)
library(vip)
library(yaml)
library(plotly)
source('R/load_and_preprocess.R')
source('R/correlation_analysis.R')
source('R/create_splits.R')
source('R/create_model_spec.R')
source('R/create_recipe.R')
source('R/create_workflow.R')
source('R/tune_model.R')
source('R/train_and_test_final_model.R')
source('R/calculate_variable_importance.R')
source('R/create_yield_maps.R')
source('R/shap_mean_importance_plot.R')
source('R/create_normalized_shap_beeswarm_plot.R')
source('R/create_shap_interaction_plot.R')
source('R/weight_functions.R')
source('R/add_mask_column.R')
source('R/create_ale_plots.R')
source('R/create_simple_mask.R')
source('R/predictions_contour.R')
source('R/create_representative_tree.R')
source('R/combine_shap_plots.R')
source('R/save_plot_object.R')
source('R/compute_shap_values.R')
source('R/train_simple_rf.R')
source('R/estimate_metrics_simple_rf.R')
source('R/plot_bootstrap_shap.R')
source('R/plot_bootstrap_shap_gamCI.R')
source('R/summarize_boots_performance.R')
source('R/remap_formula.R')
source('R/normalize_features.R')
source('R/plot_aez_distribution.R')
source('R/plot_categorical_shap.R')
source('R/create_performance_table.R')
source('R/estimate_ind_cv_performance.R')
source("R/create_and_save_obs_pred_plot.R")
source("R/combine_pred_obs_plots.R")

### Set experiment name and read configuration
args <- commandArgs(trailingOnly = TRUE)
experiment <- args[1]
AEZ <- args[2]

AEZ_list <- c("Warm.Semiarid", "Warm.Subhumid", "Warm.Humid",
              "Cool.Semiarid", "Cool.Subhumid", "Cool.Humid")
# AEZ_list <- c("Warm.Semiarid")  # Run just this AEZ


### AEZ loop
for (AEZ in AEZ_list){
  experiment <- "no_outliers_eco_isqrt_nosoil"  # MANUAL SETTINGS
  experiment_t <- experiment

  if (is.na(AEZ)){
    AEZ <- "Warm.Semiarid"  # MANUAL SETTINGS
  }
  
  # Log file
  # sink(paste0("logs/", experiment, "_", AEZ, "_log_file.txt"), 
  #      append=TRUE, split=TRUE)
  
  config <- yaml::read_yaml(paste0("config/", experiment, "_config.yaml"))
  
  create_colour_features <- TRUE
  create_plotly <- TRUE
  
  n_bootstraps <- 99  # NUMBER OF BOOTSTRAPS
  
  # Feature mapping
  feature_mapping <- c(
    "Rain.Days.L.1" = "Fraction_Rainy_Days",
    "Mean_clay_tot_psa" = "Clay",
    "Mean_sand_tot_psa" = "Sand",
    "Mean_ecec.f" = "ECEC",
    "Mean_OC" = "OC",
    "Presowing.Rain.sum" = "Presowing_Rainfall",
    "Rain.sum" = "Total_Rainfall",
    "Rain.sum.Dev.Mean" = "Total_Rainfall_Dev",
    "Tmax.mean" = "Avg_Max_Temp",
    "Rain.Days.L.1.Dev.Mean" = "Fraction_Rainy_Days_Dev",
    "Rain.Max.RSeq.5" = "Longest_Dry_Spell",
    "Tmax.mean.Dev.Mean" = "Avg_Max_Temp_Dev",
    "yield" = "Yield"
  )
  
  
  ## Initialize bootstrap and full data model storage
  bootstrap_results <- list(
    train_metrics = list(),
    oob_metrics = list(),
    test_metrics = list(),
    MDA_importance = list(),
    shap_values = list()
  )
  full_results <- list(
    train_metrics = list(),
    oob_metrics = list(),
    test_metrics = list(),
    MDA_importance = list(),
    shap_values = list()
  )
  
  
  ## Data settings from config
  data_path <- config$data_path
  target <- config$target
  formula_string <- gsub("\\s+", " ", config$general_formula)
  formula_string <- gsub('"', '', formula_string)
  formula_string <- trimws(formula_string)
  general_formula <- as.formula(formula_string)
  general_formula <- remap_formula(formula_string, 
                                   feature_mapping)
  weighted <- config$weighted
  # Preprocessing settings
  imputation_method <- config$preprocessing$imputation_method
  lower_quantile <- config$preprocessing$lower_quantile
  upper_quantile <- config$preprocessing$upper_quantile
  # Data splitting settings
  train_prop <- config$splits$train_prop
  n_cv_folds <- config$splits$cv_folds
  cv_seed <- config$splits$seed
  
  
  ## Load and preprocess data
  data <- load_and_preprocess(data_path = data_path,
                              target_col = target,
                              cols_to_remove = NULL,
                              imputation_method = imputation_method,
                              lower_quantile = lower_quantile,
                              upper_quantile = upper_quantile)
  
  # data %>% 
  #   group_by(AEZ16simple) %>%
  #   summarise(mean = mean(yield))
  
  N_obs_table <- data %>% select(AEZ16simple) %>% table()
  AEZ_table_names <- names(N_obs_table)
  N_obs_df <- data.frame(
    AEZ = AEZ_list,
    N_obs = as.numeric(N_obs_table[AEZ_list])
  )
  
  plot_aez_distribution(data, experiment)
  
  existing_cols <- names(feature_mapping)[names(feature_mapping) 
                                          %in% names(data)]
  data <- data %>%
    rename(!!!setNames(existing_cols, feature_mapping[existing_cols]))
  target <- str_replace_all(target, feature_mapping)
  
  
  # List of features to plot
  feature_list <- all.vars(general_formula)[-1]
  
  normalized_data <- normalize_features(data, 
                                        feature_list)
  
  ## Subset by AEZ
  if (!is.null(AEZ)){
    data <- data %>%
      filter(AEZ16simple == AEZ)
    experiment <- paste0(experiment, "_", AEZ)
  }
  
  
  ### Iterative model tuning and analysis
  for (model_i in 2:length(config$models)){
    model_i = 2  # Only Random Forest

    if (config$models[[model_i]]$recipe_options$transformations) {
      data[[target]] <- log(data[[target]] + 1)
    }
    
    ### Weight calculation
    if (weighted){
      general_formula <- update(general_formula, . ~ . + case_weights)
      data <- data %>%
        mutate(weight_ID = paste(Site.Key, M.Year, Season, sep = "_"))
      weight_func <- assign_weight_function(config$weighted_method)
    }
    
    ## Split data
    set.seed(cv_seed)
    init_split <- initial_split(data,
                                prop = 0.8,
                                strata = !!sym(target))
    train_data <- training(init_split)
    test_data <- testing(init_split)
    
    ### Full data model analysis
    
    # Add weights to training data if specified
    if (weighted) {
      if (is.null(weight_func)) {
        stop("weight_func must be provided when weighted is TRUE")
      }
      train_data <- train_data %>%
        mutate(
          case_weights = weight_func(train_data, "weight_ID"),
          case_weights = importance_weights(case_weights)
        )
    }
    ## Create recipe
    recipe <- create_recipe(data = train_data, 
                           formula = general_formula,
                           one_hot = config$models[[model_i]]$recipe_options$one_hot,
                           standardize = config$models[[model_i]]$recipe_options$standardize,
                           normalize = config$models[[model_i]]$recipe_options$normalize,
                           augment = config$models[[model_i]]$recipe_options$augment,
                           variable_selection = config$models[[model_i]]$recipe_options$variable_selection,
                           parallel = config$models[[model_i]]$recipe_options$parallel)
    
    # Use this code to reverse the standardization/transformation of the data
    prepped_recipe <- prep(recipe, training = train_data)
    # See how the data looks after applying the recipe
    transformed_data <- bake(prepped_recipe, new_data = NULL)  # NULL for training data
    
    final_workflow <- train_simple_rf(data = train_data,
                                      target_col = target,
                                      formula = general_formula,
                                      rec = recipe,
                                      mtry = 3,
                                      trees = 1000,
                                      min_n = 25,
                                      max.depth = 7
    )
    all_metrics <- estimate_metrics_simple_rf(final_workflow,
                                              train_data,
                                              test_data,
                                              target_col = target)
    
    ## ALE plots and MDI importance or regression coefficients
    variable_importance <- calculate_variable_importance(
      trained_workflow = final_workflow,
      model_name = config$models[[model_i]]$name,
      output_dir = paste0("outputs/", experiment, "/full_results/")
    )
    
    ## MDA importance
    full_MDA_importance <- tibble(variable = variable_importance$Variable, 
                             MDA_importance = variable_importance$Importance)
    
    ## SHAP calculation
    shap_values <- compute_shap_values(final_workflow, train_data)
    columns_to_keep <- c(all.vars(general_formula)[1:(length(all.vars(general_formula))-1)],
                         colnames(select(shap_values, starts_with("SHAP"))),
                         c("Code", "Author", "DOI", "Country", "Site.Key", 
                           "Lat", "Lon", "M.Year", "N.obs"))
    shap_values <- shap_values %>%
      select(all_of(columns_to_keep))
    
    ## Observed vs predicted yields
    scatter_plot <- create_and_save_obs_pred_plot(
      workflow = final_workflow,
      test_data = test_data,
      target_col = target,
      experiment = experiment_t,
      AEZ = AEZ
    )
    
    ## Store full data model results
    full_results$feature_importance <- full_MDA_importance
    full_results$train_metrics <- all_metrics$train_metrics
    full_results$oob_metrics <- all_metrics$oob_metrics
    full_results$test_metrics <- all_metrics$test_metrics
    full_results$shap_values <- shap_values
    full_results$data <- list(normalized_data = tibble(normalized_data$normalized_data) %>%
                                filter(N.obs %in% train_data$N.obs),
                              norm_params = normalized_data$norm_params
                              )
    output_dir_results <- paste0("outputs/", experiment, "/full_results")
    dir.create(output_dir_results, showWarnings = FALSE, recursive = TRUE)
    saveRDS(full_results, file = file.path(output_dir_results, "full_results.rds"))
    
    
    ## Estimate CV SHAP importance
    cv_results <- estimate_ind_cv_performance(
      data = train_data,
      target_col = target,
      formula = general_formula,
      recipe = recipe,
      n_folds = n_cv_folds,
      experiment = experiment
    )
    cv_output_dir <- paste0("outputs/", experiment, "/full_results/")
    saveRDS(cv_results, file = file.path(cv_output_dir, "cv_results.rds"))
    
    ### Bootstrap loop
    for (bootstrap_i in 1:n_bootstraps) {
      start_time <- Sys.time()
      cat(paste0("Bootstrap ", bootstrap_i, "... \n"))
      set.seed(bootstrap_i)
      bootstrap_indices <- sample(nrow(train_data), size = nrow(train_data), replace = TRUE)
      bootstrap_data <- train_data[bootstrap_indices, ]
      
      # Add weights to training data if specified
      if (weighted) {
        if (is.null(weight_func)) {
          stop("weight_func must be provided when weighted is TRUE")
        }
        bootstrap_data <- bootstrap_data %>%
          mutate(
            case_weights = weight_func(bootstrap_data, "weight_ID"),
            case_weights = importance_weights(case_weights)
          )
      }
      
      ## Create recipe
      recipe = create_recipe(data = bootstrap_data, 
                             formula = general_formula,
                             one_hot = config$models[[model_i]]$recipe_options$one_hot,
                             standardize = config$models[[model_i]]$recipe_options$standardize,
                             normalize = config$models[[model_i]]$recipe_options$normalize,
                             augment = config$models[[model_i]]$recipe_options$augment,
                             variable_selection = config$models[[model_i]]$recipe_options$variable_selection,
                             parallel = config$models[[model_i]]$recipe_options$parallel)
      
      # Use this code to reverse the standardization/transformation of the data
      prepped_recipe = prep(recipe, training = bootstrap_data)
      # See how the data looks after applying the recipe
      transformed_data = bake(prepped_recipe, new_data = NULL)  # NULL for training data
      
      final_workflow <- train_simple_rf(data = bootstrap_data,
                                  target_col = target,
                                  formula = general_formula,
                                  rec = recipe,
                                  mtry = 3,
                                  trees = 500,
                                  min_n = 25,
                                  max.depth = 7
                                  )
      
      all_metrics <- estimate_metrics_simple_rf(final_workflow,
                                                bootstrap_data,
                                                test_data,
                                                target_col = target)
      
      bootstrap_results$train_metrics[[bootstrap_i]] <- all_metrics$train_metrics
      bootstrap_results$test_metrics[[bootstrap_i]] <- all_metrics$test_metrics
      bootstrap_results$oob_metrics[[bootstrap_i]] <- all_metrics$oob_metrics
      
      
      shap_values <- compute_shap_values(final_workflow, train_data)
      columns_to_keep <- c(all.vars(general_formula)[1:(length(all.vars(general_formula))-1)],
                           colnames(select(shap_values, starts_with("SHAP"))),
                           c("Code", "Author", "DOI", "Country", "Site.Key", 
                             "Lat", "Lon", "M.Year", "N.obs"))
      shap_values <- shap_values %>%
        select(all_of(columns_to_keep))
      bootstrap_results$shap_values[[bootstrap_i]] <- shap_values
      end_time <- Sys.time()
      cat(paste0("time: ", difftime(end_time, start_time, units = "secs"), " seconds\n"))
      }
    ### Save the list object
    output_dir_results <- paste0("outputs/", experiment, "/bootstrap_results")
    dir.create(output_dir_results, showWarnings = FALSE, recursive = TRUE)
    
    saveRDS(bootstrap_results, file = file.path(output_dir_results, "bootstrap_results.rds"))
  }
  
  ### Produce SHAP plots with bootstrapped data
  # List of features to plot
  feature_list <- all.vars(general_formula)[-1]
  if (weighted) feature_list <- feature_list[-length(feature_list)]
  
  # Output directory
  output_dir <- paste0("outputs/", experiment, "/bootstrap_plots/")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # MANUAL SETTINGS #
  # AEZ <- "Warm.Humid"
  # experiment <- "eco_isqrt_nosoil_Warm.Humid"
  # full_results <- readRDS(paste0('outputs/', experiment, '/full_results/full_results.rds'))
  # bootstrap_results <- readRDS(paste0('outputs/', experiment, '/bootstrap_results/bootstrap_results.rds'))
  
  # Loop through the features and color_by to create and save the plots
  # color_by adds visualization for interactive effects
  for (var in feature_list) {
    for (color_by in feature_list) {
      if (var != color_by) {
        # Strict 95% bands
        plot <- plot_bootstrap_shap(
          bootstrap_results = bootstrap_results,
          full_results = full_results,
          feature = var,
          color_by = color_by,
          confidence_level = 0.95,
          title = NULL,
          AEZ = AEZ
        )

        ggsave(
          filename = file.path(output_dir, paste0("shap_plot_", var, "_", color_by, ".png")),
          plot = plot,
          width = 8,
          height = 6,
          bg = "white"
        )
        saveRDS(object = plot,
                file = file.path(output_dir, paste0("shap_plot_", var, "_", color_by, ".RDS")))
        
        # GAM fit for bands
        plot <- plot_bootstrap_shap_gamCI(
          bootstrap_results = bootstrap_results,
          full_results = full_results,
          feature = var,
          color_by = color_by,
          confidence_level = 0.95,
          title = NULL,
          AEZ = AEZ,
          geom_k = 8
        )

        ggsave(
          filename = file.path(output_dir, paste0("GAMCI_shap_plot_", var, "_", color_by, ".png")),
          plot = plot,
          width = 8,
          height = 6,
          bg = "white"
        )
        saveRDS(object = plot,
                file = file.path(output_dir, paste0("GAMCI_shap_plot_", var, "_", color_by, ".RDS")))

        # Improved GAM fit for bands
        plot <- plot_bootstrap_shap_improved(
          bootstrap_results = bootstrap_results,
          full_results = full_results,
          feature = var,
          color_by = color_by,
          confidence_level = 0.95,
          title = NULL,
          AEZ = AEZ
        )
        ggsave(
          filename = file.path(output_dir, paste0("improv_shap_plot_", var, "_", color_by, ".png")),
          plot = plot,
          width = 8,
          height = 6,
          bg = "white"
        )
        saveRDS(object = plot,
                file = file.path(output_dir, paste0("improv_shap_plot_", var, "_", color_by, ".RDS")))
        
      }
    }
  }
  cat(paste0("SHAP plots done for: ", AEZ, "\n"))
  
  # MANUAL SETTINGS #
  # experiment <- "no_outliers_eco_isqrt_nosoil_Cool.Humid"
  # full_results <- readRDS(paste0('outputs/', experiment, '/full_results/full_results.rds'))
  # bootstrap_results <- readRDS(paste0('outputs/', experiment, '/bootstrap_results/bootstrap_results.rds'))
  
  summarize_boots_performance(full_results,
                              bootstrap_results, 
                              experiment, 
                              set = "train")
  summarize_boots_performance(full_results, 
                              bootstrap_results, 
                              experiment, 
                              set = "test")
  summarize_boots_performance(full_results, 
                              bootstrap_results, 
                              experiment, 
                              set = "oob")
}

# Create model performance table
performance_table <- create_performance_table(aez_list = AEZ_list, 
                                              experiment_t = experiment_t,
                                              base_dir = "model_output", 
                                              N_obs_df = N_obs_df)

# Make plot grid for predicted vs observed yields
combine_pred_obs_plots(experiment = experiment_t,
                       aez_list = AEZ_list)

ggsave(
  paste0('final_results/', experiment_t, '_scatter_plots.png')
       )
ggsave(
  paste0('final_results/', experiment_t, '_scatter_plots.pdf'),
  width = 180, 
  height = 160,
  units = "mm",
  dpi = 300
       )

# end_total_time <- Sys.time()
# cat(paste0("Done: ", n_bootstraps, " bootstraps in ", difftime(end_total_time, start_total_time, units = "secs"), " seconds\n"))
# sink()

