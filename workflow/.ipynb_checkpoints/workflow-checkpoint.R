# TODO:
# Be sure the framework works with the default values (no tuning)
# Categorize treatments and varieties

dir.create("logs", recursive = TRUE, showWarnings = FALSE)
sink("logs/log_file.txt", append=TRUE, split=TRUE)

# Load required libraries
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
source('R/shap_mean_importance.R')
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


### Set experiment name and read configuration
args <- commandArgs(trailingOnly = TRUE)
experiment <- args[1]
AEZ <- args[2]

if (is.na(experiment)){
  experiment <- "data_isqrt_nosoil"
}
if (is.na(AEZ)){
  AEZ <- NULL
  AEZ <- "Warm.Subhumid"  # Set to NULL for the entirity of Africa
  # # table(data$AEZ16simple)
  # Cool.Humid Cool.Semiarid Cool.Subhumid    Warm.Humid 
  # 249           287           599           276 
  # Warm.Semiarid Warm.Subhumid 
  # 508          3014 
}

config <- yaml::read_yaml(paste0("config/", experiment, "_config.yaml"))

create_colour_features <- TRUE
create_plotly <- TRUE
# Degree for the polynomial line in the SHAP plots
poly_degree <- 2  # NULL for not fitting a line


### Data settings from config
data_path = config$data_path
target = config$target
formula_string <- gsub("\\s+", " ", config$general_formula)
formula_string <- gsub('"', '', formula_string)  # Remove all quotation marks
formula_string <- trimws(formula_string)  # Remove leading and trailing whitespace
general_formula <- as.formula(formula_string)
weighted <- config$weighted
# Preprocessing settings
imputation_method = config$preprocessing$imputation_method
lower_quantile = config$preprocessing$lower_quantile
upper_quantile = config$preprocessing$upper_quantile
# Data splitting settings
train_prop = config$splits$train_prop
n_cv_folds = config$splits$cv_folds
cv_seed = config$splits$seed

### Load and preprocess data
data <- load_and_preprocess(data_path = data_path,
                            target_col = target,
                            cols_to_remove = NULL,
                            imputation_method = imputation_method,
                            lower_quantile = lower_quantile,
                            upper_quantile = upper_quantile)

### Select an AEZ
if (!is.null(AEZ)){
  data <- data %>%
    filter(AEZ16simple == AEZ)
  experiment <- paste0(experiment, "_", AEZ)
}

# SHAP colour features
plotly_additional_vars <- c()
if (create_colour_features){
  colour_features <- c("TorC", "M.Year", "Country", "Site.Type",
                       "Elevation", "Mean_ecec.f",
                       "Mean_sand_tot_psa", "AF_trt", "GM_trt", "IF_trt",
                       "IC_trt", "Mu_trt", "OF_trt", "IV_trt", "WH_trt",
                       "Rot_trt", "Till_trt",  "Soil.Texture.USDA",
                       "low_high_yields")  #, "site1_mask")
  
  plotly_additional_vars <- c("Site.Key", "Country", "Soil.Texture.USDA", "Site.ID")
  
  data <- add_mask_column(data, pattern="^a",
                          new_column_name="AF_trt", true_value="Agroforestry")
  data <- add_mask_column(data, pattern="^b1[12]\\.[12]",
                          new_column_name="GM_trt", true_value="Green Manure")
  data <- add_mask_column(data, pattern="^b(1[6-9]|2[0-3])",
                          new_column_name="IF_trt", true_value="Inorg. Fert.")
  data <- add_mask_column(data, pattern="^b25$|^b50",
                          new_column_name="IC_trt", true_value="Intercropping")
  data <- add_mask_column(data, pattern="^b27",
                          new_column_name="Mu_trt", true_value="Mulching")
  data <- add_mask_column(data, pattern="^b30$",
                          new_column_name="OF_trt", true_value="Org. Fert.")
  data <- add_mask_column(data, pattern="^b69$",
                          new_column_name="IV_trt", true_value="Improved Var.",
                          false_value="Unimproved Var.")
  data <- add_mask_column(data, pattern="^b70|^b71",
                          new_column_name="WH_trt", true_value="Water Harvest")
  data <- add_mask_column(data, pattern="^b43$",
                          new_column_name="Rot_trt", true_value="Rotation")
  # data <- create_simple_mask(data,
  #                            column = Site.Key, 
  #                            value = "12.6940 -7.0150 B250", 
  #                            new_column_name = site1_mask)
  
  data <- data %>%
    mutate(
      low_high_yields = case_when(
        is.na(yield) ~ "NA",
        yield >= quantile(data$yield, 0.9, na.rm = TRUE) ~ "High yields",
        yield <= quantile(data$yield, 0.1, na.rm = TRUE) ~ "Low yields",
        TRUE ~ NA_character_
      )
    )
  
  data <- data %>%
    mutate(
      Till_trt = case_when(
        str_detect(T1, "^b38$") | str_detect(T2, "^b38$") | str_detect(T3, "^b38$") ~ "NT",
        str_detect(T1, "^b39$") | str_detect(T2, "^b39$") | str_detect(T3, "^b39$") ~ "RT",
        str_detect(T1, "^h6$") | str_detect(T2, "^h6$") | str_detect(T3, "^h6$") ~ "CT",
        TRUE ~ "No data"
      )
    )
}


### Iterative model tuning and analysis
for (model_i in 1:length(config$models)){
  # The recipe is only standardizing/normalizing predictors ATM
  if (config$models[[model_i]]$recipe_options$transformations) {
    data[[target]] <- log(data[[target]] + 1)
  }
  
  if (weighted){
    general_formula <- update(general_formula, . ~ . + case_weights)
    data <- data %>%
      mutate(weight_ID = paste(Site.Key, M.Year, Season, sep = "_"))
  }
  
  ### Split data
  splits <- create_splits(data,
                          target = target,
                          train_prop = train_prop,
                          n_cv_folds = n_cv_folds,
                          weighted = weighted,
                          weight_func = log_inverse_weight,
                          seed = cv_seed,
                          imp_column = "weight_ID")
  
  train_test_split <- splits$train_test_split
  train_data <- splits$train_data
  test_data <- splits$test_data
  cv_folds <- splits$cv_folds
  
  ### Create recipe
  recipe = create_recipe(data = train_data, 
                         formula = general_formula,
                         one_hot = config$models[[model_i]]$recipe_options$one_hot,
                         standardize = config$models[[model_i]]$recipe_options$standardize,
                         normalize = config$models[[model_i]]$recipe_options$normalize,
                         augment = config$models[[model_i]]$recipe_options$augment,
                         variable_selection = config$models[[model_i]]$recipe_options$variable_selection,
                         parallel = config$models[[model_i]]$recipe_options$parallel)
  
  # Use this code to reverse the standardization/transformation of the data
  prepped_recipe = prep(recipe, training = train_data)
  # See how the data looks after applying the recipe
  transformed_data = bake(prepped_recipe, new_data = NULL)  # NULL for training data
  
  ### Define tuning parameters
  tuning_params <- setNames(replicate(length(config$models[[model_i]]$tune_params), 
                                      tune(), 
                                      simplify = FALSE), 
                            names(config$models[[model_i]]$tune_params))
  
  ### Create model specification
  if (config$models[[model_i]]$name == "boost_tree")
  {
    stop_iter <- config$models[[model_i]]$fixed_params$stop_iter
  } else{
    stop_iter <- NULL
  }
  
  model_spec <- create_model_spec(model_type = config$models[[model_i]]$name, 
                                  mode = config$models[[model_i]]$mode,
                                  engine = config$models[[model_i]]$engine,
                                  tune_params = tuning_params,
                                  stop_iter = stop_iter)
  
  ### Create workflow
  wf <- create_workflow(recipe = recipe,
                        model_spec = model_spec,
                        weighted = weighted)
  
  ### Define the hyperparameter grid
  # Modify code as the code below if the current code fails
  # rf_grid <- expand.grid(
  #   mtry = floor(seq(1, ncol(train_data) - 1, length.out = 5)),
  #   min_n = c(2, 5, 10)
  # )
  if (length(tuning_params) == 1){
    config_grid <- data.frame(
      penalty = as.vector(unlist(config$models[[model_i]]$tune_params)))
  } else {
    config_grid <- do.call(expand.grid, config$models[[model_i]]$tune_params)}
  
  ### Train and evaluate model
  set.seed(123)
  tune_result <- tune_model(
    workflow = wf,
    resamples = cv_folds,
    grid = config_grid,
    metrics = metric_set(rmse, rsq, mae),
    control = control_grid(save_pred = TRUE),
    verbose = TRUE,
    model_name = config$models[[model_i]]$name,
    output_dir = paste0("model_output/", experiment, "/tuning")
  )
  
  ### Collect the cross-validation metrics
  cv_metrics <- collect_metrics(tune_result)
  print(cv_metrics)
  
  ### Select the best model
  best_model <- select_best(tune_result, 
                            metric = "rsq")
  
  ### Train and test final model
  set.seed(123)
  final_workflow = train_and_test_final_model(workflow = wf, 
                                              target_col = target,
                                              best_model = best_model,
                                              train_data = train_data, 
                                              test_data = test_data, 
                                              model_name = config$models[[model_i]]$name,
                                              output_dir = paste0("model_output/", experiment, "/final_workflows"))
  
  ### Variable importance (MDA & regression coefficients)
  variable_importance <- calculate_variable_importance(
    trained_workflow = final_workflow,
    model_name = config$models[[model_i]]$name,
    output_dir = paste0("outputs/", experiment)
  )
  
  ### Create yield maps
  yield_maps <- create_yield_maps(
    data = bind_rows(train_data, test_data),  # Combine train and test data
    predictions = bind_rows(
      predict(final_workflow, new_data = train_data),
      predict(final_workflow, new_data = test_data)
    ),
    longitude = "Longitude",
    latitude = "Latitude",
    actual_yield = "yield",
    output_dir = paste0("outputs/", experiment, "/", config$models[[model_i]]$name, "/maps")
  )
  
  ### Create ALE plots
  if (config$models[[model_i]]$name %in% c("boost_tree", "rf")) {
    require("DALEXtra")
    ale_plots <- create_ale_plots(
      workflow = final_workflow,
      train_data = train_data,
      output_dir = paste0("outputs/", experiment, "/", config$models[[model_i]]$name),
      num_features = 10  # Adjust this to show more or fewer features
    )
  }
  
  
  ## Create SHAP plots
  if (config$models[[model_i]]$name %in% c("boost_tree", "rf")) {
    require("tidymodels")
    require("vip")
    require("fastshap")
    require("ggplot2")
    require("dplyr")
    require("treeshap")
    require("ggbeeswarm")
    
    # List of main features and secondary features
    if (config$models[[model_i]]$name == "rf"){
      feature_names <- final_workflow$fit$fit$fit$forest$independent.variable.names
    }
    
    dir.create(paste0("outputs/", experiment, "/",config$models[[model_i]]$name, "/shap_plots"), showWarnings = FALSE, recursive = TRUE)
    
    # This plot requires standardized/normalized data
    # ## Individual feature Beeswarm plot
    shap_beeswarm <- create_normalized_shap_beeswarm_plot(
      workflow = final_workflow,
      new_data = train_data,
      num_features = 10)
    ggsave(paste0("outputs/", experiment, "/",config$models[[model_i]]$name,
                  "/shap_plots/shap_beeswarm_plot.png"), shap_beeswarm, width = 12, height = 8)
    save_plot_object(shap_beeswarm, 
                     paste0("outputs/", experiment, "/",config$models[[model_i]]$name, "/shap_plots"),
                     "shap_beeswarm_plot")

    # SHAP mean variable importance    
    shap_plot <- shap_mean_importance(
      workflow = final_workflow,
      new_data = train_data,
      num_features = 10
    )
    ggsave(paste0("outputs/", experiment, "/", config$models[[model_i]]$name, 
                  "/shap_plots/shap_mean_importance.png"), shap_plot, width = 12, height = 8)
    save_plot_object(shap_plot, 
                     paste0("outputs/", experiment, "/", config$models[[model_i]]$name, "/shap_plots"),
                     "shap_mean_importance")
    print("SHAP variable importance: Done")
    ## Interaction plots
    for (main_feature in feature_names) {
      for (secondary_feature in feature_names) {
        if (main_feature != secondary_feature) {
          
          shap_interaction <- create_shap_interaction_plot(
            workflow = final_workflow,
            new_data = train_data,
            feature1 = main_feature,  # Variable on X-axis
            feature2 = secondary_feature,    # Variable on colorgradient
            poly_degree = 2,
            plotly_additional_vars = plotly_additional_vars
          )
          
          ggsave(paste0("outputs/", experiment, "/",config$models[[model_i]]$name,
                        "/shap_plots/shap_interaction_", main_feature, "_", secondary_feature, ".png"),
                 shap_interaction$plot, width = 10, height = 8)
          save_plot_object(shap_interaction$plot, 
                           paste0("outputs/", experiment, "/",config$models[[model_i]]$name, "/shap_plots"),
                           paste0("shap_interaction_", main_feature, "_", secondary_feature))
          
          if (create_plotly){
            # Convert to interactive plotly plot
            p <- ggplotly(shap_interaction$plot)
            p
            # # Save the interactive plot as an HTML file
            dir.create(paste0("outputs/", experiment, "/", config$models[[model_i]]$name, "/int_plots"), showWarnings = FALSE, recursive = TRUE)
            htmlwidgets::saveWidget(p,
                                    file = paste0(
                                      "outputs/", experiment, "/", 
                                      config$models[[model_i]]$name,
                                      "/int_plots/shap_interaction_",
                                      main_feature, "_", secondary_feature, ".html")
            )
          }
          " TEST BELOW. NEED TO DEFINE CONDITION FOR THIS "
          beeswarm_plot1 <- NULL
          beeswarm_plot2 <- NULL
          beeswarm_plot3 <- NULL

          if (main_feature == "Tmax.mean.Dev.Mean" & secondary_feature == "Rain.sum.Dev.Mean") {
              beeswarm_plot1 <- shap_interaction$plot
          } else if (main_feature == "Rain.sum.Dev.Mean" & secondary_feature == "Tmax.mean") {
              beeswarm_plot2 <- shap_interaction$plot
          } else if (main_feature == "Tmax.mean" & secondary_feature == "Rain.Days.L.1.Dev.Mean") {
              beeswarm_plot3 <- shap_interaction$plot
          }

          if (!is.null(beeswarm_plot1) && !is.null(beeswarm_plot2) && !is.null(beeswarm_plot3)) {
          combined_plot <- combine_shap_plots(
              shap_plot,
              list(beeswarm_plot1, beeswarm_plot2, beeswarm_plot3),
              title = paste(AEZ, experiment)
          )
              ggsave(paste0("outputs/", experiment, "/", config$models[[model_i]]$name,
                            "/combined_shap_plot.png"), combined_plot, width = 12, height = 10)
                     } else {
                         print("Not all beeswarm plots are available. Unable to create combined plot.")
                     }
          " TEST ABOVE. NEED TO DEFINE CONDITION FOR THIS "
          
          
          ### Contour plot for predictions
          pred_contour <- predictions_contour(
            workflow = final_workflow,
            feature1 = main_feature,
            feature2 = secondary_feature,
            data = train_data,
            n = 10
          )
          dir.create(paste0("outputs/", experiment, "/", config$models[[model_i]]$name,
                            "/pred_plots/"), recursive = TRUE, showWarnings = FALSE)
          ggsave(paste0("outputs/", experiment, "/",config$models[[model_i]]$name,
                        "/pred_plots/pred_contour_", main_feature, "_", secondary_feature, ".png"),
                 pred_contour, width = 10, height = 8, bg = "white")
          save_plot_object(pred_contour, 
                           paste0("outputs/", experiment, "/",config$models[[model_i]]$name, "/pred_plots"),
                           paste0("pred_contour_", main_feature, "_", secondary_feature))
          
        }
      }
      print(paste0("All colour feature SHAP plots for " , main_feature ,": Done"))
    }

    
    ### Non-feature colored SHAP plots
    if (create_colour_features){
      for (main_feature in feature_names) {
        for (colour_feature in colour_features) {
          shap_interaction <- create_shap_interaction_plot(
            workflow = final_workflow,
            new_data = train_data,
            feature1 = main_feature,  # Variable on X-axis
            feature2 = secondary_feature,
            color_feature = colour_feature,  # Variable on colorgradient
            poly_degree = 2
          )
          ggsave(paste0("outputs/", experiment, "/",config$models[[model_i]]$name,
                        "/shap_plots/shap_interaction_", main_feature, "_", colour_feature, ".png"),
                 shap_interaction$plot, width = 10, height = 8)
          save_plot_object(shap_interaction$plot, 
                           paste0("outputs/", experiment, "/",config$models[[model_i]]$name, "/shap_plots"),
                           paste0("shap_interaction_", main_feature, "_", secondary_feature))
        }
        print(paste0("All colour non-feature SHAP plots for " , main_feature ,": Done"))
      }
    }
    
    
    ### Representative tree  
    png(paste0("outputs/", experiment, "/", config$models[[model_i]]$name, 
        "/representative_tree.png"), width = 1600, height = 1200, res = 300)
    rep_tree <- create_representative_tree(
      final_workflow, train_data, test_data, max_depth = 5)
    dev.off()
    save_plot_object(rep_tree, 
                     paste0("outputs/", experiment, "/", config$models[[model_i]]$name),
                     "representative_tree")
    }
}


sink()
