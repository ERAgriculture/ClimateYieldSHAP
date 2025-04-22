create_performance_table <- function(aez_list, N_obs_df, experiment_t, base_dir = "model_output") {
  # Initialize results dataframe
  results <- data.frame(
    AEZ = aez_list,
    N_obs = N_obs_df$N_obs,
    Boot_Train_R2 = NA, Boot_Train_RMSE = NA, Boot_Train_MAE = NA,
    Boot_Test_R2 = NA, Boot_Test_RMSE = NA, Boot_Test_MAE = NA,
    Full_Train_R2 = NA, Full_Train_RMSE = NA, Full_Train_MAE = NA,
    Full_Test_R2 = NA, Full_Test_RMSE = NA, Full_Test_MAE = NA
  )
  
  for (i in seq_along(aez_list)) {
    aez <- aez_list[i]
    experiment <- paste0(experiment_t, "_", gsub(" ", ".", aez))
    
    # Read bootstrap results
    train_boot_path <- file.path(base_dir, experiment, "train_bootstrap_metrics.csv")
    test_boot_path <- file.path(base_dir, experiment, "test_bootstrap_metrics.csv")
    
    if (file.exists(train_boot_path) && file.exists(test_boot_path)) {
      train_boot <- read.csv(train_boot_path)
      test_boot <- read.csv(test_boot_path)
      
      results$Boot_Train_R2[i] <- train_boot$mean[train_boot$.metric == "boots_rsq"]
      results$Boot_Train_RMSE[i] <- train_boot$mean[train_boot$.metric == "boots_rmse"]
      results$Boot_Train_MAE[i] <- train_boot$mean[train_boot$.metric == "boots_mae"]
      
      results$Boot_Test_R2[i] <- test_boot$mean[test_boot$.metric == "boots_rsq"]
      results$Boot_Test_RMSE[i] <- test_boot$mean[test_boot$.metric == "boots_rmse"]
      results$Boot_Test_MAE[i] <- test_boot$mean[test_boot$.metric == "boots_mae"]
    }
    
    # Read full model results
    full_results_path <- file.path("outputs", experiment, "full_results", "full_results.rds")
    if (file.exists(full_results_path)) {
      full_results <- readRDS(full_results_path)
      
      # Extract train metrics
      train_metrics <- full_results$train_metrics
      results$Full_Train_R2[i] <- train_metrics$.estimate[train_metrics$.metric == "rsq"]
      results$Full_Train_RMSE[i] <- train_metrics$.estimate[train_metrics$.metric == "rmse"]
      results$Full_Train_MAE[i] <- train_metrics$.estimate[train_metrics$.metric == "mae"]
      
      # Extract test metrics
      test_metrics <- full_results$test_metrics
      results$Full_Test_R2[i] <- test_metrics$.estimate[test_metrics$.metric == "rsq"]
      results$Full_Test_RMSE[i] <- test_metrics$.estimate[test_metrics$.metric == "rmse"]
      results$Full_Test_MAE[i] <- test_metrics$.estimate[test_metrics$.metric == "mae"]
    }
  }
  
  # Round numeric columns
  numeric_cols <- names(results)[!names(results) %in% c("AEZ")]
  for (col in numeric_cols) {
    if (grepl("R2", col)) {
      results[[col]] <- round(results[[col]], 2)
    } else {
      results[[col]] <- round(results[[col]], 0)
    }
  }

  write.csv(results, file = paste0("final_results/", experiment_t, "_performance_metrics.csv"))
  return(results)
}


# Example usage:
# aez_list <- c("Warm Semiarid", "Warm Subhumid", "Warm Humid", 
#               "Cool Semiarid", "Cool Subhumid", "Cool Humid")
# N_obs_df <- data.frame(AEZ = aez_list, N_obs_df = c(2218, 6105, 615, 
#                                                     3564, 1615, 698))
# 
# performance_table <- create_performance_table(aez_list = aez_list, 
#                                               experiment_t = "eco_isqrt_nosoil",
#                                               base_dir = "model_output", 
#                                               N_obs_df = N_obs_df)
