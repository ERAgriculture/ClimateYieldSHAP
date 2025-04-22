require("tidyverse")
require("corrplot")
require("car")
require("ggcorrplot")
require("lsr")

cramerV <- function(x, y) {
  confusion_matrix <- table(x, y)
  chi_squared <- chisq.test(confusion_matrix, correct=FALSE)
  n <- sum(confusion_matrix)
  min_dim <- min(nrow(confusion_matrix) - 1, ncol(confusion_matrix) - 1)
  
  V <- sqrt(chi_squared$statistic / (n * min_dim))
  return(V)
}


correlation_analysis <- function(data, target_var = NULL, cor_threshold = 0.7) {
  
  # Separate numeric and categorical variables
  numeric_vars <- data %>% select(where(is.numeric)) %>% names()
  categorical_vars <- data %>% select(where(~!is.numeric(.))) %>% names()
  
  if(length(numeric_vars) > 1) {
    # 1. Check for aliased variables
    aliased_vars <- findLinearCombos(data[numeric_vars])
    if(!is.null(aliased_vars$remove)) {
      cat("\nAliased variables detected. Removing:\n")
      to_remove_vars <- numeric_vars[aliased_vars$remove]
      print(numeric_vars[aliased_vars$remove])
      data <- data %>% select(-all_of(to_remove_vars))
      numeric_vars <- data %>% select(where(is.numeric)) %>% names()
    } else {
      cat("\nNo aliased variables detected.\n")
    }
    
    # numeric_vars <- data %>% select(where(is.numeric)) %>% names()
    # Remove constant columns
    numeric_data <- data[numeric_vars]
    non_constant_vars <- numeric_data %>% 
      summarise(across(everything(), ~var(., na.rm = TRUE) != 0)) %>%
      select(where(isTRUE)) %>%
      names()
    
    if(length(non_constant_vars) != length(numeric_vars)) {
      constant_vars <- setdiff(numeric_vars, non_constant_vars)
      cat("\nConstant variables detected and removed:\n")
      print(constant_vars)
      numeric_vars <- non_constant_vars
    }
    
    if(length(numeric_vars) > 1) {
      # 2. Correlation plot
      cor_matrix <- cor(data[numeric_vars], use = "pairwise.complete.obs")
      
      print("Correlation Plot:")
      png(paste0("corrplot_", ncol(data),"vars.png"), width = 800, height = 600)
      corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
               tl.col = "black", tl.srt = 45, tl.cex = 0.6)
      dev.off()
      
      # 3. VIF calculation
      if(!is.null(target_var) && target_var %in% names(data)) {
        formula <- as.formula(paste(target_var, "~", paste(numeric_vars[numeric_vars != target_var], collapse = " + ")))
        tryCatch({
          vif_results <- vif(lm(formula, data = data))
          cat("\nVariance Inflation Factors:\n")
          print(vif_results)
        }, error = function(e) {
          cat("\nError in VIF calculation. This might be due to perfect multicollinearity or other issues in the data.\n")
          print(e)
        })
      } else {
        cat("\nTarget variable not specified or not found. Skipping VIF calculation.\n")
      }
      
      # 4. Highlight high correlations
      high_cor <- which(abs(cor_matrix) > cor_threshold & abs(cor_matrix) < 1, arr.ind = TRUE)
      if(nrow(high_cor) > 0) {
        cat("\nHighly correlated variable pairs (|r| >", cor_threshold, "):\n")
        for(i in 1:nrow(high_cor)) {
          if(high_cor[i, 1] < high_cor[i, 2]) {
            cat(numeric_vars[high_cor[i, 1]], "and", numeric_vars[high_cor[i, 2]], 
                ": r =", round(cor_matrix[high_cor[i, 1], high_cor[i, 2]], 2), "\n")
          }
        }
      } else {
        cat("\nNo variable pairs with correlation above", cor_threshold, "detected.\n")
      }
    } else {
      cat("\nNot enough non-constant numeric variables for correlation analysis.\n")
    }
  } else {
    cat("\nNot enough numeric variables for correlation analysis.\n")
  }
  
  # 5. Cramer's V for categorical variables
  if(length(categorical_vars) > 1) {
    # Remove categorical variables with only one unique value
    single_value_vars <- sapply(data[categorical_vars], function(x) length(unique(x)) == 1)
    if(any(single_value_vars)) {
      cat("\nRemoving categorical variables with only one unique value:\n")
      print(names(single_value_vars)[single_value_vars])
      categorical_vars <- categorical_vars[!single_value_vars]
    }
    
    if(length(categorical_vars) > 1) {
      cat("\nCramer's V for categorical variables:\n")
      cramer_v_matrix <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars))
      rownames(cramer_v_matrix) <- colnames(cramer_v_matrix) <- categorical_vars
      for(i in 1:(length(categorical_vars)-1)) {
        for(j in (i+1):length(categorical_vars)) {
          cramer_v_matrix[i,j] <- cramer_v_matrix[j,i] <- 
            cramerV(data[[categorical_vars[i]]], data[[categorical_vars[j]]])
        }
      }
      print(round(cramer_v_matrix, 2))
      
      # Highlight high Cramer's V values
      high_cramer <- which(cramer_v_matrix > cor_threshold & !is.na(cramer_v_matrix), arr.ind = TRUE)
      if(nrow(high_cramer) > 0) {
        cat("\nHighly associated categorical variable pairs (Cramer's V >", cor_threshold, "):\n")
        for(i in 1:nrow(high_cramer)) {
          if(high_cramer[i, 1] < high_cramer[i, 2]) {
            cat(categorical_vars[high_cramer[i, 1]], "and", categorical_vars[high_cramer[i, 2]], 
                ": Cramer's V =", round(cramer_v_matrix[high_cramer[i, 1], high_cramer[i, 2]], 2), "\n")
          }
        }
      } else {
        cat("\nNo categorical variable pairs with Cramer's V above", cor_threshold, "detected.\n")
      }
    } else {
      cat("\nNot enough categorical variables with multiple values for Cramer's V calculation.\n")
    }
  } else {
    cat("\nNot enough categorical variables for Cramer's V calculation.\n")
  }
}