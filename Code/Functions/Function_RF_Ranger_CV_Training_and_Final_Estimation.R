print('Sourced: rf_training_and_imputation: Use to train tuned models using bootstrapped samples and original sample.')

# ------------------------------- #
source(here::here('Code', 'Functions', 'Function_check_cpu_config.R'))
# ------------------------------- #
nfolds <- seq_along(train_folds_list)
ncores_inner <- floor(ncores/length(nfolds))

# Set the memory limit; 3 GB
options(future.globals.maxSize = 3000 * 1024^2)


plan(multicore, workers = length(nfolds))

rf_training_and_imputation <- function(model_type){ 
  
  # Create vector of case.weights. 
  # Uses same value for each iteration. 
  model_weights =  ifelse(dta_untreated_wfolds[model_dep_var] == 0, 1, tuning_params$case.weights)
  
  tuning_params <- tuning_params %>% select(-c(case.weights))
  
  # Use function below for converting factor to numeric. 
  as.numeric_from_factor <- function(x) as.numeric(as.character(x) )
  
  # Cross-Validation Modeling - Pre-treatment data. 
  tic()
  cv_results <- nfolds %>%
    
    future_map(function(.x){
      
      print(paste0('Fold ID: ', .x) )
      train_idx <- train_folds_list[[.x]]
      # --------------------------------------------- #
      # Processing information 
      # --------------------------------------------- #
      cpu_info <- get_cpu_info()
      cat(sprintf("Process ID: %d\n", cpu_info$pid))
      cat(sprintf("CPU Affinity: %s\n", cpu_info$cpu_affinity))
      cat(sprintf("Number of Process Threads: %s\n", cpu_info$process_threads))
      cat(sprintf("Number of Available Cores: %s\n", cpu_info$available_cores))
      # --------------------------------------------- #
      set.seed(243444)
      rf_model <- ranger(x = x_train[train_idx, ], 
                         y = y_train[train_idx], 
                         mtry = tuning_params$mtry,
                         sample.fraction = tuning_params$sample.fraction, 
                         replace = tuning_params$replace, 
                         num.trees = ntrees$num.trees, 
                         min.node.size = 1, 
                         min.bucket = 1, 
                         max.depth = 0, 
                         splitrule = 'gini', 
                         case.weights = model_weights[train_idx], 
                         classification = TRUE, 
                         oob.error = FALSE, 
                         num.threads = 0, 
                         verbose = FALSE)
      # --------------------------------------------- #
      # Get updated processing information. 
      # --------------------------------------------- #
      new_cpu_info <- get_cpu_info()
      cat(sprintf("Final Number of Process Threads: %s\n", new_cpu_info$process_threads))
      cat(sprintf("Final Number of Available Cores: %s\n", new_cpu_info$available_cores))
      
      val_idx <- val_folds_list[[.x]]
      
      rename_lookup <- c('actual' = model_dep_var)
      
      cv_preds_rf <- predict(rf_model, data = x_train[val_idx, ])
      
      cv_preds <- dta_untreated_wfolds %>% 
        
        dplyr::slice(val_idx) %>% 
        
        select(fold_id, GEOID, year, all_of(model_dep_var) ) %>%
        
        rename(all_of(rename_lookup)) %>%
        
        mutate(cv_preds = as.numeric_from_factor(cv_preds_rf$predictions), 
               cv_error = actual - cv_preds)
      
      return(cv_preds)
      
    })
  
  toc()
  
  cv_results <- set_names(cv_results, nm = nfolds)
  
  cv_results <- bind_rows(cv_results, .id = 'fold_id')
  
  # --------------------------------------------- #
  # Compute classification error. 
  # --------------------------------------------- #
  min_cv_mse <- cv_results %>% 
    group_by(fold_id) %>%
    summarise(ce = sum(actual != cv_preds)/n() ) %>%
    ungroup() %>%
    summarise(ce = mean(ce))
  
  # --------------------------------------------- #
  # Estimate a final pre-treatment model
  # --------------------------------------------- #
  
  plan(multicore, workers = ncores)
  # --------------------------------------------- #
  cpu_info <- get_cpu_info()
  cat('*****Final Pre-Treatment Model*****\n')
  cat(sprintf("Process ID: %d\n", cpu_info$pid))
  cat(sprintf("CPU Affinity: %s\n", cpu_info$cpu_affinity))
  cat(sprintf("Number of Process Threads: %s\n", cpu_info$process_threads))
  cat(sprintf("Number of Available Cores: %s\n", cpu_info$available_cores))
  # --------------------------------------------- #
  set.seed(2434)
  rf_model_final_fit <- ranger(x = x_train, 
                               y = y_train, 
                               mtry = tuning_params$mtry,
                               sample.fraction = tuning_params$sample.fraction, 
                               replace = tuning_params$replace, 
                               num.trees = ntrees$num.trees, 
                               min.node.size = 1, 
                               min.bucket = 1, 
                               max.depth = 0, 
                               splitrule = 'gini', 
                               case.weights = model_weights, 
                               classification = TRUE, 
                               oob.error = FALSE, 
                               num.threads = 0, 
                               verbose = FALSE)
  
  # --------------------------------------------- #
  # Get updated processing information. 
  # --------------------------------------------- #
  new_cpu_info <- get_cpu_info()
  cat('*****Post-Estimation CPU Summary*****\n')
  cat(sprintf("Final Number of Process Threads: %s\n", new_cpu_info$process_threads))
  cat(sprintf("Final Number of Available Cores: %s\n", new_cpu_info$available_cores))
  
  rename_lookup <- c('actual' = model_dep_var)
  
  cf_preds_rf <- predict(rf_model_final_fit, data = x_treat)
  
  cf_preds <- dta_treated %>% 
    
    select(GEOID, year, all_of(model_dep_var) ) %>%
    
    rename(all_of(rename_lookup)) %>%
    
    mutate(cf_preds = as.numeric_from_factor(cf_preds_rf$predictions), 
           tau = actual - cf_preds)
  
  if (model_type == 'full_sample'){
    model_results <- list(min_cv_mse = min_cv_mse, #The min_cv_mse is not the MSE, but is actually the mininum misclassification rate.  
                          cv_errors_opt = cv_results, # The combined cross-validated errors from each cross-validation fit. 
                          data_cf_preds = cf_preds, # Estimated counterfactuals.
                          rf_model_fit = rf_model_final_fit) # Save final fitted model.
  } else if (model_type == 'bootstrap'){ 
    model_results <- list(min_cv_mse = min_cv_mse, #The min_cv_mse is not the MSE, but is actually the mininum misclassification rate.  
                          cv_errors_opt = cv_results, # The combined cross-validated errors from each cross-validation fit. 
                          data_cf_preds = cf_preds) # Estimated counterfactuals.
    
    }
   
  
  
  return(model_results)
  
}


