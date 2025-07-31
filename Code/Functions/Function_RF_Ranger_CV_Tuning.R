print('Sourced: rf_tuning: Use to tune hyperparameters, excluding ntrees.')
rf_tuning <- function(param_idx){ 
  
  # Create vector of case.weights. 
  model_weights =  ifelse(dta_untreated_wfolds[model_dep_var] == 0, 1, design$case.weights[param_idx])
  
  design <- design %>% select(-c(case.weights))
  
  nfolds <- seq_along(train_folds_list)
  
  # Use function below for converting factor to numeric. 
  as.numeric_from_factor <- function(x) as.numeric(as.character(x) )
  
  plan(multicore, workers = ncores)
  
  # Cross-Validation Modeling - Pre-treatment data. 
  tic()
  cv_results <- nfolds %>%
    
    future_map(function(.x){
      
      train_idx <- train_folds_list[[.x]]
      
      set.seed(243444)
      rf_model <- ranger(x = x_train[train_idx, ], 
                         y = y_train[train_idx], 
                         mtry = design$mtry[param_idx],
                         sample.fraction = design$sample.fraction[param_idx], 
                         replace = design$replace[param_idx], 
                         num.trees = ntrees, 
                         min.node.size = 1, 
                         min.bucket = 1, 
                         max.depth = 0, 
                         splitrule = 'gini', 
                         case.weights = model_weights[train_idx], 
                         classification = TRUE, 
                         oob.error = FALSE, 
                         num.threads = 0)
      
      val_idx <- val_folds_list[[.x]]
      
      rename_lookup <- c('actual' = model_dep_var)
      
      cv_preds_rf <- predict(rf_model, data = x_train[val_idx, ])
      
      cv_preds <- dta_untreated_wfolds %>% 
        
        slice(val_idx) %>% 
        
        select(fold_id, GEOID, year, all_of(model_dep_var) ) %>%
        
        rename(all_of(rename_lookup)) %>%
        
        mutate(cv_preds = as.numeric_from_factor(cv_preds_rf$predictions), 
               cv_error = actual - cv_preds)
      
      return(cv_preds)
      
    })
  
  toc()
  
  cv_results <- set_names(cv_results, nm = nfolds)
  
  cv_results <- bind_rows(cv_results, .id = 'fold_id')
  
  return(cv_results)
  
}

print('Sourced: rf_tuning_trees: Use to tune number of trees.')

get_cpu_info <- function() {
  pid <- Sys.getpid()
  cpu_affinity <- system(sprintf("taskset -cp %d", pid), intern = TRUE)
  return(list(
    pid = pid,
    cpu_affinity = cpu_affinity,
    num_threads = system(sprintf("ps -p %d -o nlwp=", pid), intern = TRUE)
  ))
}

nfolds <- seq_along(train_folds_list)
ncores_inner <- floor(ncores/length(nfolds))

plan(multicore, workers = length(nfolds))

rf_tuning_trees <- function(param_idx){ 
  
  # Create vector of case.weights. 
  model_weights =  ifelse(dta_untreated_wfolds[model_dep_var] == 0, 1, design$case.weights)
  
  design <- design %>% select(-c(case.weights))
  
  
  # Use function below for converting factor to numeric. 
  as.numeric_from_factor <- function(x) as.numeric(as.character(x) )
  
  # Cross-Validation Modeling - Pre-treatment data. 
  tic()
  cv_results <- nfolds %>%
    
    future_map(function(.x){
      
      train_idx <- train_folds_list[[.x]]
      print(paste0('Fold ID: ', .x) )
      cpu_info <- get_cpu_info()
      cat(sprintf("Process ID: %d\n", cpu_info$pid))
      cat(sprintf("CPU Affinity: %s\n", cpu_info$cpu_affinity))
      cat(sprintf("Number of threads: %s\n", cpu_info$num_threads))
      
      set.seed(243444)
      rf_model <- ranger(x = x_train[train_idx, ], 
                         y = y_train[train_idx], 
                         mtry = design$mtry,
                         sample.fraction = design$sample.fraction, 
                         replace = design$replace, 
                         num.trees = ntrees[param_idx], 
                         min.node.size = 1, 
                         min.bucket = 1, 
                         max.depth = 0, 
                         splitrule = 'gini', 
                         case.weights = model_weights[train_idx], 
                         classification = TRUE, 
                         oob.error = FALSE, 
                         num.threads = 0, 
                         verbose = FALSE)
      
      # Get updated thread info
      new_cpu_info <- get_cpu_info()
      cat(sprintf("Final thread count: %s\n", new_cpu_info$num_threads))
      
      val_idx <- val_folds_list[[.x]]
      
      rename_lookup <- c('actual' = model_dep_var)
      
      cv_preds_rf <- predict(rf_model, data = x_train[val_idx, ])
      
      cv_preds <- dta_untreated_wfolds %>% 
        
        slice(val_idx) %>% 
        
        select(fold_id, GEOID, year, all_of(model_dep_var) ) %>%
        
        rename(all_of(rename_lookup)) %>%
        
        mutate(cv_preds = as.numeric_from_factor(cv_preds_rf$predictions), 
               cv_error = actual - cv_preds)
      
      return(cv_preds)
      
    })
  
  toc()
  
  cv_results <- set_names(cv_results, nm = nfolds)
  
  cv_results <- bind_rows(cv_results, .id = 'fold_id')
  
  return(cv_results)
  
}

