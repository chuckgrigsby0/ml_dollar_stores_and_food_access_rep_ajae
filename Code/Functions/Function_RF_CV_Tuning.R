#Wrapper function for performing random search for xgb.cv. 
#--------------------------------------------------------------------------------------------#
#Function for classification. 
#--------------------------------------------------------------------------------------------#
# Number of low-access to access (0/1)
class_counts <- dta_untreated_wfolds %>% reframe(across(.cols = matches('^low_access'), .fns = table)) ; class_counts
# Ratio of not low-access to low-access. (0 to 1)
class_ratios <- as.numeric(class_counts[1, ])/as.numeric(class_counts[2, ]); class_ratios
class_ratios <- data.frame(dep_var = names(class_counts), ratio = class_ratios)

scale_pos_weight_data = class_ratios %>% filter(dep_var == model_dep_var) %>% pull(ratio); scale_pos_weight_data

if (model_geography == 'Rural'){ 
  scale_pos_weight_data = 1 #
} else if (model_geography == 'Urban'){ 
  scale_pos_weight_data = scale_pos_weight_data
}

print(paste('scale_pos_weight_data =', scale_pos_weight_data))
#--------------------------------------------------------------------------------------------#

#Set up early stopping during model training. 
#--------------------------------------------------------------------------------------------#
#If after 10 cross-validation iterations, the model does not improve w.r.t cv error, stop tree building/boosting. 
#--------------------------------------------------------------------------------------------#
early_stop_global = 10
#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#
#Number of trees = Number of iterations of tree building. 
#--------------------------------------------------------------------------------------------#
ntrees = 2000
#--------------------------------------------------------------------------------------------#
n_predictors <- length(colnames(xgb_train))
# Set up objective function and parameters. 
#--------------------------------------------------------------------------------------------#
objective_function <- function(eta, gamma, max_depth){ #
  
  
  
  tuning_params <- list(booster = 'gbtree', 
                        
                        tree_method = 'hist', 
                        
                        objective = 'binary:logistic', 
                        
                        eval_metric = 'error', 
                        
                        num_parallel_tree = 1, 
                        
                        eta = 1, 
                        
                        random_state = 2434, 
                        
                        max_depth = 20, 
            
                        scale_pos_weight = scale_pos_weight_data, # scale_pos_weight_data
                        
                        subsample = 0.80, # On each iteration we sample 0.80 without replacement. 
                        
                        colsample_bynode = floor(sqrt(n_predictors))/n_predictors, # Consistent with RF
                        
                        min_child_weight = 0) # binary classification error rate. 
  
  #--------------------------------------------------------------------------------------------#
  set.seed(243444)
  #--------------------------------------------------------------------------------------------#
  xgb_cv_model <- xgb.cv(data = xgb_train,
                         params = tuning_params,
                         nrounds = ntrees, # Specified above. 
                         showsd = TRUE, 
                         # early_stopping_rounds = early_stop_global, # Specified above. 
                         # metrics = 'error', 
                         maximize = FALSE, #We are not maximizing the logloss function. 
                         folds = val_folds_list, 
                         train_folds = train_folds_list, 
                         prediction = TRUE, 
                         nthread = ncores, 
                         verbose = FALSE, 
                         nrounds = 1)
  
  # The function finds the maximium so we set the Score to finding the minimum negative cv error. 
  output <- list(Score = -min(xgb_cv_model$evaluation_log$test_error_mean), 
                 nrounds_opt = xgb_cv_model$best_iteration)
  
  return(output)
  
}
#--------------------------------------------------------------------------------------------#
if (model_geography == 'Rural'){ 
  
  #--------------------------------------------------------------------------------------------#
  # Define paramater bounds for the random search optimization. 
  #--------------------------------------------------------------------------------------------#
  # param_bounds <- list(eta = c(0.025, 0.3), 
  #                     max_depth = c(10L, 25L), 
  #                    gamma = c(0.1, 0.3), 
  #                    scale_pos_weight = c(1, sqrt(scale_pos_weight_data)))
  param_bounds <- list(eta = c(0.025, 0.3), 
                       max_depth = c(12L, 25L), 
                       gamma = c(0.1, 0.3), 
                       colsample_bylevel = c(0.7, 0.9)) # 
  #--------------------------------------------------------------------------------------------#
  
} else if (model_geography == 'Urban'){ 
  
  #--------------------------------------------------------------------------------------------#
  # Define parameter bounds for the random search optimization. 
  #--------------------------------------------------------------------------------------------#
  # param_bounds <- list(eta = c(0.025, 0.3), 
  #                    max_depth = c(10L, 25L), 
  #                    gamma = c(0.1, 0.3), 
  #                    scale_pos_weight = c(scale_pos_weight_data, scale_pos_weight_data + 1))
  
  param_bounds <- list(eta = c(0.025, 0.3), 
                       max_depth = c(12L, 25L), 
                       gamma = c(0.1, 0.3), 
                       colsample_bylevel = c(0.7, 0.9) ) # 
  #--------------------------------------------------------------------------------------------#
  
}

# cl <- makeCluster(ncores)
# registerDoParallel(cl)
#clusterExport(cl, c('val_folds_list','train_folds_list', 'la_formula', 'dta_untreated_wfolds'))
#clusterEvalQ(cl, expr= {
#  library('xgboost')
#  library('dplyr')
#})
#--------------------------------------------------------------------------------------------#
# plan(multicore, workers = ncores)

xgb_cv <- bayesOpt(FUN = objective_function, 
                   bounds = param_bounds, 
                   initPoints = length(param_bounds) + 1, 
                   iters.n = 10, # 10 
                   iters.k = 1, # 1
                   parallel = FALSE)

#--------------------------------------------------------------------------------------------#
# stopCluster(cl)
# registerDoSEQ()
