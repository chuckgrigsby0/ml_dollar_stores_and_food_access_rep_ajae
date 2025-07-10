#--------------------------------------------------------------------------------------------#
# Helper script and function for classification using BO tuning/training approach. 
#--------------------------------------------------------------------------------------------#
# Number of low-access to access (0/1)
class_counts <- dta_untreated_wfolds %>% reframe(across(.cols = matches('^low_access'), .fns = table))

# Ratio of not low-access to low-access. (0 to 1)
class_ratios <- as.numeric(class_counts[1, ])/as.numeric(class_counts[2, ])
class_ratios <- data.frame(dep_var = names(class_counts), ratio = class_ratios)

scale_pos_weight_data = class_ratios %>% 
  filter(dep_var == model_dep_var) %>% 
  pull(ratio)

if (model_geography == 'Rural'){ 
  scale_pos_weight_data = 1
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
#Number of trees = Number of iterations of tree building. 
#--------------------------------------------------------------------------------------------#
ntrees = 2000
#--------------------------------------------------------------------------------------------#

# Set up objective function and parameters. 
#--------------------------------------------------------------------------------------------#
objective_function <- function(eta, gamma, max_depth, colsample_bylevel){ #
  
   
  
tuning_params <- list(eta = eta, # Controls the learning rate. If you increase eta, then you need to increase ntrees.
                             
                      gamma = gamma, # Controls regularization. 
                                      # Increasing gamma reduces the complexity of the trees
                             
                      max_depth = max_depth, # Controls the depth of each tree built for each iteration. 
                                             # Increasing makes trees more complex. 6 is the default 
                      
                      scale_pos_weight = scale_pos_weight_data, # scale_pos_weight_data. Used to handle imbalanced outcomes. 
                             
                      subsample = 0.80, # On each iteration we sample 0.80 without replacement. 
                             
                      colsample_bylevel = colsample_bylevel, # Randomly samples a subset of columns at each level of the tree.  
                             
                      colsample_bytree = 0.75, 
                      
                      booster = 'gbtree', 
                      
                      tree_method = 'hist', 
                      
                      objective = 'binary:logistic', 
                      
                      eval_metric = 'error') # binary classification error rate. 
                      
#--------------------------------------------------------------------------------------------#
set.seed(243444)
#--------------------------------------------------------------------------------------------#
  xgb_cv_model <- xgb.cv(data = xgb_train,
                         params = tuning_params,
                         nrounds = ntrees, # Specified above. 
                         showsd = TRUE, 
                         early_stopping_rounds = early_stop_global, # Specified above. 
                         maximize = FALSE, 
                         folds = val_folds_list, 
                         train_folds = train_folds_list, 
                         prediction = TRUE, 
                         nthread = ncores, 
                         verbose = FALSE)
  
  # BO maximizes the objective, so we use negative error to minimize the actual error rate. 
  output <- list(Score = -min(xgb_cv_model$evaluation_log$test_error_mean), 
                 nrounds_opt = xgb_cv_model$best_iteration)
  
  return(output)
  
}

# Specify search space for tuning hyperparams. 
param_bounds <- list(eta = c(0.025, 0.3), 
                     max_depth = c(12L, 25L), 
                     gamma = c(0.1, 0.3), 
                     colsample_bylevel = c(0.7, 0.9) )
#--------------------------------------------------------------------------------------------#

xgb_cv <- bayesOpt(FUN = objective_function, 
                   bounds = param_bounds, 
                   initPoints = length(param_bounds) + 1, 
                   iters.n = 10, 
                   iters.k = 1, 
                   parallel = FALSE)

#--------------------------------------------------------------------------------------------#
