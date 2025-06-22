pacman::p_load('mlr3verse')
# library(mlr3)
# library(mlr3learners)
# library(mlr3tuning)
# library()
# library(mlr3tuningspaces)

convert_dep_var_to_factor <- function(dta){
  
  dta <- dta %>% 
    mutate(across(.cols = matches('^low_access'), 
                  .fns = \(x) factor(x, levels = c(0, 1) ) ) )
  
  return(dta)
}

dta_untreated_wfolds <- convert_dep_var_to_factor(dta = dta_untreated_wfolds)
dta_treated <- convert_dep_var_to_factor(dta = dta_treated)

#--------------------------------------------------------------------------------------------#
# Number of low-access to access (0/1)
#--------------------------------------------------------------------------------------------#
class_counts <- dta_untreated_wfolds %>% reframe(across(.cols = matches('^low_access'), .fns = table)) ; class_counts
#--------------------------------------------------------------------------------------------#
# Ratio of not low-access to low-access. (0 to 1)
#--------------------------------------------------------------------------------------------#
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
# Create the task (no need for xgb.DMatrix)
task <- as_task_classif(x = dta_untreated_wfolds, 
                        target = model_dep_var,
                        positive = '1',
                        id = 'pretreatment', 
                        label = paste0('Dep Var: ', model_dep_var, 'Geography: ', model_geography))


task$select(covars_by_model[[1]])

# learner = lrn('classif.xgboost')
# as.data.table(learner$param_set) %>% View()

# Define the parameter space matching your bounds
search_space <- ps(
  eta = p_dbl(lower = 0.025, upper = 0.3),
  max_depth = p_int(lower = 12, upper = 25),
  gamma = p_dbl(lower = 0.1, upper = 0.3),
  colsample_bylevel = p_dbl(lower = 0.7, upper = 0.9) 
)

internal_search_space = ps(
  nrounds = p_dbl(upper = ntrees, tags = "internal_tuning",
                  aggr = function(x) as.integer(mean(unlist(x))))
)

# Create the learner with fixed parameters
learner <- lrn("classif.xgboost",
               booster = "gbtree",
               tree_method = "hist",
               objective = "binary:logistic",
               eval_metric = "error",
               early_stopping_rounds = early_stop_global,
               # nrounds = ntrees,
               validate = 'test', 
               subsample = 0.80,
               colsample_bytree = 0.75,
               scale_pos_weight = scale_pos_weight_data
)

# Define resampling strategy (matching your custom folds)
# If you have predefined folds:
custom_cv <- rsmp("custom")
custom_cv$instantiate(task, 
                      train_sets = train_folds_list, 
                      test_sets = val_folds_list)

# Define LHS tuner
design = generate_design_lhs(param_set = search_space, n = 2)

tuner <- tnr("design_points", design = design$data, batch_size = 10)  # matches your initPoints

set.seed(7832)
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

# Set up the tuning instance
cv_models <- tune(
  tuner = tuner, 
  task = task,
  learner = learner,
  resampling = custom_cv,
  measure = msr("classif.ce"),  # classification error
  search_space = search_space,
  internal_search_space = internal_search_space, 
  terminator = trm("evals", n_evals = 1) # matches your iters.n
)

as.data.table(cv_models$result_learner_param_vals)

cv_models$archive$predictions(i = 1)
# Run the tuning
tuner$optimize(instance)

# Get best parameters
best_params <- instance$result_x_domain
print(best_params)

# Train final model with best parameters
learner$param_set$values <- best_params
final_model <- learner$train(task)

# For predictions on new data
task_predict <- TaskClassif$new(
  id = "predict",
  backend = dta_treated,  # Your test data
  target = model_dep_var
)
predictions <- final_model$predict(task_predict)