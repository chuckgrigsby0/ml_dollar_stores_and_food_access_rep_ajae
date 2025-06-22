print('Sourced: Lasso_FE_Estimates_Function')
#--------------------------------------------------------------------------------------------#
Lasso_FE_Estimates_Function <- function(model_data, la_vars_exclude1, la_vars_exclude2, post_selection_output){
  
  # -------------------------------------------------------------------------------------------- #
  gc()
  # -------------------------------------------------------------------------------------------- #
  # Note that because we will estimate the model without an intercept,
  # we can include the year_2005 dummy variable. 
  model_data <- model_data %>% select(-all_of(c(la_vars_exclude1, la_vars_exclude2)))
  
  vars = names(model_data)[!grepl('low_access', names(model_data))]; vars
  # The inclusion of -1 is because glmnet estimates the model with an intercept automatically. 
  la_formula <- xpd(regex('low_access') ~ ..ctrl - 1, ..ctrl = vars, data = model_data) 
  #print(la_formula)
  
  # Create model matrices
  x_train=model.matrix(la_formula, data=model_data)
  # y_train needs to be a numeric vector and pull() can be used to convert it most generally.  
  y_train= model_data %>% select(starts_with('low_access')) %>% pull()
  
  #--------------------------------------------------------------------------------------------#
  #Include a specific variable on every model run (i.e., apply no penalization.)
  penalty_fac = as.numeric(!grepl('year_|STATE_[A-Z]', vars))
  length(penalty_fac)==length(colnames(x_train))
  # Verify that the no-penalization is applied to only the year and state dummies. 
  # print( vars[which(as.numeric(!grepl('year_|STATE_[A-Z]', vars))==0)] )
  
  # Set up parallel processing and set seed. 
  
  
  set.seed(123)
  registerDoMC(ncores)
  #Run 5 fold CV using the training data, using the unique values of alpha. 
  output <- cv.glmnet(x_train, y_train, 
                      intercept = FALSE, 
                      family='gaussian', 
                      type.measure = 'mse', 
                      alpha=1, 
                      penalty.factor = penalty_fac,
                      nfolds = n_folds, # If foldid is supplied, nfolds can be missing. 
                      foldid=fold_id, 
                      trace.it=TRUE, 
                      parallel = TRUE)
  
  output$cvm[which(output$lambda==output$lambda.min)]
  output$cvm[which(output$lambda==output$lambda.1se)]
  
  # Using a custom to cleanly extract the non-zero coefficients. 
  coef_sel <- tidy_glmnet(fitted_model = output, lambda_val = 'lambda.1se')
  
  # Save the optimal lambda value based on the 5-fold CV. 
  lambda_min <- output$lambda.1se
  lambda_min_index <- output$index[2]
  cv_error <- output$cvm[lambda_min_index]
  cv_sd <- output$cvsd[lambda_min_index]
  
  if (isTRUE(post_selection_output)){
  
  coef_sel_str <- coef_sel$row[!grepl('Intercept', coef_sel$row)]
  # Create a new formula/function based on the selected variables. 
  la_formula2 <- xpd(regex('low_access') ~ ..ctrl - 1, ..ctrl = coef_sel_str, data = model_data)
  # Obtain a new model matrix based on the selected variables. 
  x_train2 = model.matrix(la_formula2, data=model_data)
  y_train2 = model_data %>% select(starts_with('low_access')) %>% pull()
  # y_train2 needs to be a numeric vector and pull() can be used to convert it most generally.  
  
  # Create new penalty vector based on selected variables. 
  penalty_fac2 = as.numeric(!grepl('year_|STATE_', coef_sel_str))
  # Refit the model to the full data based on updated parameters. 
  lasso_model <- glmnet(x = x_train2, y = y_train2, 
                        intercept = FALSE, 
                        family = 'gaussian', 
                        type.measure = 'mse', 
                        alpha=1, 
                        penalty.factor = penalty_fac2,
                        lambda = lambda_min,
                        trace.it=TRUE, 
                        parallel = TRUE)
  
  coef_double_sel <- tidy_glmnet(fitted_model = lasso_model, lambda_val = NULL)
  # Fit an unpenalized regression model to the selected varibles. 
  reg_model <- glm(la_formula2, family = 'gaussian', data = model_data)
  coef_post_sel <- broom::tidy(reg_model)
  
  model_list <- list('lambda_min' = lambda_min, 
                     'cv_error' = cv_error, 
                     'cv_sd' = cv_sd,
                     'coef_sel' = coef_sel, 
                     'coef_double_sel' = coef_double_sel, 
                     'coef_post_sel' = coef_post_sel)
  
  } else { 
    
    model_list <- list('lambda_min' = lambda_min, 
                       'cv_error' = cv_error, 
                       'cv_sd' = cv_sd,
                       'coef_sel' = coef_sel)
    }
  
  return(model_list)
  
}
