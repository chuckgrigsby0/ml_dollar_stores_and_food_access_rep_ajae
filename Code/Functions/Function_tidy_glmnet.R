#--------------------------------------------------------------------------------------------#
# Given the issues with using the broom tidy() function, 
# I have created a custom function that allows us to obtain the 
# coefficients from the glmnet models (glmnet or cv.glmnet functions)
# One must supply the fitted model name and the lambda value. If 
# the model is estimated using glmnet, thus, no CV, assign s = NULL. 
#--------------------------------------------------------------------------------------------#
tidy_glmnet <- function(fitted_model, lambda_val){
  
  coef_table <- data.frame(row = rownames(coef(fitted_model, s = lambda_val)), 
                           value = matrix(coef(fitted_model, s = lambda_val)))
  
  coef_table <- coef_table %>% filter(value != 0)
  
  return(coef_table)
  
}
#--------------------------------------------------------------------------------------------#
print('Sourced: tidy_glmnet')