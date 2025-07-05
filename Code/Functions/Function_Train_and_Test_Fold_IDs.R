# Function to obtain a list of indices for each training and validation fold observation to be used in XGBoost algorithm. 
# Note that the dta_w_cv_folds must contain a column called fold_id. 
# By changing the return_training_ids function from TRUE to FALSE, we can obtain either the training or validation/test folds. 
# --------------------------------------------------------------------------------------------#


print('Sourced: Train_and_Test_Fold_IDs_Function')


# --------------------------------------------------------------------------------------------#
Train_and_Test_Fold_IDs_Function <- function(dta_w_cv_folds, cv_fold_ids, return_training_ids){
  
  # If you would like to obtain the row indices for the training observations, then specify TRUE. 
  if(return_training_ids == TRUE){
    
    fold_ids <- as.integer(which(!(dta_w_cv_folds$fold_id %in% cv_fold_ids))) # Note that fold_id is a variable in the column. 
  }
  # If you would like to obtain the row indices for the validation/test observations, then specify FALSE. 
  else if (return_training_ids == FALSE){
    
    fold_ids <- as.integer(which((dta_w_cv_folds$fold_id %in% cv_fold_ids))) # If fold_id is in cv_fold_ids, then row is in validation set.
  
    }
  
  return(fold_ids)
  
}
# --------------------------------------------------------------------------------------------#

