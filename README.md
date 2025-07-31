# The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis

This repository contains the scripts used for statistical analyses in "The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis"

# Code/Analysis

This directory and sub-directories contain scripts to produce results in main analyses and parts of supplementary findings. The following documents the most relevant
scripts for re-producing main results. 

### Scripts called to prepare and load data. 

The following helper scripts are called in other scripts to cleanly prepare and load data and do not need to be run separately. 

 [1] `data_preparation_bootstrap_estimation_tracts_w_time_trends.R`            
 [2] `data_preparation_bootstrap_estimation_tracts.R`                          
 [3] `data_preparation_did_regressions.R`                                      
 [4] `data_preparation_feat_eng_time_by_state_fes_create_data.R`               
 [5] `data_preparation_feat_eng_time_state_and_linear_trends_fes_create_data.R`
 [6] `data_preparation_get_hyperparameters.R`                                  
 [7] `data_preparation_imputation_estimation_national.R`                       
 [8] `data_preparation_imputation_estimation_w_time_trends.R`                  
 [9] `data_preparation_imputation_estimation.R`                                
[10] `data_preparation_model_covars_lists.R`                                   
[11] `data_preparation_prepare_binned_and_factor_covars.R`                     
[12] `data_preparation_prepare_binned_ds_policy_vars.R`                        
[13] `data_preparation_prepare_pretreatment_binned_grocery_and_ds_policy.R`    
[14] `load_data_for_imputation_estimation_w_time_trends.R`                     
[15] `load_data_for_imputation_estimation.R`


## Code/Analysis/Imputation_Xgboost

The `Main/Training` subdirectory contains a bash script and an accompanying Rscript to tune and train Xgboost models using the full sample, while
`Main/Bootstrap` contains a bash and Rscript that implements the bootstrap procedure using the optimal hyperparameters found in model training.  

Scripts are run using the `sbatch_*` scripts in the respective directories. 

Model training required approximately 30-100 GB RAM, varying by urban and rural models and tuning approach, while we set the memory to 21 and 80 GB for bootstrap models. 

The SBATCH arguments `mem` and `-export=model_geography` should be adjusted according to urban/rural models. 

We set `-mail-user=useremail` and `qos=accountname-b` as placeholders for the user's actual email and account username. In our implementation we use our email and usernames. 

## Code/Analsys/sbatch_bootstrap_all_output.sh

This helper script calls the Rscripts located in sub-directories that produce bootstrap estimates for all analyses of cross-validation errors and treatment-effect heterogeneity. 

One can alternatively run individual bash scripts from the sub-directories. However, `sbatch_bootstrap_all_output.sh` produces all bootstrap estimates in a single script. 

## Code/Analsys/sbatch_figures.sh

This helper script calls the Rscripts located in sub-directories that produce all figures in main and supplementary text, with the exception of results that include
superettes in the low-access outcome variable, as described below. 


## Code/Analysis_Supplementary_w_Superettes

The `Code/Analysis_Supplementary_w_Superettes` directory and sub-directories follow the same organizational structure as `Code/Analysis`, but results are based on the inclusion of superettes in the low-access outcome. 
