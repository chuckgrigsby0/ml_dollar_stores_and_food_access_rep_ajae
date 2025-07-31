# The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis

This repository contains the scripts for replicating all analyses in the main text and supplementary materials. We used a combination of R and bash shell scripts, as 
we have access to cluster computing facilities through the University of Florida. Estimating models typically requires approximately 20-100 GB RAM, varying by urban and rural models and tuning approach.


## Scripts called to load data. 

1. `data_preparation_imputation_estimation.R` - Loads and prepares data for model estimation. 
  - Helper program called in above script `load_data_for_imputation_estimation.R`.
  - Helper function called in above script - `'Function_Combine_Treated_or_Untreated_Data.R'`. 
2. `data_preparation_imputation_estimation_tracts.R` - Loads and prepares data for bootstrap model estimation. 
3. `Function_Compile_Market_Time_FE.R` - Called in `data_preparation_feat_eng_time_by_state_fes_create_data.R` to efficiently compile estimated FEs. 


## Imputation_Xgboost

### `Main/Training` and `Main/Bootstrap`

Contain scripts to estimate ML imputation models using Bayesian Optimization (BO) for model tuning/training and bootstrap procedure, respectively.

Scripts are run using the `sbatch_*` scripts in the respective directories. The SBATCH arguments `mem` and `-export=model_geography` should be adjusted according 
to urban/rural models. 

1. `model_imputation_xgboost_training.R` - Script to train `xgboost` models to obtain optimal hyperparameters and obtain final trained model using best parameters
with full sample. Run using `sbatch_models_training_imputation_xgboost.sh`. 
- `Function_Train_and_Test_Fold_IDs.R` - Creates indices of training/validation folds. 
- `Function_XGB_CV_Tuning.R` - Used for BO tuning procedure. 
- `Function_XGB_Final_Estimation.R` - Based on optimal hyperparameter set, program to estimate final cross-validation models and imputed outcomes. 

2. `models_imputation_xgboost_bootstrap.R` script to obtain bootstrapped counterfactual estimates. Run using `sbatch_models_bootstrap_imputation_xgboost.sh`


### LHS 

Contains scripts to estimate ML imputation models using Latin-Hypercube Sampling for model tuning. These results are a robustness check against the BO approach. 

Scripts used for model tuning, training, and bootstrap estimation under LHS approach: 
1. `models_imputation_xgboost_tuning_lhs.R`, 
2. `models_imputation_xgboost_training_lhs.R`
3. `models_imputation_xgboost_bootstrap_lhs.R` 

Run scripts using `sbatch_models_tuning_imputation_xgboost_lhs.sh` , `sbatch_models_training_imputation_xgboost_lhs.sh`, and `sbatch_models_bootstrap_imputation_xgboost_lhs.sh`, 
respectively. 

### LHS_w_Time_Trends

Scripts that implement LHS for model tuning, but data used in estimation now include `STATE + year` FEs and state-linear trends as controls instead of `State-x-year` FEs. These results 
were performed as a robustness check in response to reviewer comments. 

Scripts used for model tuning, training, and bootstrap estimation under LHS approach and state-linear trends along with state-by-year FEs. : 
1. `models_imputation_xgboost_tuning_lhs_w_time_trends.R`
2. `models_imputation_xgboost_training_lhs_w_time_trends.R`
3. `models_imputation_xgboost_bootstrap_lhs_w_time_trends.R`

Run scripts with `sbatch_models_tuning_imputation_xgboost_lhs_w_time_trends.sh`, `sbatch_models_training_imputation_xgboost_lhs_w_time_trends.sh`, `sbatch_models_bootstrap_imputation_xgboost_lhs_w_time_trends.sh`, 
respectively. 

## Imputation_RF

- Optimal hyperparameters in random forest were determined in two steps. First, we tuned `mtry`, `sample.fraction`, and `replace` while setting `ntrees` to 500. 
- After finding the optimal set of these parameters, we tune `ntrees` using the optimal parameter values. 
- We use the optimal hyperaparameters from both steps above to train final imputation models using random forest. 

Scripts used in tuning: 

1. `models_imputation_rf_tuning.R`
2. `models_imputation_rf_tuning_trees.R`
- Scripts to extract optimal parameter set among tuned models. 
3 `data_prep_opt_hyperparams.R`
4 `data_prep_opt_num_trees.R`


Scripts used in training and bootstrap procedure. 
5. `models_imputation_rf_training.R`
6. `models_imputation_rf_bootstrap.R`

Run scripts using `sbatch_models_tuning_imputation_rf.sh`, `sbatch_models_tuning_trees_imputation_rf.sh`, `sbatch_models_training_imputation_rf.sh`, `sbatch_models_bootstrap_imputation_rf.sh`
for tuning, training, and bootstrap estimation. 

`analysis_pred_probs_distribution.R`  
            

## LHS_Hyperparams

Contains scripts to create hyperparameter combinations using latin hypercube sampling in xgboost and random forest models, respectively. 

- xgboost
`data_preparation_lhs_hyperparameters.R`, `data_preparation_lhs_hyperparameters_w_trends.R`

-random forest
`data_preparation_lhs_hyperparameters_rf.R`


## RR1_01_FE_Comparison

Scripts to estimate and prepare tables of pre-treatment CV errors and post-treatment ATTs using state-level linear time trends with state and year FEs. 

1. `analysis_model_bootstrap_change_in_outcomes_array_lhs_w_time_trends.R`       
2. `rr_analysis_plot_change_in_outcomes_xgboost_lhs_w_time_trends.R`  
3. `rr_analysis_plot_change_in_outcomes_xgboost_lhs_w_time_trends_create_table.R`

## Figures_Actual_vs_Pred_RF

Scripts to create event-study type figures using random forest results. 

## Figures_Actual_vs_Pred

Scripts to create Figures 1 and 2 in main text. 

1. `sbatch_bootstrap_errors_and_counterfactuals_array.sh`
2. `analysis_model_bootstrap_errors_and_counterfactuals_array.R`
3. `analysis_plot_model_diagnostics_bootstrap.R` 
4. `analysis_plot_model_diagnostics_bootstrap_sourced.R` 

## Figures_ATT_Percent

Scripts to compute ATTs as a percentage of the average counterfactual, as shown in Figure C.5. 

1. `sbatch_bootstrap_change_in_outcomes_array.sh`
2. `analysis_model_bootstrap_change_in_outcomes_array.R` 
3. `analysis_plot_change_in_outcomes_sourced.R`
4. `analysis_plot_change_in_outcomes.R`
5. `analysis_section_5_2_change_in_outcomes.R`          


## Figures_Errors_and_Effects_on_Covars

Script to estimate bootstrap average CV errors and treatment effects across binned predictors, and treatment effects on standardized predictors.  

1. `sbatch_bootstrap_errors_and_effects_on_covars_array.sh`  
2. `analysis_model_bootstrap_effects_and_errors_on_covars_array.R`

For CV Errors. 
Creates Figures D.3 and D.6 (Supplementary Findings)

For Treatment Effects
Creates Figure 4 (main text) and Figures C.4 and C.8 (Supplementary Findings)

`analysis_plot_effects_and_errors_on_covars_sourced.R`
`analysis_plot_effects_and_errors_on_covars.R`
`analysis_plot_errors_on_covars.R`
`analysis_plot_effects_on_covars_and_dsvars.R`




## Figures_Effects_on_Grocery

Script to estimate bootstrap average CV errors and treatment effects by dollar store entries and baseline grocery stores. 

`sbatch_bootstrap_effects_and_interactions_array.sh`  
`analysis_model_bootstrap_effects_on_ds_entry_x_covars_array.R`

## Figures_Effects_on_DS_Policies

Scripts to estimate bootstrap average CV errors and treatment effects by presence of dollar store restriction policies and bans. 

`sbatch_bootstrap_effects_and_ds_policy_vars_array.sh`
`analysis_model_bootstrap_effects_on_ds_policy_vars_array.R`

Scripts to produce figures of average treatment effects across dollar store policy presence variables (Figures 5 and C.9. in main text).

`analysis_plot_effects_and_ds_policy_vars_sourced.R`        
`analysis_plot_effects_and_ds_policy_vars.R`                

## Figures_Errors_on_Grocery_and_DS_Policy

Scripts to estimate bootstrap average CV errors across baseline grocery stores and future dollar store entries, dollar store restriction policy presence,
and geographic region. 

`analysis_model_bootstrap_errors_on_ds_entry_x_grocery_array.R`
`sbatch_bootstrap_errors_on_ds_entry_x_grocery_array.sh`

Scripts to create figures showing average CV errors w.r.t. dollar store policy presence, baseline grocery stores and future dollar store entries, and geographic region. 

See Figures D.4, D.5, D.7, and D.8 of supplementary materials. 

`analysis_plot_errors_on_ds_entry_x_grocery_sourced.R`         
`analysis_plot_errors_on_ds_entry_x_grocery.R`
`data_preparation_errors_on_ds_entry_x_grocery_emp_estimates.R`


# Tables

## Tables_Model_Fit_Metrics

Scripts to create table of model fit using various accuracy measures. 

`sbatch_bootstrap_summary_fit_metrics_array.sh`
`rr_analysis_model_bootstrap_summary_fit_metrics_array.R`
`rr_analysis_model_summary_fit_metrics.R`
`rr_analysis_model_summary_fit_metrics_create_table_latex.R`

# Tables_Rel_Treat_Timing_on_Covars

Scripts used to create Table C.4.

`sbatch_bootstrap_rel_year_heterogeneity.sh`
`analysis_section_5_1_heterogeneity_by_rel_time_array.R`
`analysis_section_5_1_heterogeneity_by_rel_time.R`


## FE_Estimation

This directory contains the scripts to estimate time-by-state fixed effects using the pre-treatment data. 

Scripts include: 

1. `data_preparation_feat_eng_time_by_state_fes.R` 
2. `load_data_for_time_by_state_fixed_effects.R` - Called in `data_preparation_feat_eng_time_by_state_fes.R` to load data for estimation. 



`./analysis_model_bootstrap_change_in_outcomes_array.R`
`./analysis_model_bootstrap_effects_and_errors_on_covars_array.R`
`./analysis_model_bootstrap_effects_on_ds_entry_x_covars_array.R`
`./analysis_model_bootstrap_effects_on_ds_policy_vars_array.R`
`./analysis_model_bootstrap_effects_on_grocery_and_superettes_array.R`
`./analysis_model_bootstrap_errors_and_counterfactuals_array.R`
`./analysis_plot_change_in_outcomes.R`
`./analysis_plot_change_in_outcomes_sourced.R`
`./analysis_plot_effects_and_ds_policy_vars.R`
`./analysis_plot_effects_and_ds_policy_vars_sourced.R`
`./analysis_plot_effects_and_errors_on_covars.R`
`./analysis_plot_effects_and_errors_on_covars_sourced.R`
`./analysis_plot_effects_on_covars_and_dsvars.R`
`./analysis_plot_effects_on_ds_entry_x_covars_interactions.R`
`./analysis_plot_effects_on_ds_entry_x_covars_interactions_sourced.R`
`./analysis_plot_effects_on_grocery_and_superettes.R`
`./analysis_plot_effects_on_grocery_and_superettes_sourced.R`
`./analysis_plot_errors_on_covars.R`
`./analysis_plot_model_diagnostics_bootstrap.R`
`./analysis_plot_model_diagnostics_bootstrap_sourced.R`
`./analysis_section_5_2_change_in_outcomes.R`
`./analysis_section_5_3_multiple_ds_entries.R`
`./analysis_section_5_4_heterogeneity_by_covar.R`
`./analysis_section_5_4_heterogeneity_by_low_income_status.R`
`./analysis_section_5_5_heterogeneity_by_region.R`
`./analysis_section_5_6_heterogeneity_by_ds_policy_areas.R`
`./analysis_section_6_discussion_of_superettes.R`
`./analysis_section_6_discussion_of_superettes_sociodemographics_and_stores.R`
`./analysis_section_appendix_heterogeneity_by_ds_grocery_superettes.R`
`'./data_preparation_bootstrap_estimation_tracts.R'`
`./data_preparation_get_hyperparameters.R`
`./data_preparation_imputation_estimation.R`
`./data_preparation_prepare_binned_and_factor_covars.R`
`./data_preparation_prepare_binned_grocery_and_superette_counts_2005.R`
`./Imputation_Xgboost/models_imputation_xgboost_bootstrap.R`
`./Imputation_Xgboost/models_imputation_xgboost_training.R`
`./load_data_for_analysis_plot_effects_and_errors_on_covars.R`
`./load_data_for_analysis_plot_effects_on_interactions.R`
`./load_data_for_imputation_estimation.R`