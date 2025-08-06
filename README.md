# The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis

## Code Repository

This repository contains all scripts and code necessary to replicate the analyses conducted in "The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis."

## Data Availability and Sources

This repository documents the workflow and code structure used in our research and can serve as a guide for adopting a similar methodology. 

Due to proprietary data restrictions, actual datasets are not included in this repository. However, nearly all data used in our study are publicly available and can be obtained directly from the listed sources.

Our analysis employs the following datasets:

1. **NielsenIQ's TDLinx** - Dollar store entry information, low-access outcomes, and baseline retail variables are derived from TDLinx, a comprehensive national database of U.S. food retailers. Data were accessed through a third-party agreement with the USDA Economic Research Service.

2. **U.S. Census Bureau** - Block group-level demographic and socioeconomic data are obtained from the 2000 Decennial Census and American Community Survey (ACS) Five-Year estimates for 2006-2010, 2011-2015, and 2016-2020.

3. **National Land Cover Database (NLCD)** - Land-use shares for census block groups are computed using NLCD data for 2004, 2006, 2009, 2011, 2013, 2016, and 2019.

4. **Parks and Recreation Data** - Block group-level park accessibility measures are created by combining ESRI's USA Parks, Trust for Public Land's ParkServe, and The National Conservation Easement Database.

5. **Educational Facilities** - Public and private school counts are derived from the Homeland Infrastructure Foundation-Level Data platform, which incorporates U.S. Department of Education sources including the Common Core of Data and Private School Universe Survey.

6. **Transportation Infrastructure** - Road network measures by type (interstate, U.S./state/county highways, local roads) at the block-group level are calculated using U.S. Census Bureau primary and secondary roads data.

7. **Urban-Rural Classification** - Block-group urban and rural classification is based on the 2010 U.S. Census Bureau definition and delineation of urban areas.

8. **Dollar Store Policy Data** - City and county dollar store restriction and ban policy information is obtained from the Institute for Local Self-Reliance (ILSR).

9. **Block Group Centroids** - Population-weighted centroids are obtained from the U.S. Census Bureau for calculating geographic and network distances and spatial analysis.

## Repository Structure

```
├── Code/
│   ├── Analysis/                         # Main analysis scripts
│   │   ├── Imputation_Xgboost/           # XGBoost model training and bootstrap
│   │   │   ├── Main/
│   │   │   │   ├── Training/             # Model training scripts
│   │   │   │   └── Bootstrap/            # Bootstrap estimation scripts
│   │   ├── Tables_*/                     # Table generation scripts
│   │   ├── Figures_*/                    # Figure generation scripts
│   ├── Analysis_Supplementary_w_Superettes/ # Supplementary analysis including superettes in the low-access indicator (same structure as Code/Analysis/)
│   └── Functions/                        # Helper functions and utilities
└── README.md
```


**Note:** The repository contains several other folders and scripts that run additional machine learning algorithms (e.g., random forest), implement alternative tuning strategies, and perform various robustness checks of our main results.
While these additional scripts are included in the repository for transparency, we do not provide detailed procedures for running them here. However, the folders and scripts generally follow the same organization and workflow as our main analyses. 


## Computational Requirements

### System Requirements

**Software:** R version 4.2. Many of the analyses are run using bash scripts and a SLURM job scheduler for cluster computing, which may require Linux. 

**Model Training:**
- Rural models: 30 GB RAM, ~2-3 hours (including hyperparameter optimization)
- Urban models: 100 GB RAM, ~6-8 hours (including hyperparameter optimization)

**Bootstrap Estimation (499 iterations with job arrays):**
- Rural models: 21 GB RAM, ~30-45 minutes (with 50 parallel jobs)
- Urban models: 60 GB RAM, ~2.5-4 hours (with 50 parallel jobs)

**Runtime Notes:** Our data contain 2,128,234 urban and 801,771 rural block-group observations from 2005-2020. Running models sequentially require ~21 hours (rural areas) and ~125 hours (urban areas), making cluster computing essential for our analysis. 
Researchers with smaller datasets may find the approach feasible on standard computing resources. Runtimes will also vary with machine learning algorithm, tuning, and training strategies employed. 

**Approximate storage space needed:** ~150 GB to store data and output files

### Required R Packages

**Primary Packages:**
- `pacman` - package management
- `here` - file path management  
- `dplyr`, `purrr`, `tidyr`, `stringr`, `broom` - data manipulation
- `xgboost` - gradient boosting models
- `fixest` - fixed effects estimation
- `future`, `furrr`, `parallelly` - parallel processing

**Additional Packages:**
- `ggplot2`, `tmap` - visualization
- `knitr`, `kableExtra` - table formatting
- `ParBayesianOptimization` - hyperparameter tuning
- `sf` - spatial data analysis

**Note:** Each script contains the necessary package loading commands at the top using `pacman::p_load()`. Additional packages may be required depending on specific analyses.

## Replication Instructions

### Data Preparation

We created several helper scripts in `Code/Analysis/` that automatically load and prepare data. These scripts are called by main analysis programs and do not require separate execution. 

**Scripts to load and prepare data for main analysis:**
- `load_data_for_imputation_estimation.R` - Loads primary datasets
- `data_preparation_feat_eng_time_by_state_fes_create_data.R` - Prepares state-by-time estimated fixed effects
- `data_preparation_imputation_estimation.R` - Prepares data for analysis (original sample)
- `data_preparation_bootstrap_estimation_tracts.R` - Prepares bootstrap sample data
- `data_preparation_get_hyperparameters.R` - Loads optimal hyperparameter settings (used in bootstrap estimation)
 
<details>
<summary>Additional helper scripts</summary>

**Scripts used in evaluating cross-validation errors, treatment effect heterogeneity, and creating figures and tables**
- `data_preparation_model_covars_lists.R`
- `data_preparation_prepare_binned_and_factor_covars.R`
- `data_preparation_prepare_binned_ds_policy_vars.R`
- `data_preparation_prepare_pretreatment_binned_grocery_and_ds_policy.R`

**Scripts to load and prepare data in supplementary analyses**
- `load_data_for_imputation_estimation_w_time_trends.R`
- `data_preparation_feat_eng_time_state_and_linear_trends_fes_create_data.R`
- `data_preparation_imputation_estimation_w_time_trends.R`
- `data_preparation_bootstrap_estimation_tracts_w_time_trends.R`
- `data_preparation_imputation_estimation_national.R`
- `data_preparation_did_regressions.R`
</details>

### Model Training and Bootstrap Estimation

#### Step 1: XGBoost Model Training

To train and tune XGBoost models, navigate to `Code/Analysis/Imputation_Xgboost/Main/Training/` and run:

```bash
sbatch sbatch_models_training_imputation_xgboost.sh
```
This script generates three main outputs: 
1. **Model Tuning**: Determines optimal set of XGBoost hyperparameters 
2. **Cross-validation (CV)**: Out-of-sample predictions and errors using full set of pre-treatment block groups
3. **Treatment effect estimates**: Counterfactual low-access status and treatment effects for treated block groups

Before running the SBATCH script, the following placeholders or arguments should be updated:

**Configuration Notes:**
- Urban models: `--mem=100gb --cpus-per-task=3`
- Rural models: `--mem=30gb --cpus-per-task=3`
- `--export=model_geography=Urban` (or `Rural`)
- Update `--mail-user=useremail`, replacing `useremail` with your email address
- Update `--qos=accountname-b`, replacing `accountname` with your account username
- Update file paths in R scripts as necessary. 

#### Step 2: Bootstrap Procedure

For statistical inference, we generate bootstrapped standard errors using 499 bootstrap samples. Block groups are resampled at the census-tract level, and the entire training procedure is repeated for each sample.

To implement the bootstrap procedure, open `Code/Analysis/Imputation_Xgboost/Main/Bootstrap/` and run:

```bash
sbatch sbatch_models_bootstrap_imputation_xgboost.sh
```

The bash script produces 499 bootstrap iterations, each generating:
1. **CV results**: Out-of-sample predictions and errors using pre-treatment data
2. **Treatment effect estimates**: Counterfactual predictions and treatment effects using post-treatment data

**Configuration Notes:**
- Rural models: `--mem=21gb --cpus-per-task=3`
- Urban models: `--mem=60gb --cpus-per-task=3`
- Same email and account configuration from training. 

#### Step 3: Bootstrapped Estimates of Model Diagnostics and Analyses of Treatment Effects

We use the bootstrapped results above (CV predictions and errors, predicted counterfactual outcomes and estimated treatment effects) to:
- Assess average CV errors, conduct model diagnostics, and evaluate model assumptions 
- Examine average treatment effects and treatment effect heterogeneity 

In `Code/Analysis`, run the following script:

```bash
bash Code/Analysis/sbatch_bootstrap_all_output.sh
```

This produces bootstrapped estimates for three types of analyses: 
- Model diagnostics (i.e., comparing actual vs predicted low-access status) using pre-treatment data 
- Average treatment effects over time and across covariates
- Treatment effect heterogeneity by socio-demographics, geography, baseline grocery store counts, and presence of dollar store policies.

**Configuration Notes:**
- `--mem=20gb --cpus-per-task=1` (ensures all analyses complete without errors)
- `--export=model_geography=Urban` (or `Rural`)
- Same email and account configuration from training. 

**Note:** Individual analyses can be run separately, but `sbatch_bootstrap_all_output.sh` runs all analyses in a single script. 

### Figures and Tables

To generate all main paper and appendix figures using outputs from Steps 1-3, run: 

```bash
bash Code/Analysis/sbatch_figures.sh
```

**Configuration Notes:**
- `--mem=20gb --cpus-per-task=1` (ensures all figures are created without errors)
- Script creates figures for both urban and rural model results. 
- Directories for figures should be created prior to running script. 

**Note:** Individual figures can alternatively be created by running the appropriate code found in each of the `Figures_*` directories. 
Figures for supplementary analyses where we include superettes in the low-access indicator are produced separately as described in Step 4.  

**Tables:** Generate tables using scripts within `Tables_*` directories, which create `.csv` or `.tex` files based on outputs from Steps 1-3. 

### Supplementary Analyses with Superettes

For the supplementary analyses using the modified low-access indicator that includes superettes:

1. Open `Code/Analysis_Supplementary_w_Superettes/`
2. Follow the same procedure as the main analysis (Steps 1-3)
3. The directory structure mirrors `Code/Analysis/` but uses the modified indicator


## Output

Results are organized in the following directory structure: 

```
├── Analysis/
│   ├── Model_Training/                                         
│   │   ├── Low_Access/         # Model training results and hyperparameters
│   ├── Urban_Bootstrap/                                        
│   │   ├── Low_Access/         # Bootstrap estimation results (urban models)
│   ├── Rural_Bootstrap/                                        
│   │   ├── Low_Access/         # Bootstrap estimation results (rural models)
│   ├── Figures/                                                
│   │   ├── Low_Access/       
│   │   │   ├── Urban/          # Figures (urban models)                     
│   │   │   ├── Rural/          # Figures (rural models)
│   ├── Tables/                                                 
│   │   ├── Low_Access/
│   │   │   ├── Urban/          # Tables (urban models)
│   │   │   ├── Rural/          # Tables (rural models)
```

**Note:** Steps 1-3 populate the estimation directories (Model_Training, Urban_Bootstrap, and Rural_Bootstrap), while figures and tables are created from these results and saved in their respective subdirectories.

<details>

<summary>Bootstrap Output Directories</summary>
The following subdirectories contain urban and rural bootstrap results: 
- `bootstrap_01_499_tracts`                                  
- `bootstrap_01_499_tracts_lhs`                              
- `bootstrap_01_499_tracts_ols`                              
- `bootstrap_01_499_tracts_rf`                               
- `bootstrap_01_499_tracts_w_time_trends_lhs`                
- `bootstrap_change_in_outcomes_tracts`                      
- `bootstrap_change_in_outcomes_tracts_lhs`                  
- `bootstrap_change_in_outcomes_tracts_lhs_w_time_trends`    
- `bootstrap_change_in_outcomes_tracts_ols`                  
- `bootstrap_change_in_outcomes_tracts_rf`                   
- `bootstrap_cv_error_and_att_tracts`                        
- `bootstrap_effects_by_interactions_tracts`                 
- `bootstrap_effects_on_baseline_store_counts_x_entry_tracts`
- `bootstrap_effects_on_ds_policy_vars_tracts`               
- `bootstrap_error_by_grocery_entry_policy_region_tracts`    
- `bootstrap_errors_and_effects_by_covars_tracts`            
- `bootstrap_errors_and_effects_by_entry_x_grocery`          
- `bootstrap_errors_and_predictions_mult_thresh_tracts`      
- `bootstrap_errors_and_predictions_tracts`                  
- `bootstrap_errors_and_predictions_tracts_rf`               
- `bootstrap_errors_on_baseline_store_counts_x_entry_tracts` 
- `bootstrap_fit_metrics_tracts`                             
- `bootstrap_placebo_att_tracts`                             
- `bootstrap_rel_year_heterogeneity_tracts`                  
- `bootstrap_ruca_urban_rural_comparison`                    
- `bootstrap_total_new_grocers_cf`
</details>

<details>
<summary>Figure Output Directories</summary>
The following subdirectories contain publication-ready figures: 
- `descriptive_statistics`                     
- `dollar_store_growth_by_time_tracts`         
- `effects_by_region_and_division_tracts`      
- `effects_on_covars_bins_tracts`              
- `effects_on_covars_norm_tracts`              
- `effects_on_dollar_stores_tracts`            
- `effects_on_ds_policy_vars_tracts`           
- `effects_on_interactions_tracts`             
- `effects_on_store_counts_2005_x_entry_tracts`
- `errors_and_preds_mult_thresh_tracts`        
- `errors_and_preds_tracts`                    
- `errors_and_preds_tracts_ols`                
- `errors_and_preds_tracts_rf`                 
- `errors_by_region_tracts`                    
- `errors_on_covars_bins_tracts`               
- `errors_on_covars_norm_tracts`               
- `errors_on_ds_policy_vars_tracts`            
- `errors_on_grocery_stores_2005_tracts`       
- `errors_on_store_counts_2005_x_entry_tracts` 
- `maps`                                       
- `roc_and_pr_curves`
</details>

## Questions 

For questions about the workflow or code structure, please contact authors.

## Citation

Grigsby-Calage, Chuck and Mullally, Conner and Volpe, Richard and Kropp, Jaclyn D. and Stevens, Alexander, 
The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis (August 4, 2025). 
Available at SSRN: https://ssrn.com/abstract=4822647 or http://dx.doi.org/10.2139/ssrn.4822647

---
