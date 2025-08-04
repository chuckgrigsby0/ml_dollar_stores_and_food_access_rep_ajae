# The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis

## Code Repository

This repository contains all scripts and code necessary to replicate the analyses conducted in "The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis."

**Note:** Due to proprietary data restrictions, actual datasets are not included. This repository documents the workflow and code structure used in our research and can serve as a guide for adopting a similar methodology. 

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

**Model Training:**
- Rural models: 30 GB RAM, ~2-3 hours (including hyperparameter optimization)
- Urban models: 100 GB RAM, ~6-8 hours (including hyperparameter optimization)

**Bootstrap Estimation (499 iterations with job arrays):**
- Rural models: 21 GB RAM, ~30-45 minutes (with 50 parallel jobs)
- Urban models: 60 GB RAM, ~2.5-4 hours (with 50 parallel jobs)

- **Software**: R version 4.2, SLURM job scheduler for cluster computing. 

- **Runtime Notes**: Our analysis uses 2,128,234 urban and 801,771 rural block-group observations from 2005-2020. Running models sequentially would require ~21 hours (rural areas) and ~125 hours (urban areas), making cluster computing essential for our analysis. 
Researchers with smaller datasets may find the approach feasible on standard computing resources. Runtimes will also vary with machine learning algorithm, tuning, and training strategies employed. 

### Required R Packages

**Core Packages:**
- `pacman` - package management
- `here` - file path management  
- `dplyr`, `purrr`, `tidyr`, `stringr`, `broom` - data manipulation
- `xgboost` - gradient boosting models
- `fixest` - fixed effects estimation
- `future`, `furrr`, `parallelly` - parallel processing

**Additional Packages:**
- `ggplot2` - visualization
- `knitr`, `kableExtra` - table formatting
- `ParBayesianOptimization` - hyperparameter tuning

**Note:** Each script contains the necessary package loading commands at the top using `pacman::p_load()`. Additional packages may be required depending on specific analyses.

## Replication Instructions

### Data Preparation

The analysis uses helper scripts in `Code/Analysis/` that automatically load and prepare data. These scripts are called by main analysis programs and do not require separate execution. 

**Scripts to load and prepare data for main analysis:**
- `load_data_for_imputation_estimation.R` - Loads primary datasets. 
- `data_preparation_feat_eng_time_by_state_fes_create_data.R` - Prepares state-by-time estimated fixed effects. 
- `data_preparation_imputation_estimation.R` - Prepares data for analysis (original sample)
- `data_preparation_bootstrap_estimation_tracts.R` - Prepares bootstrap sample data
- `data_preparation_get_hyperparameters.R` - Loads optimal hyperparameter settings
 
<details>
<summary>Additional helper scripts</summary>

**Scripts used in evaluating cross-validation errors and treatment effect heterogeneity**
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

To train XGBoost models with hyperparameter optimization, navigate to `Code/Analysis/Imputation_Xgboost/Main/Training/` and run:

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
- Same email and account configuration training. 

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

Individual figures can alternatively be created by running the appropriate code found in each of the `Figures_*` directories. 

**Note:** Figures for supplementary analyses where we include superettes in the low-access indicator are produced separately as described in Step 4.  

**Tables:** Generate tables using scripts within `Tables_*` directories, creating `.csv` and `.tex` files based on outputs from Steps 1-3. 

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
│   │   ├── Low_Access/         # Bootstrap estimation results (urban models)
│   ├── Figures/                                                
│   │   ├── Low_Access/       
│   │   │   ├── Urban/          # Figures (urban models)                     
│   │   │   ├── Rural/          # Figures (rural models)
│   ├── Tables/                                                 
│   │   ├── Low_Access/
│   │   │   ├── Urban/          # Tables (urban models)
│   │   │   ├── Rural/          # Tables (rural models)
```

**Note:** Steps 1-3 populate the training and bootstrap directories with estimation results, while figures and tables are output from these results and saved in their respective directories.

## Questions 

For questions about the code structure, please contact authors.

## Citation

Grigsby-Calage, Chuck and Mullally, Conner and Volpe, Richard and Kropp, Jaclyn D. and Stevens, Alexander, 
The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis (August 4, 2025). 
Available at SSRN: https://ssrn.com/abstract=4822647 or http://dx.doi.org/10.2139/ssrn.4822647

---
