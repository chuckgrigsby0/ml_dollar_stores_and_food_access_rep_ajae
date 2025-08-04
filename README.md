# The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis

## Code Repository

This repository contains all scripts and code necessary to replicate the analyses conducted in "The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis."

**Note:** Due to proprietary data restrictions, actual datasets are not included. This repository documents the workflow and code structure used in our research and can serve as a guide for researchers adopting a similar methodology. 

## Repository Structure

```
├── Code/
│   ├── Analysis/                          # Main analysis scripts
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

- **Note on runtimes**: Running rural models sequentially, as opposed to job arrays, would take approximately 21 hours, while urban models would take approximately 125 hours. 

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

`Code/Analysis/` contains various helper scripts that automatically load and prepare data for analyses. 
These scripts are called by the main analysis programs and do not need to be run separately.

**Loading/Preparing Data for Main Analysis:**
- `load_data_for_imputation_estimation.R` - Loads data. 
- `data_preparation_feat_eng_time_by_state_fes_create_data.R` - Called in above script to load and prepare feature-engineered fixed effects
- `data_preparation_imputation_estimation.R`: Prepares data for analysis (original sample)
- `data_preparation_bootstrap_estimation_tracts.R`: Prepares data for analysis (bootstrap sample)
- `data_preparation_get_hyperparameters.R`: Called in script above to load optimal hyperparameter settings found during model training
 
<details>
<summary>Additional helper scripts</summary>

**Scripts for evaluating cross-validation errors and treatment effect heterogeneity**
- `data_preparation_model_covars_lists.R`
- `data_preparation_prepare_binned_and_factor_covars.R`
- `data_preparation_prepare_binned_ds_policy_vars.R`
- `data_preparation_prepare_pretreatment_binned_grocery_and_ds_policy.R`

**Scripts for loading and preparing data in supplementary analyses**
- `load_data_for_imputation_estimation_w_time_trends.R`
- `data_preparation_feat_eng_time_state_and_linear_trends_fes_create_data.R`
- `data_preparation_imputation_estimation_w_time_trends.R`
- `data_preparation_bootstrap_estimation_tracts_w_time_trends.R`
- `data_preparation_imputation_estimation_national.R`
- `data_preparation_did_regressions.R`
</details>

### Model Training and Bootstrap Estimation

#### Step 1: XGBoost Model Training

To train the XGBoost models, find optimal hyperparameter configurations, and estimate final models using optimal parameter combinations, go to `Code/Analysis/Imputation_Xgboost/Main/Training/` and run:

```bash
sbatch sbatch_models_training_imputation_xgboost.sh
```
This script creates three main outputs: 
1. **Model Tuning**: Determines optimal set of XGBoost hyperparameters 
2. **Cross-validation**: Out-of-sample predictions and errors using full set of pre-treatment block groups
3. **Treatment effect estimates**: Counterfactual low-access status in the absence of dollar store entry and treatment effects for full set of treated block groups

Before running the SBATCH script, the following placeholders or arguments should be updated:

**Configuration Notes:**
- Urban models: `--mem=100gb --cpus-per-task=3`
- Rural models: `--mem=30gb --cpus-per-task=3`
- `--export=model_geography=Urban` (or `Rural`)
- Update `--mail-user=useremail`, replacing `useremail` email address
- Update `--qos=accountname-b`, replacing `accountname` with account username
- Update file paths in R scripts as necessary. 

#### Step 2: Bootstrap Procedure of XGBoost

For statistical inference, we generate bootstrapped standard errors using 499 bootstrap samples. Block groups are resampled at the census-tract level, and the entire training procedure is repeated for each sample.

Open `Code/Analysis/Imputation_Xgboost/Main/Bootstrap/` and run:

```bash
sbatch sbatch_models_bootstrap_imputation_xgboost.sh
```

The bash script produces 499 bootstrap iterations, each generating:
1. **Cross-validation results**: Out-of-sample predictions and errors using pre-treatment data
2. **Treatment effect estimates**: Counterfactual predictions and treatment effects using post-treatment data

**Configuration Notes:**
- Rural models: `--mem=21gb --cpus-per-task=3`
- Urban models: `--mem=60gb --cpus-per-task=3`
- Same email and account configuration from training. 

### Step 3: Bootstrapped Estimates of Model Diagnostics and Analyses of Treatment Effects

We use the bootstrapped results above (cross-validation predictions and errors, predicted counterfactual outcomes and treatment effect estimates) to:
- Assess average cross-validation errors, conduct model diagnostics, and evaluate model assumptions 
- Examine average treatment effects and treatment effect heterogeneity 

In `Code/Analysis`, run the following script:

```bash
bash Code/Analysis/sbatch_bootstrap_all_output.sh
```

This produces bootstrapped estimates for analyses that evaluate model fit on pre-treatment data (i.e., comparing actual vs predicted low-access status), average cross-validation errors and treatment effects with respect to time from treatment and covariates, 
and treatment effect heterogeneity across socio-demographic and geographic characteristics, baseline grocery store counts, and presence of dollar store policies.

**Configuration Notes:**
- `--mem=20gb --cpus-per-task=1` (ensures all analyses complete without errors)
- `--export=model_geography=Urban` (or `Rural`)
- Same email and account configuration training. 

**Note:** Individual analyses can be run separately, but `sbatch_bootstrap_all_output.sh` runs all analyses in a single script. 

#### Figures

The following bash script uses the output produced in Steps 1-3 to generate all figures presented in the main results and appendix. Figures for supplementary analyses that include superettes in the low-access indicator are produced separately as described in Step 4.  

```bash
bash Code/Analysis/sbatch_figures.sh
```

**Configuration Notes:**
- `--mem=20gb --cpus-per-task=1` (ensures all figures are created without errors)
- Script creates figures for both urban and rural model results. 
- Directories for figures should be created prior to running script. 

Individual figures can alternatively be created by running the appropriate code found in each of the `Figures_*` directories. 

#### Tables

Tables are generated using scripts in `Tables_*` directories. These scripts use outputs from Steps 1-3 and produce `.csv` and `.tex` files.

### Step 4: Supplementary Analyses with Superettes

For the supplementary analyses using the modified low-access indicator that includes superettes:

1. Open `Code/Analysis_Supplementary_w_Superettes/`
2. Follow the same procedure as the main analysis (Steps 1-3)
3. The directory structure mirrors `Code/Analysis/` but uses the modified indicator


## Questions 

For questions about the code structure, please contact paper authors.

## Citation

xxx

---
