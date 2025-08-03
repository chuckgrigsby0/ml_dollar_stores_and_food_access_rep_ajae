# The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis

## Code Repository

This repository contains all scripts and code necessary to replicate the analyses conducted in "The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis."

**Note:** The repository documents the methodological procedures and code structure used in our research. Due to proprietary data restrictions, actual datasets are not included. This repository serves as a guide for researchers with access to similar data sources.

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
│   ├── Analysis_Supplementary_w_Superettes/ # Supplementary analysis including superettes in low-access indicator (same structure as Code/Analysis/)
│   └── Functions/                        # Helper functions and utilities
└── README.md
```


**Note** The repository contains several other folders and scripts that run additional machine learning algorithms (e.g., random forest), implement alternative tuning strategies, and perform various robustness checks of our main results to address reviewer comments.
While included in the repository for transparency, we do not describe in detail the procedures for running these supplementary scripts. However, the folders and scripts generally follow the same organization and workflow as our main analyses. 


## Computational Requirements

### System Requirements
- **Memory and runtime**: 30 GB RAM and 2-3 hours for model training of rural models; 100 GB and 6-8 hours for urban models. 
- **Memory and runtime**: 21 GB RAM and 30 minutes-1.5 hours for bootstrap procedures of rural models using job arrays; 80 GB and 2.5-4 hours for urban models. 
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

`Code/Analysis/` contains several helper scripts that prepare and load data depending on the analysis. These helper scripts are automatically called by other scripts used to generate our main results and do not need to be run separately:

**Helper Scripts** (located in `Code/Analysis/`):
- `data_preparation_bootstrap_estimation_tracts_w_time_trends.R`
- `data_preparation_bootstrap_estimation_tracts.R`
- `data_preparation_did_regressions.R`
- `data_preparation_feat_eng_time_by_state_fes_create_data.R`
- `data_preparation_feat_eng_time_state_and_linear_trends_fes_create_data.R`
- `data_preparation_get_hyperparameters.R`
- `data_preparation_imputation_estimation_national.R`
- `data_preparation_imputation_estimation_w_time_trends.R`
- `data_preparation_imputation_estimation.R`
- `data_preparation_model_covars_lists.R`
- `data_preparation_prepare_binned_and_factor_covars.R`
- `data_preparation_prepare_binned_ds_policy_vars.R`
- `data_preparation_prepare_pretreatment_binned_grocery_and_ds_policy.R`
- `load_data_for_imputation_estimation_w_time_trends.R`
- `load_data_for_imputation_estimation.R`

### Model Training and Bootstrap Estimation

#### Step 1: XGBoost Model Training

To train the XGBoost models, find optimal hyperparameter configurations, and estimate final models using optimal parameter combinations, go to `Code/Analysis/Imputation_Xgboost/Main/Training/` and run:

```bash
sbatch sbatch_models_training_imputation_xgboost.sh
```
The bash script generates out-of-sample predicted outcomes and errors from cross-validation using pre-treatment data and predicted counterfactual low-access status in the absence of dollar store entry using post-treatment data, based on the empirical sample of block groups. 

Before running the SBATCH script, the following placeholders or arguments should be updated:

**Configuration Notes:**
- Adjust SBATCH `--mem` parameter based on model type (approx. 30 GB in rural models and 100 GB in urban)
- Set `--export=model_geography` to specify Urban or Rural models
- Update `--mail-user=useremail`, replacing `useremail` email address
- Update `--qos=accountname-b`, replacing `accountname` with account username
- Update file paths in R scripts as necessary. 

#### Step 2: Bootstrap Procedure of XGBoost

For statistical inference, we generate bootstrapped standard errors using 499 bootstrap samples. Block groups are resampled at the census-tract level, and the entire training procedure is repeated for each sample.

Open `Code/Analysis/Imputation_Xgboost/Main/Bootstrap/` and run:

```bash
sbatch sbatch_models_bootstrap_imputation_xgboost.sh
```

The bash script produces 499 bootstrapped sets of cross-validated predicted outcomes and errors using pre-treatment data, counterfactual predicted low-access status using post-treatment data, and estimated treatment effects. 

**Configuration Notes:**
- Memory requirements: 21 GB for rural models and 80 GB for urban models
- Same email and account configuration training. 

### Step 3: Bootstrapped Estimates of Model Diagnostics and Analyses of Treatment Effects

We use the bootstrapped results above (pre-treatment cross-validation predicted outcomes and errors, predicted post-treatment counterfactual outcomes, and estimated treatment effects) to:
- Assess average cross-validation errors, conduct model diagnostics, and evaluate model assumptions 
- Examine average treatment effects and treatment effect heterogeneity 

In `Code/Analysis`, run the following script:

```bash
bash Code/Analysis/sbatch_bootstrap_all_output.sh
```

This produces bootstrapped estimates for analyses that evaluate model fit on pre-treatment data (i.e., comparing actual vs predicted low-access status), average cross-validation errors and treatment effects with respect to time from treatment, 
and variation of treatment effects across socio-demographic and geographic characteristics, baseline grocery store counts, and presence of dollar store policies.

**Configuration Notes:**
- Memory requirements: we set `--mem=20gb` in rural and urban models to ensure that all data and analyses could be run without errors. 
- Set `--export=model_geography` to specify Urban or Rural models
- Same email and account configuration training. 

**Note** Individual analyses can be run separately, but `sbatch_bootstrap_all_output.sh` runs all analyses in a single script. 

#### Figures

The following bash script generates all figures presented in the main results and appendix. Figures for supplementary analyses (which include superettes in the low-access indicator) are produced separately as described in Step 4.  

```bash
bash Code/Analysis/sbatch_figures.sh
```

**Configuration Notes:**
- Memory requirements: We set `--mem=20gb` to ensure all figures are created without exceeding memory limits. 
- Script creates figures for both urban and rural model results. 
- Directories for figures should be created prior to running script. 

Individual figures can also be created by running the appropriate code found in each of the `Figures_*` directories. 

#### Tables

Tables are created using scripts in directories with the `Tables_*` prefix. These generate output in `.csv` or `.tex` formats for main and supplementary analyses.

### Step 4: Supplementary Analyses with Superettes

For the supplementary analyses using the modified low-access indicator that includes superettes:

1. Open `Code/Analysis_Supplementary_w_Superettes/`
2. Follow the same procedure as the main analysis (Steps 1-3)
3. The directory structure mirrors `Code/Analysis/` but uses the modified indicator


## Questions 

For questions about the methods or code structure, please contact xxx.

## Citation

xxx

---
