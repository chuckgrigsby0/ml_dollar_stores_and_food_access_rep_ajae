# The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis

## Code Repository

This repository contains all scripts and code necessary to replicate the analyses conducted in "The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis."

**Note:** This read-me file documents the project organization and workflow for replication. However, due to our use of proprietary data, the actual datasets are not included. 

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
│   │   └── [Directories for individual analysis scripts]
│   ├── Analysis_Supplementary_w_Superettes/ # Supplementary analysis including superettes in low-access indicator (same structure)
│   ├── Analysis_Placebo # Supplementary analysis for placebo treatment effects (same structure)
│   └── Functions/                        # Helper functions and utilities
└── README.md
```

## Computational Requirements

### System Requirements
- **Memory**: 30-100 GB RAM for model training (varies by urban/rural models)
- **Memory**: 21-80 GB RAM for bootstrap procedures
- **Software**: R with required packages, SLURM job scheduler (for cluster computing). We used R version 4.2 throughout our analyses. 

### Required R Packages
**Core Packages:**
- `pacman` - `p_load()` to install and load packages
- `here` - reference directories 
- `dplyr`, `purrr`, `broom`, `tidyr`, `stringr` - data manipulation
- `xgboost`, `ranger` - machine learning models
- `fixest` - fixed effects estimation and other helper functions
- `future`, `furrr`, `parallelly` - parallel processing
- `ggplot2` - for figure creation
- `knitr`, `kableExtra` - for latex table creation

**Note:** Each script contains the necessary package loading commands at the top using `pacman::p_load()`. Additional packages may be required depending on specific analyses.

## Replication Instructions

### Step 1: Data Preparation
`Code/Analysis/` contains several helper scripts that prepare and load data depending on the analysis. These scripts are automatically called by programs and do not need to be run separately:

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

### Step 2: Model Training and Bootstrap Estimation

#### XGBoost Model Training

To train the XGBoost models, determine optimal parameters, and estimate a final model using the best parameter combination, go to `Code/Analysis/Imputation_Xgboost/Main/Training/` and run:

```bash
sbatch sbatch_models_training_imputation_xgboost.sh
```

**Configuration Notes:**
- Adjust SBATCH `--mem` parameter based on model type (approx. 30 GB in rural models and 100 GB in urban)
- Set `--export=model_geography` to specify Urban or Rural models
- Update `--mail-user=useremail` with your email address
- Update `--qos=accountname-b` with your account username
- If not using SLURM, you can adjust the R scripts to run on your local machine with sufficient memory and processing capabilities. 
- Update file paths as necessary. 

**Email and Account Settings**
Before running the SBATCH script, update the following placeholders:
- `useremail` → your email address
- `accountname-b` → your cluster account name

#### Bootstrap Procedure

For statistical inference, we generate bootstrapped standard errors and confidence intervals by repeatedly implementing the training procedure across 499 bootstrap samples, where block groups are sampled at the census-tract level. 

Open `Code/Analysis/Imputation_Xgboost/Main/Bootstrap/` and run:

```bash
sbatch sbatch_models_bootstrap_imputation_xgboost.sh
```

The bash script will produce 499 sets of pre-treatment cross-validation predictions and errors, post-treatment counterfactual low-access status, and estimated treatment effects based on bootstrapped samples. 

**Configuration Notes:**
- Memory requirements: 21-80 GB depending on rural versus urban models
- Same email and account configuration training. 

### Step 3: Generate Bootstrapped Estimates of Model Diagnostics and Analyses of Treatment Effects

Using the bootstrapped pre-treatment cross-validation errors, post-treatment counterfactual predicted outcomes, and treatment effects estimated above:
- Assess cross-validation errors, conduct model diagnostics, evaluate model assumptions, etc. 
- Examine treatment effect heterogeneity 

In `Code/Analysis`, run the following script:

```bash
bash Code/Analysis/sbatch_bootstrap_all_output.sh
```

This produces bootstrapped estimates of analyses evaluating model fit on pre-treatment data, average cross-validation errros and treatment effects with respect to time from treatment, 
and variation of treatment effects across socio-demographic and geographic characteristics, baseline grocery store counts, and presence of dollar store policies.

**Configuration Notes:**
- Memory requirements: We set `--mem=20gb` in rural and urban models to ensure that all data and analyses could be run without errors. 
- Set `--export=model_geography` to specify Urban or Rural models
- Same email and account configuration training. 

**Note** Individual analyses can be run separately, but `sbatch_bootstrap_all_output.sh` runs all analyses in a single script. 

#### Tables
Tables are created using scripts in directories with the `Tables_*` prefix. These generate output in both `.csv` and `.tex` formats for main and supplementary analyses.

#### Figures

The following bash script can be run to generate all figures presented in the main analyses and parts of the appendinx. 

```bash
bash Code/Analysis/sbatch_figures.sh
```

**Configuration Notes:**
- Memory requirements: We set `--mem=20gb` to ensure that all figures are created without errors. 
- Script creates figures for urban and rural model results. 
- Directories for figures should be created prior to running script. 

Alternatively individual figures can be created by running the code found in each of the `Figures_*` directories. 


### Step 4: Supplementary Analyses with Superettes

For the supplementary analysis using the modified low-access indicator that includes superettes:

1. Navigate to `Code/Analysis_Supplementary_w_Superettes/`
2. Follow the same procedure as the main analysis (Steps 1-3)
3. The directory structure mirrors `Code/Analysis/` but uses the modified indicator

### Additional Analyses

The additional scripts and programs included in the repository can be run individually or were run as supplementary analyses to address reviewer comments. 

## Questions 

For questions about the methods or code structure, please contact xxxx.

## Citation

xxx

---
