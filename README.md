# The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis

## Code Repository

This repository contains all scripts and code necessary to replicate the analyses conducted in "The Varying Effects of Dollar Stores on Food Access: A Machine Learning Analysis."

**Note:** This read-me file documents the project organization and workflow for replication. However, because of the proprietary nature of the data, the actual datasets are not included. 

This repository serves as a guide for researchers with access to similar data, methodological approach, and computational resources.

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
- `dplyr`, `purrr`, `tidyr`, `stringr` - data manipulation
- `xgboost` - gradient boosting models
- `fixest` - fixed effects estimation
- `future`, `furrr`, `parallelly` - parallel processing

**Note:** Each script contains the necessary package loading commands at the top using `pacman::p_load()`. Additional packages may be required depending on specific analyses.

## Replication Instructions

### Step 1: Data Preparation
`Code/Analysis/` contains several helper scripts that prepare and load data. These scripts are automatically called by programs and do not need to be run separately:

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

To train the XGBoost models and determine optimal parameters, go to `Code/Analysis/Imputation_Xgboost/Main/Training/` and run:

```bash
sbatch sbatch_models_training_imputation_xgboost.sh
```

**Configuration Notes:**
- Adjust SBATCH `--mem` parameter based on model type (approx. 30 GB in rural models and 100 GB in urban)
- Set `--export=model_geography` to specify Urban or Rural models
- Update `--mail-user=useremail` with your email address
- Update `--qos=accountname-b` with your account username

#### Bootstrap Procedures

Using the optimal parameter values from training, we estimate 499 XGBoost models using bootstrapped samples, generating 499 sets of pre-treatment cross-validation errors, post-treatment counterfactual low-access status, and treatment effects using bootstrapped samples. 

After model training is complete, open `Code/Analysis/Imputation_Xgboost/Main/Bootstrap/` and run:

```bash
sbatch sbatch_models_bootstrap_imputation_xgboost.sh
```

**Configuration Notes:**
- Memory requirements: 21-80 GB depending on rural versus urban models
- Same email and account configuration as training step

### Step 3: Analysis and Results Generation

#### Generate All Bootstrap Outputs

The following script uses the cross-validation error estimates, post-treatment predictions of counterfactual low-access status, and treatment effects from the bootstrapped samples to execute various R programs that:
- Analyze cross-validation errors, conduct model diagnostics, evaluate model assumptions, etc. 
- Examine treatment effect heterogeneity
- Process bootstrapped estimates

In `Code/Analysis`, run the following script:

```bash
bash Code/Analysis/sbatch_bootstrap_all_output.sh
```

**Note** Individual analyses can be run separately, but `sbatch_bootstrap_all_output.sh` contains all analyses in a single script. 

#### Generate Tables
Tables are created using scripts in directories with the `Tables_*` prefix. These generate output in both `.csv` and `.tex` formats for main and supplementary analyses.

#### Generate Figures

With the appropriate directories created to save the output figures, the following bash scripts can be used to generate all figures presented in the main analyses at once. 

```bash
bash Code/Analysis/sbatch_figures.sh
```

Alternatively individual figures can be created by running the code found in each of the `Figures_*` directories. 

### Step 4: Supplementary Analysis with Superettes

For the supplementary analysis using the modified low-access indicator that includes superettes:

1. Navigate to `Code/Analysis_Supplementary_w_Superettes/`
2. Follow the same procedure as the main analysis (Steps 1-3)
3. The directory structure mirrors `Code/Analysis/` but uses the modified indicator

## Output Files

The scripts will generate:
- **Model Results**: Trained XGBoost models using the full data and models using bootstrapped samples. 
- **Analysis Results**: Cross-validation errors, post-treatment counterfactuals, treatment effects, model diagnostics, assessment of treatment effect heterogeneity
- **Tables**: `.csv` and `.tex` formatted tables (main and supplementary)
- **Figures**: Figures for main text and appendix. 

## Customization

### For Different Computing Environments
If not using SLURM, you can adjust the R scripts to run on your local machine with sufficient memory and processing capabilities. 
Also, you may need to update file paths as necessary. 

### Email and Account Settings
Before running the SBATCH scripts, update the following placeholders in all scripts:
- `useremail` → your email address
- `accountname-b` → your cluster account name

**Note:** Due to our use of proprietary data, the actual datasets cannot be provided with this code repository.

## Support and Issues

For questions about the methods or code structure, please contact xxxx.

## Citation


---
