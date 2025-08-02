#!/bin/bash
#SBATCH --job-name=bootstrap_all_output
#SBATCH --mail-type=ALL
#SBATCH --mail-user=useremail
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=1-499%112
#SBATCH --cpus-per-task=1
#SBATCH --mem=20gb
#SBATCH --qos=useraccount-b
#SBATCH --time=01:00:00
#SBATCH --export=model_geography=Urban,model_dep_var=low_access # Rural/Urban
#SBATCH --output=./output/bootstrap_all_output_%A_%a.out
#SBATCH --error=./output/bootstrap_all_output_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

# Run R scripts for 499 bootstrap estiamtes. These results are used to compute bootstrap standard errors. 
# Note: To run the scripts below, one must first train models in 'Code/Analysis_Supplementary_w_Superettes/Imputation_Xgboost/Training' 
# and 'Code/Analysis_Supplementary_w_Superettes/Imputation_Xgboost/Bootstrap'

Rscript Figures_Actual_vs_Pred/analysis_model_bootstrap_errors_and_counterfactuals_array.R $SLURM_ARRAY_TASK_ID

Rscript Figures_Errors_and_Effects_on_Covars/analysis_model_bootstrap_effects_and_errors_on_covars_array.R $SLURM_ARRAY_TASK_ID

Rscript Figures_Effects_on_Grocery_and_Superette/analysis_model_bootstrap_effects_on_grocery_and_superettes_array.R $SLURM_ARRAY_TASK_ID

Rscript Figures_Effects_on_DS_Policies/analysis_model_bootstrap_effects_on_ds_policy_vars_array.R $SLURM_ARRAY_TASK_ID


date
