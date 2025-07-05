#!/bin/bash
#SBATCH --job-name=figures
#SBATCH --mail-type=ALL   
#SBATCH --mail-user=username
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=15gb   
#SBATCH --time=00:60:00   
#SBATCH --output=./bootstrap_output/figures_%j.out 

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE Megabytes RAM"

echo "Creating figures of average CV errors and counterfactual predictions versus actual outcomes on relative time."
# Run R script
Rscript Figures_Actual_vs_Pred/analysis_plot_model_diagnostics_bootstrap_sourced.R

echo "Creating figures of ATT% across time since treatment."
# Run R script
Rscript Figures_ATT_Percent/analysis_plot_change_in_outcomes_sourced.R

echo "Creating figures of average CV errors and treatment effects, respectively, regressed on covariates."
# Run R script
Rscript Figures_Errors_and_Effects_on_Covars/analysis_plot_effects_and_errors_on_covars_sourced.R

echo "Creating figures of average CV errors across dollar store policy presence, baseline grocery stores and future dollar store entries, and geographic region."
# Run R script
Rscript Figures_Errors_on_Grocery_and_DS_Policy/analysis_plot_errors_on_ds_entry_x_grocery_sourced.R

echo "Creating figures of average treatment effects and dollar store entries and baseline grocery stores."
# Run R script
Rscript Figures_Effects_on_Grocery/analysis_plot_effects_on_ds_entry_x_covars_interactions_sourced.R

echo "Creating figures of effects across dollar store policy variables."
# Run R script
Rscript Figures_Effects_on_DS_Policies/analysis_plot_effects_and_ds_policy_vars_sourced.R



date
