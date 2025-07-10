#!/bin/bash
#SBATCH --job-name=figures
#SBATCH --mail-type=ALL
#SBATCH --mail-user=useremail
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=20gb
#SBATCH --time=00:60:00
#SBATCH --output=./output/figures_%j.out

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE Megabytes RAM"


echo "Creating figures of CV errors and counterfactual predictions versus actual outcomes on relative time."
#Run R script
Rscript Figures_Actual_vs_Pred/analysis_plot_model_diagnostics_bootstrap_sourced.R

echo "Creating figures of effects on dollar store entry-x-grocery store-superette counts pre-entry."
#Run R script
Rscript Figures_Effects_on_Grocery_and_Superettes/analysis_plot_effects_on_grocery_and_superettes_sourced.R

echo "Creating figures of treatment effects and CV errors regressed on covariates (binned and standardized)."
#Run R script
Rscript Figures_Errors_and_Effects_on_Covars/analysis_plot_effects_and_errors_on_covars_sourced.R

echo "Creating figures of treatment effects on dollar store policy variables."
#Run R script
Rscript Figures_Effects_on_DS_Policies/analysis_plot_effects_and_ds_policy_vars_sourced.R



date
