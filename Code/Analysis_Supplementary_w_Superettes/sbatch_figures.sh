#!/bin/bash
#SBATCH --job-name=figures
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cgrigsby0@gmail.com
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=20gb
#SBATCH --time=00:60:00
#SBATCH --output=./bootstrap_output/figures_%j.out

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE Megabytes RAM"

echo "Creating figures of effects and CV errors regressed on covariates."
#Run R script
Rscript analysis_plot_effects_and_errors_on_covars_sourced.R

echo "Creating figures of CV errors and predictions versus actual outcomes on relative time."
#Run R script
Rscript analysis_plot_model_diagnostics_bootstrap_sourced.R

echo "Creating figures of effects on interaction effects between dollar store entries and binned covariates."
#Run R script
Rscript analysis_plot_effects_on_ds_entry_x_covars_interactions_sourced.R

echo "Creating figures of changes in outcomes on relative time."
#Run R script
Rscript analysis_plot_change_in_outcomes_sourced.R

echo "Creating figures of effects on dollar store policy variables."
#Run R script
Rscript analysis_plot_effects_and_ds_policy_vars_sourced.R

echo "Creating figures of effects on dollar store entry-x-grocery store-superette counts pre-entry."
#Run R script
Rscript analysis_plot_effects_on_grocery_and_superettes_sourced.R


date
