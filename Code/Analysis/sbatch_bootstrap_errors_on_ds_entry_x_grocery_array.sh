#!/bin/bash
#SBATCH --job-name=bootstrap_errors_and_grocery_entry_pol_reg
#SBATCH --mail-type=ALL
#SBATCH --mail-user=charlesgrigsby@ufl.edu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=1-499%200
#SBATCH --cpus-per-task=1
#SBATCH --mem=20gb
#SBATCH --qos=connerm-b
#SBATCH --time=01:00:00
#SBATCH --export=model_geography=Rural,model_dep_var=low_access # Rural/Urban, low_access/low_access_pers
#SBATCH --output=./bootstrap_output/rural_bootstrap_low_access/bootstrap_errors_and_grocery_entry_pol_reg_%A_%a.out
#SBATCH --error=./bootstrap_output/rural_bootstrap_low_access/bootstrap_errors_and_grocery_entry_pol_reg_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript analysis_model_bootstrap_errors_on_ds_entry_x_grocery_array.R $SLURM_ARRAY_TASK_ID

date
