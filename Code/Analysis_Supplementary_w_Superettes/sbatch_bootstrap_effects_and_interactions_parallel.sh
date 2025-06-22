#!/bin/bash
#SBATCH --job-name=bootstrap_estimation
#SBATCH --mail-type=ALL   
#SBATCH --mail-user=charlesgrigsby@ufl.edu   
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=3
#SBATCH --mem=30gb   
#SBATCH --time=01:00:00   
#SBATCH --output=./bootstrap_output/bootstrap_rural_effects_on_ds_x_covars%j.out 

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE Megabytes RAM"

#Run R script 
Rscript analysis_model_bootstrap_effects_on_ds_entry_x_covars_parallel.R 

date
