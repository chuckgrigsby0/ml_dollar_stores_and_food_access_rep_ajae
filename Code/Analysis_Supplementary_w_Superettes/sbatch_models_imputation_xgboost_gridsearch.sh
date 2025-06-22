#!/bin/bash
#SBATCH --job-name=models_imputation_xgboost
#SBATCH --mail-type=ALL   
#SBATCH --mail-user=charlesgrigsby@ufl.edu   
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=96gb   
#SBATCH --time=10:00:00   
#SBATCH --output=models_imputation_xgboost_%j.out   

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"

#Run R script 
Rscript models_imputation_xgboost_gridsearch.R

date
