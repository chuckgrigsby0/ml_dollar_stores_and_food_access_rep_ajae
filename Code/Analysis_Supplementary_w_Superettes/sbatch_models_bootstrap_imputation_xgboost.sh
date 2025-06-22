#!/bin/bash
#SBATCH --job-name=bootstrap_estimation
#SBATCH --mail-type=ALL
#SBATCH --mail-user=charlesgrigsby@ufl.edu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=1-499%50
#SBATCH --cpus-per-task=3
#SBATCH --mem=54gb #21gb in rural models; 54gb in urban models
#SBATCH --qos=connerm-b
#SBATCH --time=20:00:00
#SBATCH --export=model_geography=Urban,model_dep_var=low_access # Rural/Urban, low_access/low_access_pers
#SBATCH --output=./bootstrap_output/urban_bootstrap_low_access/urban_bootstrap_estimation_%A_%a.out
#SBATCH --error=./bootstrap_output/urban_bootstrap_low_access/urban_bootstrap_estimation_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript models_imputation_xgboost_bootstrap.R $SLURM_ARRAY_TASK_ID

date
