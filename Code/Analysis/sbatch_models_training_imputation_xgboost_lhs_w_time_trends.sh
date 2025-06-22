#!/bin/bash
#SBATCH --job-name=models_imputation_xgboost
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cgrigsby0@gmail.com
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=6
#SBATCH --mem=70gb  # Used 60/75 gb for urban models; Used 30 gb for rural models.; next line: #SBATCH --qos=connerm-b
#SBATCH --time=20:00:00
#SBATCH --export=model_geography=Urban,model_dep_var=low_access # Rural/Urban, low_access/low_access_pers
#SBATCH --output=./tuning_and_training_output/models_imputation_xgboost_urban_la_lhs_w_trends_%j.out
#SBATCH --error=./tuning_and_training_output/models_imputation_xgboost_urban_la_lhs_w_trends_%j.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript models_imputation_xgboost_training_lhs_w_time_trends.R

date