#!/bin/bash
#SBATCH --job-name=models_imputation_xgboost
#SBATCH --mail-type=ALL
#SBATCH --mail-user=useremail
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=6
#SBATCH --mem=35gb  # Used 100 gb for urban models; Used 30 gb for rural models.
#SBATCH --qos=useraccount
#SBATCH --time=20:00:00
#SBATCH --export=model_geography=Rural,model_dep_var=low_access 
#SBATCH --output=./tuning_and_training_output/models_imputation_xgboost_rural_la_%j.out
#SBATCH --error=./tuning_and_training_output/models_imputation_xgboost_rural_la_%j.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript models_imputation_xgboost_training.R

date
