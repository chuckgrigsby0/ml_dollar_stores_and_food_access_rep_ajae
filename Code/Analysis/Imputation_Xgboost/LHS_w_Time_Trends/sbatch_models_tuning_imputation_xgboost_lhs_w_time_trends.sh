#!/bin/bash
#SBATCH --job-name=tuning_lhs
#SBATCH --mail-type=ALL
#SBATCH --mail-user=useremail
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=1-50%2
#SBATCH --cpus-per-task=3
#SBATCH --mem=65gb #65gb for Urban; 21gb for Rural
#SBATCH --qos=useraccount-b
#SBATCH --time=48:00:00
#SBATCH --export=model_geography=Urban,model_dep_var=low_access # Rural/Urban, low_access/low_access_pers
#SBATCH --output=./tuning_and_training_output/tuning_lhs_w_time_trends/urban/urban_tuning_lhs_%A_%a.out
#SBATCH --error=./tuning_and_training_output/tuning_lhs_w_time_trends/urban/urban_tuning_lhs_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript models_imputation_xgboost_tuning_lhs_w_time_trends.R $SLURM_ARRAY_TASK_ID

date
