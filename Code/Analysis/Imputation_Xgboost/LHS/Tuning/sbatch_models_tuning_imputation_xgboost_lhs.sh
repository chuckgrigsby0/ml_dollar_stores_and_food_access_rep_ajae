#!/bin/bash
#SBATCH --job-name=tuning_lhs
#SBATCH --mail-type=ALL
#SBATCH --mail-user=useremail
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=1-50%48
#SBATCH --cpus-per-task=3
#SBATCH --mem=65gb # 30gb for Rural
#SBATCH --qos=useraccount-b
#SBATCH --time=20:00:00
#SBATCH --export=model_geography=Urban,model_dep_var=low_access # Rural/Urban
#SBATCH --output=./output/urban_tuning_lhs_%A_%a.out
#SBATCH --error=./output/urban_tuning_lhs_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript models_imputation_xgboost_tuning_lhs.R $SLURM_ARRAY_TASK_ID

date
