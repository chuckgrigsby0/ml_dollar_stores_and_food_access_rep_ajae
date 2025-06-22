#!/bin/bash
#SBATCH --job-name=bootstrap_estimation
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cgrigsby0@gmail.com
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=1-499%53
#SBATCH --cpus-per-task=4 # 6 for Urban
#SBATCH --mem=21gb #21gb for Rural; 60gb for Urban
#SBATCH --qos=connerm-b
#SBATCH --time=20:00:00
#SBATCH --export=model_geography=Rural,model_dep_var=low_access # Rural/Urban, low_access/low_access_pers
#SBATCH --output=./bootstrap_output/rural_bootstrap_low_access/bootstrap_lhs_w_trends/rural_bootstrap_estimation_%A_%a.out
#SBATCH --error=./bootstrap_output/rural_bootstrap_low_access/bootstrap_lhs_w_trends/rural_bootstrap_estimation_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript models_imputation_xgboost_bootstrap_lhs_w_time_trends.R $SLURM_ARRAY_TASK_ID

date
