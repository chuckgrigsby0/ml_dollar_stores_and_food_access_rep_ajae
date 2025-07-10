#!/bin/bash
#SBATCH --job-name=models_imputation_rf
#SBATCH --mail-type=ALL
#SBATCH --mail-user=usermail
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=60gb  # 60 GB for Urban; 35 for Rural
#SBATCH --qos=useraccount-b
#SBATCH --time=20:00:00
#SBATCH --export=model_geography=Urban,model_dep_var=low_access # Rural/Urban
#SBATCH --output=./output/models_imputation_rf_%j.out
#SBATCH --error=./output/models_imputation_rf_%j.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript models_imputation_rf_training.R

date
