#!/bin/bash
#SBATCH --job-name=bootstrap_cv_error_and_att
#SBATCH --mail-type=ALL
#SBATCH --mail-user=useremail
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=#1-499%200
#SBATCH --cpus-per-task=1
#SBATCH --mem=8gb #8gb for Urban
#SBATCH --time=01:00:00
#SBATCH --export=model_geography=Rural,model_dep_var=low_access # Rural/Urban
#SBATCH --output=./bootstrap_output/rural_bootstrap_low_access/cv_error_and_att/bootstrap_cv_error_and_att_%A_%a.out
#SBATCH --error=./bootstrap_output/rural_bootstrap_low_access/cv_error_and_att/bootstrap_cv_error_and_att_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript analysis_model_bootstrap_cv_error_and_att_array.R $SLURM_ARRAY_TASK_ID

date
