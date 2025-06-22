#!/bin/bash
#SBATCH --job-name=ds_policy_vars_on_covars_lasso
#SBATCH --mail-type=ALL   
#SBATCH --mail-user=charlesgrigsby@ufl.edu   
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --qos=connerm-b
#SBATCH --mem=50gb   
#SBATCH --time=05:00:00   
#SBATCH --export=model_geography=Urban,model_dep_var=low_access # Rural/Urban, low_access/low_access_pers
#SBATCH --output=./bootstrap_output/ds_policy_vars_on_covars_lasso%j.out 

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE Megabytes RAM"

#Run R script 
Rscript analysis_ds_policy_vars_on_covars_lasso_reg.R 

date
