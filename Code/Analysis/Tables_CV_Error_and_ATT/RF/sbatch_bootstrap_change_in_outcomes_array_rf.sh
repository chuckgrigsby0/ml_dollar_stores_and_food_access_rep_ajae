#!/bin/bash
#SBATCH --job-name=bootstrap_change_in_outcomes
#SBATCH --mail-type=ALL
#SBATCH --mail-user=useremail
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=0-499%200
#SBATCH --cpus-per-task=1
#SBATCH --mem=8gb
#SBATCH --qos=useraccount
#SBATCH --time=01:00:00
#SBATCH --export=model_geography=Urban,model_dep_var=low_access # Rural/Urban
#SBATCH --output=./bootstrap_output/rf/urban/change_in_outcomes/bootstrap_change_in_outcomes_rf_%A_%a.out
#SBATCH --error=./bootstrap_output/rf/urban/change_in_outcomes/bootstrap_change_in_outcomes_rf_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript analysis_model_bootstrap_change_in_outcomes_array_rf.R $SLURM_ARRAY_TASK_ID

date
