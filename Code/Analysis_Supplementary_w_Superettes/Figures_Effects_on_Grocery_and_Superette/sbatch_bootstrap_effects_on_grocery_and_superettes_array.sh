#!/bin/bash
#SBATCH --job-name=bootstrap_effects_on_grocery_and_superettes
#SBATCH --mail-type=ALL
#SBATCH --mail-user=useremail
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=1-499%112
#SBATCH --cpus-per-task=1
#SBATCH --mem=50gb
#SBATCH --qos=useraccount-b
#SBATCH --time=01:00:00
#SBATCH --export=model_geography=Urban,model_dep_var=low_access # Rural/Urban
#SBATCH --output=./output/bootstrap_effects_on_grocery_and_superettes_%A_%a.out
#SBATCH --error=./output/bootstrap_effects_on_grocery_and_superettes_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript analysis_model_bootstrap_effects_on_grocery_and_superettes_array.R $SLURM_ARRAY_TASK_ID

date
