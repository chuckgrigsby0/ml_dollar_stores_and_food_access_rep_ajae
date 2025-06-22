#!/bin/bash
#SBATCH --job-name=bootstrap_err_and_entry_x_grocery
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cgrigsby0@gmail.com
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=0-499%200
#SBATCH --cpus-per-task=1
#SBATCH --mem=8gb
#SBATCH --qos=connerm-b
#SBATCH --time=01:00:00
#SBATCH --export=model_geography=Rural,model_dep_var=low_access # Rural/Urban, low_access/low_access_pers
#SBATCH --output=./hpg_output/rural/err_and_att_by_entry_x_grocery_%A_%a.out
#SBATCH --error=./hpg_output/rural/err_and_att_by_entry_x_grocery_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script
Rscript bootstrap_err_and_att_by_entry_and_grocery_array.R $SLURM_ARRAY_TASK_ID

date
