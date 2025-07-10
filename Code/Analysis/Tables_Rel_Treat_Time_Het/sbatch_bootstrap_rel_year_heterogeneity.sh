#!/bin/bash
#SBATCH --job-name=bootstrap_rel_year_heterogeneity
#SBATCH --mail-type=ALL
#SBATCH --mail-user=useremail
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=1-499%200
#SBATCH --cpus-per-task=1
#SBATCH --mem=20gb
#SBATCH --qos=useraccount-b
#SBATCH --time=01:00:00
#SBATCH --export=model_geography=Rural,model_dep_var=low_access # Rural/Urban
#SBATCH --output=./output/bootstrap_rel_year_heterogeneity_%A_%a.out
#SBATCH --error=./output/bootstrap_rel_year_heterogeneity_%A_%a.error

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R/4.2

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"
echo "This job requested $SLURM_MEM_PER_NODE MB RAM"

#Run R script 
Rscript analysis_section_5_1_heterogeneity_by_rel_time_array.R

date
