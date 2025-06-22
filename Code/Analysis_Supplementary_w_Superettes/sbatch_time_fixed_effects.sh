#!/bin/bash
#SBATCH --job-name=time_fes
#SBATCH --mail-type=ALL   
#SBATCH --mail-user=charlesgrigsby@ufl.edu   
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=50gb   
#SBATCH --time=10:00:00   
#SBATCH --output=time_fes_%j.out   

#Record the time and compute node the job ran on
date; hostname; pwd

#Use modules to load the environment for R
module load R

echo "Running script on $SLURM_CPUS_ON_NODE CPU cores"

#Run R script 
Rscript data_preparation_feat_eng_time_fes.R

date
