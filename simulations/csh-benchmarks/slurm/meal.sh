#!/bin/bash
# Job name:
#SBATCH --job-name=csh-sim
#
# Quality of Service:
#SBATCH --qos=medium
#
# Processors (1 node = 28 cores):
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=40G
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=boileap2@gene.com
#
# Job output:
#SBATCH --output=slurm.out
#SBATCH --error=slurm.out
#
## Command(s) to run:
## Initialize work environment
cd ~/mmrm/simulations/csh-benchmarks || exit
module load R/prd
#
## Run the script
R CMD BATCH --no-save --no-restore \
  R/meal.R \
  logs/meal.Rout
