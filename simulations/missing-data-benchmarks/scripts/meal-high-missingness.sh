#!/bin/bash
# Job name:
#SBATCH --job-name=mmrm-high-missingness-sim
#
# Quality of Service:
#SBATCH --qos=long
#
# Processors (1 node = 28 cores):
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=200G
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
cd ~/mmrm/simulations/missing-data-benchmarks || exit
module load R/prd
#
## Run the script
R CMD BATCH --no-save --no-restore \
  R/meals/meal-high-missingness.R \
  logs/meal-high-missingness.Rout
