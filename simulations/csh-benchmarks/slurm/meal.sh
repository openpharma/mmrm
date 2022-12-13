#!/bin/bash
# Job name:
#SBATCH --job-name=csh-sim
#
# Quality of Service:
#SBATCH --qos=long
#
# Processors (1 node = 28 cores):
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
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
## Initialize work environment like
cd ~/mrmm/simulations/csh-benchmarks
module load R/tst
#
## Run the script
R CMD BATCH --no-save --no-restore \
  R/meal.R \
  logs/.Rout
