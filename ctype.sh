#!/bin/bash
#SBATCH -J 100_cvs
#SBATCH --account=def-ubcxzh
#SBATCH --nodes=3
#SBATCH --cpus-per-task=32 
#SBATCH --mem-per-cpu=12G
#SBATCH --time=0-23:58

module load StdEnv/2020
module load nixpkgs/16.09
module load gcc/9.3.0
module load r-bundle-bioconductor/3.12
module load gcc/7.3.0
module load r/4.0.2
module load openmpi/3.1.2

export R_LIBS=~/R/library/4.0
Rscript 1_main_script.R
