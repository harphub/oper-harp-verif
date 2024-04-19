#!/usr/bin/env bash
#SBATCH --error=spatial_verif.%j.err
#SBATCH --output=spatial_verif.%j.out
#SBATCH --job-name=spatial_verif
#SBATCH --qos=nf
#SBATCH --mem-per-cpu=16000
module load R
Rscript verify_radar_data_nea.R 
