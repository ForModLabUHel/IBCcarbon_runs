#!/bin/bash -l
# created: Jun 22, 2018 3:37 PM
# author: minunnof
#SBATCH --job-name=anRes
#SBATCH --partition=small
#SBATCH --account=project_2000994
#SBATCH --output=sbaOut/output_%j.txt
#SBATCH --error=sbaOut/errors_%j.txt
#SBATCH --time=03:00:00
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=40000
#SBATCH --mail-type=END
#SBATCH --mail-user=francesco.minunno@helsinki.fi

module load r-env
echo "TMPDIR=/scratch/project_2000994" > .Renviron
srun Rscript --no-save anRes.r
