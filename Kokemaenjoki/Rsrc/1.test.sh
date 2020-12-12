#!/bin/bash -l
# created: Jun 22, 2018 3:37 PM
# author: minunnof
#SBATCH --job-name=test
#SBATCH --partition=small
#SBATCH --account=project_2000994
#SBATCH --output=sbaOut/output_%j.txt
#SBATCH --error=sbaOut/errors_%j.txt
#SBATCH --time=05:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=30000
#SBATCH --mail-type=END
#SBATCH --mail-user=francesco.minunno@helsinki.fi

# set the number of threads based on --cpus-per-task
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
module load r-env-singularity/4.0.2
# Clean up .Renviron file in home directory
if test -f ~/.Renviron; then
    sed -i '/TMPDIR/d' ~/.Renviron
    sed -i '/OMP_NUM_THREADS/d' ~/.Renviron
fi
# Specify a temp folder path
echo "TMPDIR=/scratch/project_2000994" >> ~/.Renviron
# Run the R script
srun singularity_wrapper exec Rscript --no-save runModel_koke1.r

