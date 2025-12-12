#!/usr/bin/env bash
#SBATCH --mem-per-cpu=16GB
#SBATCH --time=48:00:00

ml purge
ml apptainer
[ ! -d $PERM/apptainer_build/cache ] && mkdir $PERM/apptainer_build/cache
[ ! -d $PERM/apptainer_build/tmpdir ] && mkdir $PERM/apptainer_build/tmpdir

export APPTAINER_CACHEDIR=$PERM/apptainer_build/cache
export APPTAINER_TMPDIR=$PERM/apptainer_build/tmpdir
apptainer pull docker://carlos9917/harp:0.3
