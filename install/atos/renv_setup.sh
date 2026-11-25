#!/bin/bash

INSTALL_CRAN=false
INSTALL_HARP=false
FORCE_HARP=false
RENV_PATH=""

usage() {
    cat <<EOF
Usage: $0 -r PATH [OPTIONS]

Required:
  -r, --renv PATH     Path to renv
Options:
  -c, --cran          Install CRAN packages
  -H, --harp          Install harp 
  -f, --force         When installing harp, force the installation even if existing versions are found. Only relevant for -H/--harp
  -h, --help          Show this help message
EOF
}

while [ $# -gt 0 ]; do
    case "$1" in
        -r|--renv)
            if [ -z "$2" ]; then
                echo "Error: Need to specify renv path"
                usage
                exit 1
            fi
            RENV_PATH="$2"
            shift 2
            ;;
        -c|--cran)
            INSTALL_CRAN=true
            shift
            ;;
        -H|--harp)
            INSTALL_HARP=true
            shift
            ;;
        -f|--force)
            FORCE_HARP=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done
if [ -z "$RENV_PATH" ]; then
    echo "Error: Need to specify renv path"
    usage
    exit 1
fi 

cd "$(dirname "$0")" || exit 1
export RWD="$PWD"

if [ ! -d "$RENV_PATH" ]; then
    echo "Creating $RENV_PATH"
    mkdir -p "$RENV_PATH"
fi
cd "$RENV_PATH" || exit 1

INIT_RENV=false
if [ ! -d renv ];  then
    INIT_RENV=true
    echo "Creating renv in $PWD"
else
    echo "renv already exists in $PWD - installing into this"
fi
export INIT_RENV INSTALL_CRAN INSTALL_HARP FORCE_HARP
module purge
module load prgenv/gnu
module load R/4.4.3
module load proj/9.5.1
module load netcdf4/4.9.3
module load ecmwf-toolbox/2025.10.1.0
module load gdal/3.10.2
module load geos/3.13.1
module load hdf5/1.14.6

start=$(date +%s)
"$RWD"/renv_setup.R
end=$(date +%s)
elapsed=$((end - start))

echo "Finished the renv install in $PWD in $(awk "BEGIN {printf \"%.2f\", $elapsed/60}") minutes"
