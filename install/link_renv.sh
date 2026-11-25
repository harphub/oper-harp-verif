#!/bin/bash

if [ "$#" -ne 2 ]; then
        echo "Usage: $0 RENVDIR TRGDIR"
        exit 1
fi

RENVDIR=$1
TRGDIR=$2
if [ ! -d "${RENVDIR}" ] || [ ! -d "${TRGDIR}" ]; then
        echo "Did not find ${RENVDIR} and/or ${TRGDIR}"
        exit 1
fi

cd "${TRGDIR}" || exit 1
# If a renv directory exists here, then abort.
# If it is just a soft link, then continue
if [ -d renv ]; then
        if [ -L renv ]; then
                echo "The existing renv soft link is:"
                ls -l renv*
                echo "This will be overwritten"
        else
                echo "renv exists in $PWD and is NOT a soft link!"
                echo "Exiting as we should not overwrite this"
                exit 1
        fi
fi
if [ -d "${RENVDIR}"/renv ] && [ -f "${RENVDIR}/renv.lock" ]; then
        echo "Linking ${RENVDIR}/renv to $PWD" 
        ln -sfn "${RENVDIR}"/renv .
        ln -sfn "${RENVDIR}"/renv.lock .
        if [ -f .Rprofile ]; then
                echo "The existing .Rprofile will be backed up!"
                cp .Rprofile .Rprofile.bak
        fi
        echo 'source("renv/activate.R")' > .Rprofile
        ls -l renv*
else
        echo "Did not find ${RENVDIR}/renv"
fi

echo "Done!"