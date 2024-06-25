# Developments done during Polly Schmederer's ACCORD VS at DMI (June 2024)

Polly Schmederer (Geosphere), Carlos Peralta (DMI) and Fabrizio Baordo (DMI)

**Topics of VS**

Generalizing spatial verifications: improving R scripting; use of reticulate package to interface R with Python; testing 'pannelfication' tool

**Data used for spatial verification**

Observations:

DMI's radar precipitation product: Surface Quantitative Precipitation Estimation (SQPE) using both rain guage and radar data

EUMETSAT SEVIRI data (https://api.eumetsat.int/data/browse/collections): High Rate SEVIRI Level 1.5 Image Data - MSG - 0 degree (native), e.g. MSG3-SEVI-MSG15-0100-NA-20240102235743.693000000Z-NA.nat

NWP: 

Grib files output of the DEODE workflow running HARMONIE cy46h1 (total presipitation and FULL POS simulated radiances channels WV_062 & IR_108)

**Content of the repository**

**- installHarp**
  renv.lock
  ./renv/cellar/harpSpatial_0.0.1.9009.tar.gz
  ./renv/cellar/harpIO_0.2.2.tar.gz
  Note: pull request will be done for harpIO & harpSpatial, but for now the changes are saved here:
  harpIO:
     - changes to read HDF files
  harpSpatial:
     - fc_param_defs input to read_grid()
     - prm$basename input to read_grid() 
     - prm$basename to force IR and WV satellite channels as the same parameter
     - return fields from verify_spatial()
     - adding scores in verify_spatial() (like RMSE, corr coef, percentage FSS) 
     
**- scripts**
   
### Installation instructions

Development was done on ATOS (shared using accord group)

Refer to the [installation instructions](INSTALLATION.md) for details of how to install different libraries.

# R install on ATOS using info from repositoy

1) load the following modules: R/4.3.3   ecmwf-toolbox/2024.04.0.0   proj/9.3.1
2) cd where renv.lock is in the repository (cd installHarp)
7)  start R
8)  if you do not have renv: install.packages("renv")
9)  quit and restar R
10)  intall env: renv::restore()
11)  if you want to see the packages: renv::status() 
12)  

Additional dependencies when using reticulate to interface with python (more info in reading_functions.py):

- python metview for reading the grib file
- satpy for reading and regridding MSG data (you might use /perm/miag/venvs/satpy/bin/python3 'rx' permissions for accord group)

