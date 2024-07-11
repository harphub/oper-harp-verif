# Developments done during Polly Schmederer's ACCORD VS at DMI (June 2024)

Polly Schmederer (GeoSphere), Carlos Peralta (DMI) and Fabrizio Baordo (DMI)

**Topics of VS**

Generalizing spatial verifications: improving R scripting; use of reticulate package to interface R with Python; testing 'panelification' tool

**Data used for spatial verification**

Observations:
  
  DMI's radar precipitation product: Surface Quantitative Precipitation Estimation (SQPE) using both rain guage and radar data

  EUMETSAT SEVIRI data (https://api.eumetsat.int/data/browse/collections): High Rate SEVIRI Level 1.5 Image Data - MSG - 0 degree (native), e.g. MSG3-SEVI-MSG15-0100-NA-20240102235743.693000000Z-NA.nat

NWP:

  Grib files output of the DEODE workflow running HARMONIE cy46h1 (total presipitation and FULL POS simulated radiances channels WV_062 & IR_108)

**Content of the repository**

* reading_functions
  
  **using reticulate**
  * nat files (seviri data in their native format)
  * nc  files (regridded snow data)
    
  **using harp**
  * grib files (using grib message for simulated satellite channels defined in file as "unknown";
                adding rain, snow, graupel to total precipitation)

* examples

  **read data using reticulate**
  
  -> example_read_DataUsingReticulate.R 
  
  **spatial verification**
  
  examples on how verify_spatial can be used used:
  * total precipitation (example_verify_tp_deode.R)
  * satellite channels  (example_sat_deode.R)
  * snow cover          (example_snow_cover.R)
    
* panelification scripts
  * panel_main.R
    
    script that
    * uses panel_configs/
      
      (for definitions on reading model and observations, score definitions, plottings schemes)
    * calls panel_ranking_functions.R,
            panel_utils.R and
            panel_plotting_functions.R
    * Plots are saved in PLOTS/
   
     
### Installation instructions

Development was done on ATOS (shared using accord group)

Refer to the [installation instructions](INSTALLATION.md) for details of how to install different libraries.
