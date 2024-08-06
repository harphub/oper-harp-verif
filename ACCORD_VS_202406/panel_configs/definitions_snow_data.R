### DEFINITIONS for panel_tool_scripts/main.R ###

######################
###### PARAMETER #####
######################

param            = "bin_snow" # "WV_062"

prm_units        = "none"


#####################
### OBSERVATIONS ####
#####################
require(reticulate)

source(paste0(here(),"/ACCORD_VS_202406/scripts/reading_functions.R"))
python_function = paste0(here(),"/ACCORD_VS_202406/scripts/reading_functions.py")
python_version = "/usr/local/apps/python3/3.11.8-01/bin/python3"

ob_file_path     = paste0(here(), "/ACCORD_VS_202406/sample_data/snow_data")

ob_file_template = "daily-avhrr-sce-nhl_ease-50_{YYYY}{MM}{DD}1200_resampled.nc"

ob_file_opts   <- list(
                            python_function = python_function,
                            python_version = python_version,
                            invert_data = TRUE,
                            is_obs = TRUE
                            )

ob_name          = "bin_snow"

ob_accumulation   = NULL

ob_file_format   = "nc_reticulate"

ob_interp_method = "closest"

verif_date       = as.POSIXct(as.character(init_time), format="%Y%m%d%H") + lead_time * 60 * 60

#####################
###### MODEL ########
#####################

fc_file_path 	 = paste0(here(), "/ACCORD_VS_202406/sample_data/snow_data")

fc_file_template =  "binary_snow_{YYYY}{MM}{DD}_reg.grib2"

fc_param_defs  = NULL

fc_file_format    = "grib"

fc_file_opts      = grib_opts(param_find = list(bin_snow = use_grib_shortName("fscov")))

fc_interp_method  = "closest" 

fc_accumulation   = NULL

#####################
#### ADDITIONAL #####
#####################

#crude way to determine domain, change to first data file available
#sample nc file, only used to check if reading function works below
ncFile <- paste0(here(),"/ACCORD_VS_202406/sample_data/snow_data/daily-avhrr-sce-nhl_ease-50_201505011200_resampled.nc")

ob_reticulate_opts <- list(
                            python_function = python_function,
                            python_version = python_version,
                            invert_data = TRUE,
                            is_obs = TRUE
                            )

fc_file_opts <- grib_opts(param_find = list(bin_snow = use_grib_shortName("fscov")))

thresholds <- c(0,1)

