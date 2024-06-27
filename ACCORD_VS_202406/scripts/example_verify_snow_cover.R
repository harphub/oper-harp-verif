# Example reading netcdf data using reticulate to verify snow
# cover from the Harmonie model (using CARRA West data) and
# snow cover from MET Norway.
# The snow cover data has been previouls pre-processed and projected on the same
# regular lat lon model grid of Harmonie
# The data uses a binary variable called bin_snow, that is set to 1
# where there is snow and to 0 where there isn't
require(harp)
require(reticulate)
require(meteogrid)
require(here)
#load the python functions
#Note they require
# ml ecmwf-toolbox
# and a particualr version of python (details in this repo)
python_function <- "reading_functions.py"
python_version <- "/perm/miag/venvs/satpy/bin/python3"
#sample nc file, only used to check if reading function works below
ncFile <- paste0(here(),"/../sample_data/snow_data/daily-avhrr-sce-nhl_ease-50_201505011200_resampled.nc")
#options for the function below
lead_time <- 0
param <- "bin_snow"
ob_reticulate_opts <- list(
                            python_function = python_function,
                            python_version = python_version,
                            invert_data = TRUE,
                            is_obs = TRUE
                            )
fc_file_opts <- grib_opts(param_find = list(bin_snow = use_grib_shortName("fscov")))

#here I source the function below
source("read_DataUsingReticulate.R")
#this is a test
check_data <- read_nc_reticulate(file_name = ncFile,
                    parameter = "bin_snow",
                    format_opts = ob_reticulate_opts
                    )
init_time <- 2015050100
ob_template <- "daily-avhrr-sce-nhl_ease-50_{YYYY}{MM}{DD}1200_resampled.nc"
fc_template <- "binary_snow_{YYYY}{MM}{DD}_reg.grib2"
fc_file_path <- paste0(here(),"/../sample_data/snow_data")
ob_file_path <- paste0(here(),"/../sample_data/snow_data")
thresholds <- c(0,1)
verif <- verify_spatial(
  dttm              = init_time,
  fcst_model        = "cerise",
  parameter         = "bin_snow",
  lead_time         = lead_time,
  #lt_unit           = "h",
  fc_file_path      = fc_file_path,
  fc_file_template  = fc_template,
  #fc_accumulation   = "0h",
  fc_file_format    = "grib",
  fc_file_opts      = fc_file_opts,
  fc_param_defs     = NULL,
  fc_interp_method  = NULL,
  ob_file_path      = ob_file_path,
  ob_file_template  = ob_template,
  ob_file_format    = "nc_reticulate",
  ob_file_opts      = ob_reticulate_opts,
  #ob_accumulation   = "0h",
  ob_interp_method  = NULL,
  verif_domain      = NULL, # verif_domain_500,
  return_data       = TRUE,
  sqlite_path       = ".", #NULL, # sqlite_path,
  sqlite_file       = "test_snow_verif_harp.sqlite", #NULL, # sqlite_file,
  thresholds        = thresholds,
  #scores            = "FSS",
  return_fields     = TRUE
)
#call the python unfciont
