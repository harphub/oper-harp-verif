# Example verification for radar data from DMI and the NEA model

library(here)
library(harpSpatial)
library(harpIO)


#library(starsExtra)
library(meteogrid)
library(dplyr)


format_opts <- netcdf_opts(
  proj4_var="Lambert_Azimuthal_Grid",
  x_dim="xc",
  y_dim="yc",
  lat_var = "lat", #NULL,
  lon_var = "lon", #NULL,
  y_rev = TRUE,
  force_param_name = "TRUE"
)



fcst_opts <- grib_opts(param_find = list("AccPcp3h"=use_grib_key("shortName","tp"),
                                         "lead_time"=use_grib_key("shortName","step")))
obs_opts <- netcdf_opts(proj4_var = "projection_lcc", lon_var = NULL, lat_var = NULL)




start_date <- 202310010000
end_date <- 202310310000
thresholds              <- c(1,5,10,15,20,25)
obs_file_path <- "/ec/res4/scratch/nhd/CERISE/sample_data/radar/3h_accum/"
#obs_file_path <- "/home/tenantadmin/R/spatial_verif/radar/3h_accum/"
obs_template <- "{YYYY}{MM}{DD}{HH}00.kavrRAD.03.h5" #for 3h

sqlite_path <- here()
sqlite_file <- paste0(paste("harp_spatial_scores_3h",start_date,end_date,sep="_"),".sqlite")

fc_file_path            <- "/ec/res4/scratch/nhd/CERISE/sample_data/NEA/"
#fc_file_path            <- "/home/tenantadmin/R/spatial_verif/NEA"
fc_file_template <- "NEA_{YYYY}{MM}{DD}00_TP.grib"
fc_file_format          <- "grib"
fc_interp_method        <- NULL


#example_file <- "/home/tenantadmin/R/spatial_verif/radar/202310150000.kavrRAD.01.h5"
example_file <- "/ec/res4/scratch/nhd/CERISE/sample_data/radar/3h_accum/202310150000.kavrRAD.03.h5"
#param <- "pcp"
param <- "Pcp"
precip <- harpIO::read_grid(example_file,param,hdf5_opts=hdf5_opts(data_path="/pcp/data1/data", odim=FALSE,meta=TRUE))
#precip <- read_grid( "NEA_2023100100_TP_subset.grib", "Pcp") 
dom <- as.geodomain(precip)


fcdate_start <- format(as.POSIXct(as.character(start_date), format="%Y%m%d%H%M")) # - leadtime * 60 * 60, "%Y%m%d%H%M")
fcdate_end <- format(as.POSIXct(as.character(end_date), format="%Y%m%d%H%M")) # - leadtime * 60 * 60, "%Y%m%d%H%M")


verif3h <- verify_spatial(
  start_date        = start_date,
  end_date          = end_date,
  det_model         = "NEA_det",
  parameter         = "AccPcp3h",
  lead_time         = seq(1, 48),
  fc_file_path      = fc_file_path,
  fc_file_template  = fc_file_template,
  fc_options = fcst_opts,
  fc_file_format = "grib",
  ob_file_path      = obs_file_path,
  ob_file_template  = obs_template,
  ob_accumulation   = "3h",
  verif_domain      = dom,
  return_data = TRUE,
  sqlite_path = sqlite_path,
  sqlite_file = sqlite_file,
  thresholds = thresholds
)

