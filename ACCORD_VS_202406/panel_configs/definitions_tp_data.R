### DEFINITIONS for panel_tool_scripts/main.R ###

######################
###### PARAMETER #####
######################
 # lead_time <- 23

pcp_accum_period = "3h"  # "1h", "3h", "6h"

param		 = paste0("Accpcp", pcp_accum_period)

prm_units      	 = "mm"  # "kg m**-2"

#####################
### OBSERVATIONS ####
#####################

ob_file_path     = paste0(here(), "/ACCORD_VS_202406/sample_data/radar")

ob_file_template = switch(
			  pcp_accum_period,
		      	  "1h"	= "/{YYYY}{MM}{DD}{HH}00.kavrRAD.01.h5",
	    		  "3h"	= "/{YYYY}{MM}{DD}{HH}00.kavrRAD.03.h5",
			  )

ob_file_opts   <- list(data_path="/dataset1/data1/data",
			odim="FALSE",
			meta=TRUE,
			invert_data=FALSE
  			)

ob_accumulation  <- pcp_accum_period

ob_name          = "radar_composite"

ob_file_format   = "hdf5"

ob_interp_method = "closest"

verif_date       = as.POSIXct(as.character(init_time), format="%Y%m%d%H") + lead_time * 60 * 60

#####################
###### MODEL ########
#####################

fc_file_path 	 = paste0(here(), "/ACCORD_VS_202406/sample_data/deode")

fc_file_template <- switch(
                                  model,
                                  "DK2500m_atos" = paste0(init_time, "/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+00{LDT2}h00m00s.grb"),
                                  "DK2500m_hres" = paste0(init_time, "/harmonie_DK2500g_SP_HRES/surface_gc_300x300_2500m+00{LDT2}h00m00s.grb"),
                                  "DK500m_atos"  = paste0(init_time, "/harmonie_DK500g_SP_ATOSDT_00bd/surface_gc_1500x1500_500m+00{LDT2}h00m00s.grb"),
                                  "DK500m_hres"  = paste0(init_time, "/harmonie_DK500g_SP_HRES/surface_gc_1500x1500_500m+00{LDT2}h00m00s.grb"))


fc_param_defs     = modify_param_def(
			 	     "pcp",
			       	     decription = "Total precipitation",
				     grib = new_grib_param(
					  		   name = list(r = "tirf",
						  			 g = "tgrp",
							  		 s = "tsnowp"
								  	 ),
							   ),
				     func = function(r, g, s) r + g + s,
				     accum = readr::parse_number(pcp_accum_period)
				     ) 

fc_file_format    = "grib"

fc_file_opts      = list()

fc_interp_method  = "closest" 

fc_accumulation   = NULL

#####################
#### ADDITIONAL #####
#####################

source(paste0(here(),"/ACCORD_VS_202406/scripts/reading_functions.R"))

deode_grb_2500    <- read_param_with_grbmessg(paste0(here(), "/ACCORD_VS_202406/sample_data/deode/2024010200/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+0024h00m00s.grb"), 1)

deode_grb_500     <- read_param_with_grbmessg(paste0(here(), "/ACCORD_VS_202406/sample_data/deode/2024010200/harmonie_DK500g_SP_ATOSDT_00bd/surface_gc_1500x1500_500m+0024h00m00s.grb"), 1)

verif_domain_2500 <- get_domain(deode_grb_2500)
verif_domain_500  <- get_domain(deode_grb_500)

domain_500_DK_mainland  <- geo_subgrid(verif_domain_500, 55*5, 190*5,80*5, 245*5)
domain_2500_DK_mainland <- geo_subgrid(verif_domain_2500, 55, 190,80, 245)

verif_domain = domain_500_DK_mainland
 
######################
####### BASIC ########
######################

# thresholds     = c(1, 3, 5, 7, 10, 15, 20, 25, 30)
thresholds     = c(0.1, 1, 3, 5, 7, 10, 15, 20, 25, 30)
percentiles    = c(25, 50, 75, 90, 95)

# window_sizes   = c(0, 1, 2, 4, 8, 12, 20)  # default
window_sizes   = c(10, 20, 30, 40, 60, 80, 100, 120, 140, 160, 180, 200)  # default

scores         = c("mae", "bias", "rmse", "Rpearson", "FSS", "FSSp")

return_data    = TRUE
return_fields  = TRUE

sqlite_path    = NULL
sqlite_file    = NULL


