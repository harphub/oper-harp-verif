### DEFINITIONS for panel_tool_scripts/main.R ###

######################################################
### DEFINE MODELS, DATE, LEAD_TIMES
######################################################

# init_time  <- 2024010200

# models <- c("DK2500m_atos", "DK2500m_hres", "DK500m_atos", "DK500m_hres")

# lead_time  <- 23


######################
###### PARAMETER #####
######################

param            = "IR_108" # "WV_062"

prm_units      	 = "K"

#####################
### OBSERVATIONS ####
#####################

### special information due to own reading function ###

require(reticulate)

source(paste0(here(),"/ACCORD_VS_202406/scripts/reading_functions.R"))

python_function = paste0(here(),"/ACCORD_VS_202406/scripts/reading_functions.py")  
# this is to use satpy, you can point to your python env if you have satpy installed 
python_version = "/perm/miag/venvs/satpy/bin/python3"

msgFile <- paste0(here(), "/ACCORD_VS_202406/sample_data/seviri/MSG3-SEVI-MSG15-0100-NA-20240102225743.579000000Z-NA.nat")

grb_file_path   <- paste0(here(), "/ACCORD_VS_202406/sample_data/deode")

grb_file        <- switch(
                                  model,
                                  "DK2500m_atos" = paste0(init_time, "/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+00{LDT2}h00m00s.grb"),
                                  "DK2500m_hres" = paste0(init_time, "/harmonie_DK2500g_SP_HRES/surface_gc_300x300_2500m+00{LDT2}h00m00s.grb"),
                                  "DK500m_atos"  = paste0(init_time, "/harmonie_DK500g_SP_ATOSDT_00bd/surface_gc_1500x1500_500m+00{LDT2}h00m00s.grb"),
                                  "DK500m_hres"  = paste0(init_time, "/harmonie_DK500g_SP_HRES/surface_gc_1500x1500_500m+00{LDT2}h00m00s.grb"))


file_4regridding_grb <- generate_filenames(file_path = grb_file_path,
                   file_date = 2024010223,
                   file_template = grb_file,
                   lead_time = lead_time
                   )

areaId <- switch(model,
                 "DK2500m_hres" = c(53, 6, 59, 17, 2.5, 'laea'),
                 "DK2500m_atos" = c(53, 6, 59, 17, 2.5, 'laea'),
                 "DK500m_hres"  = c(53, 6, 59, 17, 0.5, 'laea'),
                 "DK500m_atos"  = c(53, 6, 59, 17, 0.5, 'laea')
                 )

# We use grib message to extract radiances from grib files (shortName is unknown)
messnum <- switch(param,
                  "WV_062" = 4,
                  "IR_108" = 5)

##### now standard #####

ob_file_path     = ""

ob_file_template = msgFile 

ob_file_opts <- list(
                            python_function = python_function,
                            python_version = python_version,
                            file_4regridding_grb = file_4regridding_grb,
                            grib_message = messnum,
                            strAreaId = areaId,
                            invert_data = TRUE,
                            is_obs = TRUE,
                            origin = "Meteosat-10"
                            )


ob_name          = "Meteosat-10"

ob_file_format   = "msg_reticulate"

ob_accumulation  = "0h"

ob_interp_method = "closest"

verif_date       = as.POSIXct(as.character(init_time), format="%Y%m%d%H") + lead_time * 60 * 60

#####################
###### MODEL ########
#####################

# fc_file_path <- paste0(here(), "/ACCORD_VS_202406/sample_data/deode")

fc_file_path 	 = ""

fc_file_template <- msgFile

fc_param_defs    = NULL

fc_file_opts <- list(
                     python_function = python_function,
                     python_version = python_version,
                     file_4regridding_grb = file_4regridding_grb,
                     grib_message = messnum,
                     strAreaId = areaId,
                     invert_data = TRUE,
                     is_obs = FALSE,
                     origin = model
                     )

fc_interp_method  = "closest"

fc_file_format    = "msg_reticulate"

fc_accumulation   = "0h"


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

# verif_domain  = NULL

######################
####### BASIC ########
######################

thresholds <- switch(param,
                     "WV_062" = c(220, 240, 260 ),   # TODO: set meaningful thresholds for WV 6.2
                     "IR_108" = c(273, 260, 250, 240, 230))

percentiles    = c(25, 50, 75, 90, 95)
scores         = c("mse", "mae", "bias", "rmse", "Rpearson", "FSS", "FSSp")

return_data    = TRUE
return_fields  = TRUE

sqlite_path    = NULL
sqlite_file    = NULL


