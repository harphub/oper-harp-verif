require(harp)
require(reticulate)
require(meteogrid)
require(here)

source(paste0(here(), "/ACCORD_VS_202406/scripts/read_DataUsingReticulate.R"))

############################

python_function = paste0(here(), "/ACCORD_VS_202406/scripts/reading_functions.py")
python_version  = "/perm/aut4452/venvs/satpy/bin/python3" # TODO: replace this path

############################
# define:
#
msgFile    <- paste0(here(), "/ACCORD_VS_202406/sample_data/seviri/MSG3-SEVI-MSG15-0100-NA-20240102225743.579000000Z-NA.nat")

strChannel <-"IR_108" # "WV_062"

areaId <- c(53, 6, 59, 17, 2.5, 'laea')
ICmod <- "hres"

init_time  <- 2024010200
lead_time  <- 23
############################

experiment <- paste0(strAreaId, "_", ICmod)

messnum <- switch(strChannel,
		  "WV_062" = 23,
		  "IR_108" = 24)

fc_file_template        <- switch(
                                  model,
                                  "DK2500m_atos" = paste0(init_time, "/harmonie_DK2500g_SP_jan_ATOSDT_00bd/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
                                  "DK2500m_hres" = paste0(init_time, "/harmonie_DK2500g_SP_HRES_jan/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
                                  "DK500m_atos"  = paste0(init_time, "/harmonie_DK500g_jan_ATOSDT_00bd/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"),
                                  "DK500m_hres"  = paste0(init_time, "/harmonie_DK500g_SP_HRES_jan24/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"))

fc_file_path <- paste0(here(), "/ACCORD_VS_202406/sample_data/deode")

file_4regridding_grb <- generate_filenames(
	                  file_path     = fc_file_path,
		          file_date     = init_time,
		          file_template = fc_file_template,
		          lead_time     = lead_time
			  )

file.exists(file_4regridding_grb)

ob_reticulate_opts <- list(
			    python_function      = python_function, 
			    python_version       = python_version,
			    file_4regridding_grb = file_4regridding_grb,
			    grib_message         = messnum,
			    strAreaId            = areaId,
			    invert_data          = TRUE,
                            is_obs               = TRUE,
			    origin               = "Meteosat-10"
			    )

fc_reticulate_opts <- list(
			    python_function      = python_function,
			    python_version       = python_version,
			    file_4regridding_grb = file_4regridding_grb,
			    grib_message         = messnum,
			    strAreaId            = areaId,
			    invert_data          = TRUE,
                            is_obs               = FALSE,
			    origin               = model
			    )
##################
#  call the function like this: 
##################
mod <- read_msg_reticulate(
	            file_name   = msgFile,
		    parameter   = strChannel,
                    date_times  = init_time,
		    is_forecast = TRUE,
                    lead_time   = lead_time,
		    format_opts = fc_reticulate_opts
		    )

sat <- read_msg_reticulate(
	            file_name   = msgFile,
		    parameter   = strChannel,
                    date_times  = init_time,
		    is_forecast = FALSE, 
                    lead_time   = lead_time,
		    format_opts = ob_reticulate_opts
		    )

# or via read_grid() :
mod_via_read_grid <- read_grid(
			       msgFile,
			       strChannel,
			       ddtm             = init_time,
			       lead_time        = lead_time,
			       file_format      = "msg_reticulate",
			       file_format_opts = fc_reticulate_opts,
			       show_progress    = TRUE
			       )

sat_via_read_grid <- read_grid(
			       msgFile,
			       strChannel,
			       ddtm             = init_time,
			       lead_time        = lead_time,
			       file_format      = "msg_reticulate",
			       file_format_opts = ob_reticulate_opts,
			       show_progress    = TRUE
			       )

#####################
# quick plot

library(RColorBrewer)

veri_date <- format(as.POSIXct(as.character(init_time), format="%Y%m%d%H") +
		    lead_time * 60 * 60, "%Y%m%d %H UTC")

plot_ob <- plot_field(
           mod_via_read_grid,
           palette = brewer.pal(8, "Greys"),
           # palette = brewer.pal(length(thresholds)+1, "Greys"),
           #breaks  = thresholds,
           title   = as.character(paste(info$origin , info$name, "\n", info$TIME)),
)

