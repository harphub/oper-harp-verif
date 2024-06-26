require(harp)
require(reticulate)
require(meteogrid)

source("/perm/aut4452/ACCORD_VS/R/harp_local_installation/read_DataUsingReticulate.R")

############################

# python_function = "/perm/miag/ACCORD_VS/scripts/reading_functions.py"
# python_version = "/perm/miag/venvs/satpy/bin/python3"

python_function = "/perm/aut4452/ACCORD_VS/R/harp_local_installation/reading_functions.py"
python_version = "/perm/aut4452/venvs/satpy/bin/python3"

############################
# define:
#
msgFile <- "/perm/miag/ACCORD_VS/MSG/20240102/MSG3-SEVI-MSG15-0100-NA-20240102225743.579000000Z-NA.nat"

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

str_addition <- switch(
		       as.character(init_time),
		       "2024010200" = "_jan",
		       "20231220" = ""
		       )

fc_file_template        <- switch(
                                  experiment,
                                  "DK2500m_atos" = paste0(init_time, "/harmonie_DK2500g_SP", str_addition, "_ATOSDT_00bd/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
                                  "DK2500m_hres" = paste0(init_time, "/harmonie_DK2500g_SP", str_addition, "_HRES/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
                                  "DK500m_atos"  = paste0(init_time, "/harmonie_DK500g_SP", str_addition, "_ATOSDT_00bd/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"),
                                  "DK500m_hres"  = paste0(init_time, "/harmonie_DK500g_SP", str_addition, "_HRES/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"))


file_4regridding_grb <- generate_filenames(file_path = "/perm/miag/ACCORD_VS/deode_exps",
		   file_date = 2024010223,
		   file_template = fc_file_template,
		   lead_time = lead_time
		   )

file.exists(file_4regridding_grb)

ob_reticulate_opts <- list(
			    python_function = python_function, 
			    python_version = python_version,
			    file_4regridding_grb = file_4regridding_grb,
			    grib_message = messnum,
			    strAreaId = areaId,
			    invert_data = TRUE,
                            is_obs = TRUE,
			    origin = "Meteosat-10"
			    )

fc_reticulate_opts <- list(
			    python_function = python_function,
			    python_version = python_version,
			    file_4regridding_grb = file_4regridding_grb,
			    grib_message = messnum,
			    strAreaId = areaId,
			    invert_data = TRUE,
                            is_obs = FALSE,
			    origin = experiment
			    )
##################
#  call the function like this: 
##################
mod <- read_msg_reticulate(file_name = msgFile,
		    parameter = strChannel,
                    date_times = init_time,
		    is_forecast = TRUE,
                    lead_time = lead_time,
		    format_opts = fc_reticulate_opts
		    )

sat <- read_msg_reticulate(file_name = msgFile,
		    parameter = strChannel,
                    date_times = init_time,
		    is_forecast = FALSE, 
                    lead_time = lead_time,
		    format_opts = ob_reticulate_opts
		    )

mod_via_read_grid <- read_grid(msgFile,
	  strChannel,
          ddtm = init_time,
          lead_time = lead_time,
	  file_format = "msg_reticulate",
	  file_format_opts = fc_reticulate_opts,
	  show_progress = TRUE
	  )

mod_via_read_grid <- read_grid(msgFile,
	  strChannel,
          ddtm = init_time,
          lead_time = lead_time,
	  file_format = "msg_reticulate",
	  file_format_opts = ob_reticulate_opts,
	  show_progress = TRUE
	  )

#####################
# quick plot

library(RColorBrewer)

veri_date <- format(as.POSIXct(as.character(init_time), format="%Y%m%d%H") + lead_time * 60 * 60, "%Y%m%d %H UTC")
info <- attributes(mod_via_read_grid)$info

plot_ob <- plot_field(
           mod_via_read_grid,
           palette = brewer.pal(8, "Greys"),
           # palette = brewer.pal(length(thresholds)+1, "Greys"),
           #breaks  = thresholds,
           title   = as.character(paste(info$origin , info$name, "\n", info$TIME)),
)

