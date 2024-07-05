# Those two packages below are installed with local changes.
# # harpIO - locally modified
# install.packages("/perm/aut4452/ACCORD_VS/R/harp_local_installation/harpIO_branch_DMI_GeoSphere.tar.gz", repos=NULL, type="source")

# # harpSpatial development - locally modified
# install.packages("/perm/aut4452/ACCORD_VS/R/harp_local_installation/harpSpatial_branch_DMI_GeoSphere_dev.tar.gz", repos=NULL, type="source")

library(harp)
library(Rgrib2)
library(here)

source(paste0(here(), "/ACCORD_VS_202406/scripts/read_DataUsingReticulate.R"))

############################

python_function = paste0(here(), "/ACCORD_VS_202406/scripts/reading_functions.py")
python_version = "/perm/aut4452/venvs/satpy/bin/python3" # TODO: replace this path

############################
# define:
#
msgFile <- paste0(here(), "/ACCORD_VS_202406/sample_data/seviri/MSG3-SEVI-MSG15-0100-NA-20240102225743.579000000Z-NA.nat")

param <-"IR_108" # "WV_062"

areaId <- c(53, 6, 59, 17, 2.5, 'laea')

ICmod <- "hres"

init_time  <- 2024010200
lead_time  <- 23
############################
experiment <- paste0(strAreaId, "_", ICmod)

messnum <- switch(param,
                  "WV_062" = 23,
                  "IR_108" = 24)

thresholds <- switch(param,
                     "WV_062" = c(220, 240, 260 ),   # TODO: set meaningful thresholds for WV 6.2
                     "IR_108" = c(273, 260, 250, 240, 230))

str_addition <- switch(
                       as.character(init_time),
                       "2024010200" = "_jan",
                       "20231220" = ""
                       )

fc_file_path  <- paste0(here(), "/ACCORD_VS_202406/sample_data/deode")

grb_file_template        <- switch(
                                  model,
                                  "DK2500m_atos" = paste0(init_time, "/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
                                  "DK2500m_hres" = paste0(init_time, "/harmonie_DK2500g_SP_HRES/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
                                  "DK500m_atos"  = paste0(init_time, "/harmonie_DK500g_SP_ATOSDT_00bd/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"),
                                  "DK500m_hres"  = paste0(init_time, "/harmonie_DK500g_SP_HRES/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"))


file_4regridding_grb <- generate_filenames(file_path = fc_file_path,
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

mod_via_read_grid <- read_grid(msgFile,
          param,
          ddtm = init_time,
          lead_time = lead_time,
          file_format = "msg_reticulate",
          file_format_opts = ob_reticulate_opts,
          show_progress = TRUE
          )


verif <- verify_spatial(
  dttm		    = init_time, 
  fcst_model        = experiment,
  parameter         = param,
  lead_time         = lead_time,
  lt_unit	    = "h",
  fc_file_path      = "",
  fc_file_template  = file_template,
  fc_accumulation   = "0h",
  fc_file_format    = "msg_reticulate",
  fc_file_opts      = fc_reticulate_opts,
  fc_param_defs     = NULL,
  fc_interp_method  = NULL,
  ob_file_path      = "",
  ob_file_template  = file_template,
  ob_file_format    = "msg_reticulate",
  ob_file_opts      = ob_reticulate_opts,
  ob_accumulation   = "0h",
  ob_interp_method  = NULL, 
  verif_domain      = NULL, # verif_domain_500,
  return_data       = TRUE,
  sqlite_path       = NULL, # sqlite_path,
  sqlite_file       = NULL, # sqlite_file,
  thresholds        = thresholds,
  return_fields     = TRUE
)


# quick plot

library(RColorBrewer)

veri_date <- format(as.POSIXct(as.character(init_time), format="%Y%m%d%H") + lead_time * 60 * 60, "%Y%m%d %H UTC")
info <- attributes(verif$obfield)$info

plot_ob <- plot_field(
           verif$obfield,
           palette   = brewer.pal(8, "Greys"),
           # palette = brewer.pal(length(thresholds)+1, "Greys"),
           # breaks  = thresholds,
	   title     = as.character(paste(info$origin , info$name, "\n", info$TIME))
)

info <- attributes(verif$obfield)$info
plot_fc <- plot_field(
           verif$fcfield,
           palette   = brewer.pal(8, "Greys"),
           # breaks  = prec_breaks,
	   title     = as.character(paste(info$origin , info$name, "\n", info$TIME))
           )
# remove.packages("harpSpatial")

