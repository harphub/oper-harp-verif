# ## harpSpatial development 
# install_github("pollyaschm/harpSpatial", "ACCORD_VS_202406")

library(harp)
library(Rgrib2)
library(here)

source(paste0(here(), "/ACCORD_VS_202406/scripts/reading_functions.R"))

############################

python_function = paste0(here(), "/ACCORD_VS_202406/scripts/reading_functions.py")
# this is to use satpy, you can point to your python env if you have satpy installed
python_version = "/perm/miag/venvs/satpy/bin/python3"

############################
# define:
#
msgFile    <- paste0(here(), "/ACCORD_VS_202406/sample_data/seviri/MSG3-SEVI-MSG15-0100-NA-20240102225743.579000000Z-NA.nat")

param      <-"IR_108" # "WV_062"

model <- "DK2500m_hres" # "DK2500m_hres", "DK2500m_atos", "DK500m_hres", "DK500m_atos"

init_time  <- 2024010200
lead_time  <- 23
############################
areaId <- switch(model,
                 "DK2500m_hres" = c(53, 6, 59, 17, 2.5, 'laea'),
                 "DK2500m_atos" = c(53, 6, 59, 17, 2.5, 'laea'),
                 "DK500m_hres"  = c(53, 6, 59, 17, 0.5, 'laea'),
                 "DK500m_atos"  = c(53, 6, 59, 17, 0.5, 'laea')
                 )
# We use grib message to extract radiances from grib files (shortName is unknown)
messnum       <- switch(param,
                  "WV_062" = 4,
                  "IR_108" = 5)

thresholds    <- switch(param,
                     "WV_062" = c(220, 240, 260 ),   # TODO: set meaningful thresholds for WV 6.2
                     "IR_108" = c(273, 260, 250, 240, 230))

fc_file_template  <- msgFile
ob_file_template  <- msgFile

fc_file_path      <- paste0(here(), "/ACCORD_VS_202406/sample_data/deode")

grb_file_template <- switch(
                          model,
                          "DK2500m_atos" = paste0(init_time, "/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
                          "DK2500m_hres" = paste0(init_time, "/harmonie_DK2500g_SP_HRES/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
                          "DK500m_atos"  = paste0(init_time, "/harmonie_DK500g_SP_ATOSDT_00bd/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"),
                          "DK500m_hres"  = paste0(init_time, "/harmonie_DK500g_SP_HRES/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"))

file_4regridding_grb <- generate_filenames(
		            file_path     = fc_file_path,
			    file_date     = init_time,
			    file_template = grb_file_template,
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


verif <- verify_spatial(
  dttm		    = init_time, 
  fcst_model        = model,
  parameter         = param,
  lead_time         = lead_time,
  lt_unit	    = "h",
  fc_file_path      = "",
  fc_file_template  = fc_file_template,
  fc_accumulation   = "0h",
  fc_file_format    = "msg_reticulate",
  fc_file_opts      = fc_reticulate_opts,
  fc_param_defs     = NULL,
  fc_interp_method  = NULL,
  ob_file_path      = "",
  ob_file_template  = ob_file_template,
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

message("
Results from verify_spatial: ")
print(verif)

####################
# quick plot
####################

verif_date <- as.POSIXct(as.character(init_time), format="%Y%m%d%H") + lead_time * 60 * 60

### FSS ###

ggplot(
       verif$FSS,
       aes(factor(scale), factor(threshold),
         fill = fss
         )) +
geom_tile(width=2) +
scale_fill_gradient2(
                   midpoint = 0.5,
                   low = scales::muted("blue"),
                   high= scales::muted("green")
                   ) +
labs(
     x = "window sizes",
     y = "threshold",
     title = paste("Model: ", model, ", Param: ", param),
     subtitle = paste0("Period: ", format(verif_date, format="%Y-%m-%d %H:%M"),
                     " + ", lead_time)
     )
ggsave(paste0("PLOTS/FSS_", param, "_", format(verif_date, format="%Y%m%d%H%M+"), lead_time, "_", model, ".png"))
message("Saved FSS plot to:
", paste0("./PLOTS/FSS_", param, "_", format(verif_date, format="%Y%m%d%H%M+"), lead_time, "_", model, ".png"))


### radiances fields ###

message("
To test plotting the actual fields, run this script manually in an R session.")

library(RColorBrewer)

ob_info <- attributes(verif$obfield)$info
plot_ob <- plot_field(
           verif$obfield,
           palette   = brewer.pal(8, "Greys"),
           # palette = brewer.pal(length(thresholds)+1, "Greys"),
           # breaks  = thresholds,
	   title     = as.character(paste(ob_info$origin , ob_info$name, "\n", ob_info$TIME))
)

fc_info <- attributes(verif$fcfield)$info
plot_fc <- plot_field(
           verif$fcfield,
           palette   = brewer.pal(8, "Greys"),
           # breaks  = prec_breaks,
	   title     = as.character(paste(fc_info$origin , fc_info$name, "\n", fc_info$TIME))
           )
# remove.packages("harpSpatial")

