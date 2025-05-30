# ## harpIO
# install_github("pollyaschm/harpIO", "invert-hdf5-data")

library(harp)
library(Rgrib2)
library(hdf5r)
library(here)

fc_file_path 		<- paste0(here(), "/ACCORD_VS_202406/sample_data/deode")

init_time		<- 2024010200
model     		<- "DK2500m_atos" # DK2500m_atos, DK2500mhres, DK500m_atos, DK500m_hres
lead_time               <- 24

fc_file_template <- switch(
                          model,
                          "DK2500m_atos" = paste0(init_time, "/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
                          "DK2500m_hres" = paste0(init_time, "/harmonie_DK2500g_SP_HRES/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
                          "DK500m_atos"  = paste0(init_time, "/harmonie_DK500g_SP_ATOSDT_00bd/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"),
                          "DK500m_hres"  = paste0(init_time, "/harmonie_DK500g_SP_HRES/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"))

ob_file_path		<- paste0(here(), "/ACCORD_VS_202406/sample_data/radar")
pcp_accum_period	<- "3h"
param			<- paste0("Accpcp", pcp_accum_period)

ob_file_template	<- "{YYYY}{MM}{DD}{HH}00.kavrRAD.03.h5"

### verification domain ###
source(paste0(here(), "/ACCORD_VS_202406/scripts/reading_functions.R"))

# deode_grb_2500 <- read_param_with_grbmessg(paste0(here(), "/ACCORD_VS_202406/sample_data/deode/2024010200/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+0024h00m00s.grb"), 1)
deode_grb_500  <- read_param_with_grbmessg(paste0(here(), "/ACCORD_VS_202406/sample_data/deode/2024010200/harmonie_DK500g_SP_ATOSDT_00bd/surface_gc_1500x1500_500m+0024h00m00s.grb"), 1)

# verif_domain_2500 <- get_domain(deode_grb_2500)
verif_domain_500  <- get_domain(deode_grb_500)
###

my_param_defs <- modify_param_def(
                               "pcp",
                               decription = "Total precipitation",
                               grib = new_grib_param(
                                                     name = list(r = "tirf",
                                                                 g = "tgrp",
                                                                 s = "tsnowp"
                                                                 ),
                                                     ),
                               func = function(r, g, s) r + g + s,
			       accum = extract_numeric(pcp_accum_period)
                               ) 

ob_format_opts <- list(data_path="/dataset1/data1/data",
                      odim="FALSE",
                      meta=TRUE,
                      invert_data=FALSE
                      )


# harpSpatial development
verif <- verify_spatial(
  dttm		    = init_time, 
  fcst_model        = model,
  parameter         = param, 
  lead_time         = lead_time,
  fc_file_path      = fc_file_path,
  fc_file_template  = fc_file_template,
  fc_accumulation   = NULL, #pcp_accum_period,
  fc_file_format    = "grib",
  fc_param_defs     = my_param_defs,
  ob_file_path      = ob_file_path,
  ob_file_template  = ob_file_template,
  ob_file_format    = "hdf5",
  ob_file_opts      = ob_format_opts,
  ob_accumulation   = pcp_accum_period,
  verif_domain      = verif_domain_500,
  return_data       = TRUE,
  sqlite_path       = NULL, # sqlite_path,
  sqlite_file       = NULL, # sqlite_file,
  thresholds        = c(1, 3, 5, 7, 10, 15, 20, 25, 30),
  return_fields     = TRUE
)

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


### precipitation fields ###
message("
To test plotting the actual fields, run this script manually in an R session.")

# # GeoSphere plotting scheme
# prec_breaks = c(0, 0.1, 0.2, 0.5, 1., 5., 10., 15,
#            20., 25., 30., 35., 40., 45., 50., 100)
# prec_palette = c("#CFCCC8","#00FE96","#00FEC8","#00FEFE",  ## "#FFFFFE"
#             "#00C8FE","#0096FE","#0032FE","#3200FE",
#             "#6400FE","#9600FE","#C800FE","#FA00FE",
#             "#C800C8", "#960096","#FF0000")

# DMI plotting scheme
prec_breaks = c(0, 0., 1, 3, 5, 7, 10., 15.,
           20, 25, 30, 40, 50, 60, 70, 80, 100)
prec_palette = c("#e5ebec", "#c7e5fb","#8cc7f2", "#45a6eb", "#1c73b2",
                 "#4d991b", "#71ce9c", "#b4db72", "#f5f305",
                 "#f6d125", "#f6a625", "#f54125", "#ae092f",
                 "#d59de5", "#9c04c6", "#23052b")

plot_ob <- plot_field(
           verif$obfield,
           palette = prec_palette,
           breaks  = prec_breaks,
	   title   = as.character(paste("obs  ", param, "\n", verif_date)),
)
plot_fc <- plot_field(
           verif$fcfield,
           palette = prec_palette,
           breaks  = prec_breaks,
	   title  = as.character(paste(model, "  ", param, "\n",
				       init_time, "+", lead_time, "h"))
           )

