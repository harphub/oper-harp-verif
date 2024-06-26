# Those two below are loaded.
# # harpIO - locally modified
# install.packages("/perm/aut4452/ACCORD_VS/R/harp_local_installation/harpIO_branch_DMI_GeoSphere.tar.gz", repos=NULL, type="source")

# harpSpatial development - locally modified
# install.packages("/perm/aut4452/ACCORD_VS/R/harp_local_installation/harpSpatial_branch_DMI_GeoSphere_test.tar.gz", repos=NULL, type="source")

library(harp)
library(Rgrib2)
library(hdf5r)


fc_file_path 		<- "/perm/miag/ACCORD_VS/deode_exps"

init_time		<- 2024010200 # 20231220, 20240120
experiment		<- "DK2500m_atos" # DK2500m_atos, DK2500m_hres, DK500m_atos, DK500m_hres

fc_file_template	<- switch(
				  experiment,
				  "DK2500m_atos" = paste0(init_time, "/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
				  "DK2500m_hres" = paste0(init_time, "/harmonie_DK2500g_SP_HRES_jan/surface_gc_300x300_2500m+00{LDT}h00m00s.grb"),
				  "DK500m_atos"  = paste0(init_time, "/harmonie_DK500g_jan_ATOSDT_00bd/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"),
				  "DK500m_hres"  = paste0(init_time, "/harmonie_DK500g_SP_HRES_jan24/surface_gc_1500x1500_500m+00{LDT}h00m00s.grb"))

ob_file_path		<- "/perm/miag/ACCORD_VS/radar"
pcp_accum_period		<- "1h"  # "1h", "3h", "6h"
param			<- paste0("Accpcp", pcp_accum_period)

ob_file_template		<- switch(
				      pcp_accum_period,
				      "1h"	= "kavrrad_1h/{YYYY}{MM}{DD}{HH}00.kavrRAD.01.h5",
				      "3h"	= "kavrrad_3h/{YYYY}{MM}{DD}{HH}00.kavrRAD.03.h5",
				      "6h"	= "kavrrad_6h/{YYYY}{MM}{DD}{HH}00.kavrRAD.06.h5"
				      )

# #resolution
# model: 2500m, 500m
# radar: 500m 
source("/perm/aut4452/ACCORD_VS/R/harp_local_installation/read_deode_exps.R")

deode_grb_2500 <- read_param_with_grbmessg("/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS/deode_exps/2023122000/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+0036h00m00s.grb", 1)

deode_grb_500 <- read_param_with_grbmessg("/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS/deode_exps/2023122000/harmonie_DK500g_SP_ATOSDT_00bd/surface_gc_1500x1500_500m+0033h00m00s.grb", 1)

verif_domain_2500 <- get_domain(deode_grb_2500)
verif_domain_500 <- get_domain(deode_grb_500)

domain_500_DK_mainland <- geo_subgrid(verif_domain_500, 55*5, 190*5,80*5, 245*5)
domain_2500_DK_mainland <- geo_subgrid(verif_domain_2500, 55, 190,80, 245)

 
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


lead_time <- 35 # seq(31, 31)
veri_date <- format(as.POSIXct(as.character(init_time), format="%Y%m%d%H") + lead_time * 60 * 60, "%Y%m%d %H UTC")

# # harpSpatial master
# verif3h <- verify_spatial(
#   start_date        = init_time,
#   end_date          = init_time,
#   det_model         = experiment,
#   parameter         = "pcp", ##prm$basename, #"AccPcp3h",
#   lead_time         = lead_time,
#   fc_file_path      = fc_file_path,
#   fc_file_template  = fc_file_template,
#   fc_file_format = "grib",
#   fc_param_defs     = my_param_defs,
#   ob_file_path      = ob_file_path,
#   ob_file_template  = ob_file_template,
#   ob_accumulation   = ob_accum_period,
#   verif_domain      = verif_domain_500,
#   return_data = TRUE,
#   sqlite_path = NULL, # sqlite_path,
#   sqlite_file = NULL, # sqlite_file,
#   thresholds = c(0.1, 1, 5, 10, 15, 20, 30, 40)
# )

# harpSpatial development
verif <- verify_spatial(
  dttm		    = init_time, 
  fcst_model        = experiment,
  parameter         = param, ##prm$basename, #"Accpcp3h",
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
  verif_domain      = domain_500_DK_mainland,
  return_data       = TRUE,
  sqlite_path       = NULL, # sqlite_path,
  sqlite_file       = NULL, # sqlite_file,
  thresholds        = c(1, 3, 5, 7, 10, 15, 20, 25, 30),
  percentiles       = c(25, 50, 75, 90, 95),
  scores            = c("FSSp", "FSS"), #, "SAL", "mse", "mae", "bias", "NACT", "rmse", "Rpearson"),
  return_fields     = TRUE
)


# quick plot
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
	   title   = as.character(paste("composite  ", param, "\n", veri_date)),
)

plot_fc <- plot_field(
           verif$fcfield,
           palette = prec_palette,
           breaks  = prec_breaks,
	   title  = as.character(paste(experiment, "  ", param, "\n",
				       init_time, "+", lead_time, "h"))
           )

print(verif)

remove.packages("harpSpatial")

# source(paste0("/home/aut4452/ACCORD_VS/scripts/config/config_", experiment))
