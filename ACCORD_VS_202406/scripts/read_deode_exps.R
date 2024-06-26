## should be pre-loaded:
# module load R/4.2.2
# module load ecmwf-toolbox
##
# install.packages("/perm/aut4452/ACCORD_VS/R/harp_local_installation/harpSpatial.tar.gz", repos=NULL, type='source')

library(harp)
library(Rgrib2)


# case <- 2023122000
# exp <- "harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m"
# lt <- 30
# 
# path2data <- "/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS"
# 
# file <- paste0(path2data, "/deode_exps/", case, "/", exp, "+0030h00m00s.grb")

accum_period <- "3h"
deode_grb_file_1 <- "/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS/deode_exps/2023122000/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+0033h00m00s.grb"
deode_grb_file_2 <- "/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS/deode_exps/2023122000/harmonie_DK2500g_SP_ATOSDT_00bd/surface_gc_300x300_2500m+0036h00m00s.grb"

deode_grb_file_1 <- "/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS/deode_exps/2023122000/harmonie_DK500g_SP_ATOSDT_00bd/surface_gc_1500x1500_500m+0033h00m00s.grb"
deode_grb_file_2 <- "/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS/deode_exps/2023122000/harmonie_DK500g_SP_ATOSDT_00bd/surface_gc_1500x1500_500m+0036h00m00s.grb"

deode_grb_file_1 <- "/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS/deode_exps/2023122000/harmonie_DK2500g_SP_HRES/surface_gc_300x300_2500m+0033h00m00s.grb"
deode_grb_file_2 <- "/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS/deode_exps/2023122000/harmonie_DK2500g_SP_HRES/surface_gc_300x300_2500m+0036h00m00s.grb"

deode_grb_file_1 <- "/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS/deode_exps/2023122000/harmonie_DK500g_SP_HRES/surface_gc_1500x1500_500m+0033h00m00s.grb"
deode_grb_file_2 <- "/etc/ecmwf/nfs/dh2_perm_b/aut4452/ACCORD_VS/deode_exps/2023122000/harmonie_DK500g_SP_HRES/surface_gc_1500x1500_500m+0036h00m00s.grb"

# file.exists(file)

## grib definitions ##

# grb_opts <- grib_opts(
# 		       param_find = list(tirf = use_grib_shortName("tirf"),
# 					 tgrp = use_grib_shortName("tgrp"),
#					 tnowp = use_grib_shortName("tsnowp"))
# 		       )

## read data ##

## precipitation
# read_grid(file,
# 	  parameter = "tirf",
# 	  file_format = "grib",
# 	  file_format_opts = grb_opts
# 	  )


#' Read total precipitation from DEODE grib
#'
#'@param file
#'@return geofield
#' param <- read_deode_tp("<your_file>")
read_deode_tp <- function(file){
	# export ECCODES_DEFINITION_PATH=$PERM/definitions:ECCODES_DEFINITION_PATH
	# added: /perm/aut4452/definitions/grib2/localConcepts/lfpw containging grib parameter information
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
				       accum = extract_numeric(accum_period)

				       )
	tp_gf <- read_grid(
			   deode_grb_file_1,
			   "pcp",
			   param_defs = my_param_defs,
			   file_format = "grib"
			   )


	return (tp_gf)
}

# quick plot precipitation
# prec_breaks = c(0, 0.1, 0.2, 0.5, 1., 5., 10., 15.,
#            20., 25., 30., 35., 40., 45., 50., 100)
# prec_palette = c("#CFCCC8","#00FE96","#00FEC8","#00FEFE",
# # prec_palette = c("#FFFFFE","#00FE96","#00FEC8","#00FEFE",
#             "#00C8FE","#0096FE","#0032FE","#3200FE",
#             "#6400FE","#9600FE","#C800FE","#FA00FE",
#             "#C800C8", "#960096","#FF0000")

# deode_tp_1 <- read_deode_tp(deode_grb_file_1)
# deode_tp_2 <- read_deode_tp(deode_grb_file_2)

# deode_tp <- deode_tp_2 - deode_tp_1


# plot_field(
#            deode_tp,
#            palette = prec_palette,
#            breaks = prec_breaks
#            )


#' Read parameter using grib message
#'
#'@param file 
#'@param grb_message
#'@return geofield
#' param <- read_param_with_grbmessg("<your_file>", <grb_message_nr>)
read_param_with_grbmessg <- function(file, grb_message){
        require(Rgrib2)
        grib_list <- Gopen(file)
        gf <- Gdec(grib_list, grb_message)
        return (gf)
}


