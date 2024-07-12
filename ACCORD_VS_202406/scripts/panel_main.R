library(harp)
library(Rgrib2)
library(hdf5r)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(here)

######################################################
### DEFINE MODELS, DATE, LEAD_TIMES
######################################################

### tp ####
# def_param <- "tp"
def_param <- "sat"

definition_file <- switch(def_param,
			  "tp" = paste0(here(),"/ACCORD_VS_202406/panel_configs/definitions_tp_data.R"),
			  "sat" = paste0(here(),"/ACCORD_VS_202406/panel_configs/definitions_sat_data.R"))

definition_file_plt <- switch(def_param,
			      "tp" = paste0(here(),"/ACCORD_VS_202406/panel_configs/definitions_tp_plotting.R"),
			      "sat" = paste0(here(),"/ACCORD_VS_202406/panel_configs/definitions_sat_plotting.R"))


init_time <- 2024010200

lead_time <- switch (def_param,
		     "tp" = 24,
		     "sat"= 23)

models <- c("DK2500m_atos", "DK2500m_hres", "DK500m_atos", "DK500m_hres")

#######################################################
### READ ALL DATA into verif_data and verif_fields ####
#######################################################

verif_data   <- vector("list", length(models))
verif_fields <- vector("list", length(models))
names(verif_data)   <- models
names(verif_fields) <- models 

for (model in models){

  source(definition_file)

  message("ob_file_opts: ")
  print(ob_file_opts)
  message("ob_file_template: ", ob_file_template)
  message("model:  ", model)
  message("fcdate: ", init_time)

  verif <- verify_spatial(
    dttm	      = init_time, 
    fcst_model        = model,
    parameter         = param,
    lead_time         = lead_time,
    fc_file_path      = fc_file_path,
    fc_file_template  = fc_file_template,
    fc_accumulation   = fc_accumulation,
    fc_file_format    = fc_file_format,
    fc_file_opts      = fc_file_opts,
    fc_interp_method  = fc_interp_method,
    fc_param_defs     = fc_param_defs,
    ob_file_path      = ob_file_path,
    ob_file_template  = ob_file_template,
    ob_file_format    = ob_file_format,
    ob_file_opts      = ob_file_opts,
    ob_interp_method  = ob_interp_method,
    ob_accumulation   = ob_accumulation,
    verif_domain      = verif_domain,
    sqlite_path       = sqlite_path,
    sqlite_file       = sqlite_file,
    thresholds        = thresholds,
    percentiles       = percentiles,
    scores            = scores,
    return_data       = return_data,
    return_fields     = return_fields
  )
  fc_tmp <- list(fcfield = tibble::tibble(
				  valid_dttm = verif_date,
				  parameter  = param,
				  lead_time  = lead_time,
				  fcdate     = init_time,
				  !!as.name(model) := geolist(verif$fcfield),
				  units      = prm_units
				  ) 
  )
  class(fc_tmp) <- "harp_fcst"
  ob_tmp <- list(obfield = tibble::tibble(
				  valid_dttm = verif_date,
				  parameter  = param,
				  !!as.name(ob_name) := geolist(verif$obfield),
				  units      = prm_units
				  ) 
  )
  class(ob_tmp) <- "harp_analysis"

  verif$fcfield      <- NULL
  verif$obfield      <- NULL
  # verif$verif_domain <- NULL

  verif_data[[model]]   <- verif
  verif_fields[[model]] <- fc_tmp
}

verif_fields[[ob_name]]   <- ob_tmp
verif_fields$verif_domain <- verif_domain

message("verif_data: ")
print(verif_data)
message("verif_fields: ")
print(verif_fields)


########################
### RANK THE MODELS ####
########################

source(paste0(here(), "/ACCORD_VS_202406/scripts/panel_utils.R"))
source(paste0(here(), "/ACCORD_VS_202406/scripts/panel_ranking_functions.R"))

verif_data <- main_ranking(verif_data)

########################
####### PLOTTING #######
########################

source(paste0(here(), "/ACCORD_VS_202406/scripts/panel_plotting_functions.R"))

plot_name <- paste0("panel_", param, "_", format(verif_date, "%Y%m%d%H%M+"), lead_time, "_", model, ".png")

main_plotting(verif_data      = verif_data,
	      verif_fields    = verif_fields,
	      ob_name         = ob_name,
	      param           = param,
	      plt_definitions = definition_file_plt,
	      plot_name       = plot_name)

# # Quick plot FSS only 
# for (model in models){
#   verif    <- verif_data[[model]]
#   title    <- paste("Model: ", model, ", Param: ", param)
#   subtitle <- paste0("Period: ", format(verif_date, "%Y-%m-%d %H:%M"), 
#                        " + ", lead_time)
#   plt_name <- paste0("./PLOTS/FSS_", param, "_", format(verif_date, "%Y%m%d%H%M+"), lead_time, "_", model, ".png")
#   quick_plot_FSS(verif, title, subtitle, plt_name)
# }

# # Quick plot fields only
# title    <- paste(ob_name, ", Param: ", param)
# subtitle <- paste0("Period: ", format(verif_date, "%Y-%m-%d %H:%M"))
# plt_name <- paste0("./PLOTS/field_", param, "_", format(verif_date, "%Y%m%d%H%M"), "_", ob_name, ".png")
# 
# plot_panel_field(verif_fields[[ob_name]]$obfield,
# 		 ob_name,
# 		 title    = title,
# 		 subtitle = subtitle,
# 		 breaks   = breaks,
# 		 palette  = palette,
# 		 plt_name = plt_name)
# 
# for (model in models){
# 	title    <- paste("Model: ", model, ", Param: ", param)
# 	subtitle <- paste0("Period: ", format(verif_date, "%Y-%m-%d %H:%M"),
#                        " + ", lead_time)
# 	plt_name <- paste0("./PLOTS/field_", param, "_",
# 			   format(init_time, format="%Y%m%d%H%M+"),
# 			   lead_time, "_", model, ".png")
# 	plot_panel_field(verif_fields[[model]]$fcfield,
# 			 model,
# 			 title    = title,
# 			 subtitle = subtitle,
# 			 breaks   = breaks,
# 			 palette  = palette,
# 			 plt_name = plt_name)
# }
