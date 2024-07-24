library(harp)
library(Rgrib2)
library(hdf5r)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(here)
library(yaml)

####################
### LOAD CONFIGS ###
####################

args <- commandArgs(trailingOnly=TRUE)

if (length(args) == 0){
	run_case <- NULL 
	yaml_file <- "./panel_configs/panelification.yml"
} else if (length(args) == 1){
	run_case  <- args[[1]]
	yaml_file <- "./panel_configs/panelification.yml"  
} else {
	run_case  <- args[[1]]
	yaml_file <- args[[2]]
}

configs   <- yaml.load_file(yaml_file)
message("Use configs from file: ", yaml_file)

if (is.null(run_case)){
	cfg <- configs$verify_case[[1]]  
	message("No specific verify_case was provided.
		Running default which is the first entry of verify_case in config file.")
} else {
	cfg <- configs$verify_case[[run_case]]
	message("Run verification for: ", run_case)
}

def_param <- cfg$def_param 
init_time <- cfg$init_time
lead_time <- cfg$lead_time 

models <- configs$models

definition_file_plt <- file.path(here(), "ACCORD_VS_202406/panel_configs", configs$plt_config[[def_param]])

#######################################################
### READ ALL DATA into verif_data and verif_fields ####
#######################################################

verif_data   <- vector("list", length(models))
verif_fields <- vector("list", length(models))
names(verif_data)   <- models
names(verif_fields) <- models 

for (model in models){

  definition_file <- configs[[model]][[def_param]]$definition_file
  source(file.path(here(), "ACCORD_VS_202406/panel_configs", definition_file))

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
    window_sizes      = window_sizes,
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

plot_name <- paste0("panel_", param, "_", format(verif_date, format="%Y%m%d%H%M+"), lead_time, ".png")

main_plotting(verif_data      = verif_data,
	      verif_fields    = verif_fields,
	      ob_name         = ob_name,
	      param           = param,
	      plt_definitions = definition_file_plt,
	      plot_name       = plot_name)

# Quick plot FSS only 
if (configs$save_additional_plt$FSS){
  for (model in models){
    verif    <- verif_data[[model]]
    title    <- paste("Model: ", model, ", Param: ", param)
    subtitle <- paste0("Period: ", format(init_time, format="%Y-%m-%d %H:%M"), 
                         " + ", lead_time)
    plot_name <- paste0("FSS_", param, "_", format(init_time, format="%Y%m%d%H%M"), "+", lead_time, "_", model, ".png")
    quick_plot_FSS(verif,
		   title     = title,
		   subtitle  = subtitle,
		   plot_name = plot_name)
  }
}


# Quick plot fields only
if (configs$save_additional_plt$fields){
  source(definition_file_plt)
  title    <- paste(ob_name, ", Param: ", param)
  subtitle <- paste0("Period: ", format(verif_date, format="%Y-%m-%d %H:%M"))
  plot_name <- paste0("field_", param, "_", format(verif_date, format="%Y%m%d%H%M"), "_", ob_name, ".png")
  
  plot_panel_field(verif_fields[[ob_name]]$obfield,
  		 ob_name,
  		 title    = title,
  		 subtitle = subtitle,
  		 breaks   = breaks,
  		 palette  = palette,
  		 plot_name = plot_name)
  
  for (model in models){
  	title    <- paste("Model: ", model, ", Param: ", param)
  	subtitle <- paste0("Period: ", format(init_time, format="%Y-%m-%d %H:%M"),
                         " + ", lead_time)
  	plot_name <- paste0("field_", param, "_",
  			   format(init_time, format="%Y%m%d%H%M"), "+", 
  			   lead_time, "_", model, ".png")
  	plot_panel_field(verif_fields[[model]]$fcfield,
  			 model,
  			 title    = title,
  			 subtitle = subtitle,
  			 breaks   = breaks,
  			 palette  = palette,
  			 plot_name = plot_name)
  }
}
message("Finished. ")
