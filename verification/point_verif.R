#!/usr/bin/env Rscript

#================================================#
# MAIN POINT VERIFICATION SCRIPT
#================================================#

#================================================#
# PACKAGES ETC
#================================================#

suppressPackageStartupMessages({
  library(harp)
  library(here)
  library(argparse)
  library(yaml)
  library(dplyr) 
  library(tidyr) 
  library(purrr) # imap
  library(forcats) # fcst_inorder
  library(stringr) # str_to_title
  library(RColorBrewer) # Some colourmaps
  library(gridExtra) # For grid arrange
  library(grid) # For grid arrange
  library(pracma) # For logseq
  library(RSQLite) # For station selection
  library(scales) # For hue_pal()
  library(pals) # For trubetskoy palette
  library(scico) # For more (and better) colour palettes
})

# Check some optional packages
if ("sf" %in% rownames(installed.packages())) {
  library(sf)
  sf_available <- T
} else {
  warning("sf package not found - filtering via poly files will not work and will be skipped!\n")
  sf_available <- F
}
if ("cowplot" %in% rownames(installed.packages())) {
  library(cowplot)
  cowplot_available <- T
} else {
  warning("cowplot package not found\n")
  cowplot_available <- F
}

source(here::here("verification/fn_station_selection.R"))
source(here::here("verification/fn_verif_helpers.R"))
source(here::here("visualization/fn_plot_point_verif.R"))
source(here::here("visualization/fn_plot_aux_scores.R"))
source(here::here("visualization/fn_plot_helpers.R"))
source(here::here("visualization/fn_scorecard_signif.R"))
source(here::here("visualization/fn_plot_signif_diff.R"))
source(here::here("visualization/fn_plot_tile_scorecard.R"))

# Turn off the "summarise() has grouped by ..." messages from summarise
options(dplyr.summarise.inform = FALSE)

#================================================#
# READ COMMAND LINE ARGUMENTS
#================================================#

if (!interactive()) {
  
  parser <- argparse::ArgumentParser()
  parser$add_argument("-start_date",
                      type    = "character",
                      default = " None",
                      help    = "First date to process in YYYYMMDDHH format")
  parser$add_argument("-end_date",
                      type    = "character",
                      default = "None",
                      help    = "Final date to process in YYYYMMDDHH format")
  parser$add_argument("-config_file",
                      type    = "character",
                      default = "None",
                      help    = "Configuration file")
  parser$add_argument("-params_file",
                      type    = "character",
                      default = "verification/set_params.R",
                      help    = "Parameter file")
  parser$add_argument("-params_list",
                      type    = "character",
                      default = "T2m",
                      help    = "Which parameters to verify. This should be a comma
                                 separated string of parameters, for example
                                 T2m,S10m,T,S. These parameters must exist in the 
                                 parameter file. If params_list is not specified, 
                                 only T2m is run. Using params_list = ALL is 
                                 NOT recommended in general as it will use all
                                 parameters in the params_file")
  parser$add_argument("-dynamic_sid_gen",
                      type    = "logical",
                      default = TRUE,
                      help    = "If TRUE, geographic SID lists are generated on 
                                 the fly during the verification based on the 
                                 options avilable in fn_station_selection.R.
                                 If FALSE, the static lists in sid_list.rds will
                                 be used")
  parser$add_argument("-plot_dynamic_sid",
                      type    = "logical",
                      default = FALSE,
                      help    = "If TRUE, a map of the SID lists used for each 
                                 parameter will be plotted in the verification
                                 directory.")
  parser$add_argument("-mod_def_rds",
                      type    = "logical",
                      default = FALSE,
                      help    = "Flag to prepend the project name to the
                                 default rds filenames")
  parser$add_argument("-add_proj_png",
                      type    = "logical",
                      default = FALSE,
                      help    = "Flag to prepend project name to the default png 
                                 filenames")
  parser$add_argument("-rolling_verif",
                      type    = "logical",
                      default = FALSE,
                      help    = "Flag to indicate rolling verification. This 
                                 should only be used for surface variables")
  parser$add_argument("-gen_sc_only",
                      type    = "logical",
                      default = FALSE,
                      help    = "Flag to run scorecard generation and plotting only.
                                 This is not compatible with rolling_verif option")
  parser$add_argument("-use_fixed_dates",
                      type    = "logical",
                      default = TRUE,
                      help    = "Use the input start/end dates in output figs/rds?
                                 If FALSE, the first/last valid fcst_dttm are used")
  parser$add_argument("-skip_sid_verif",
                      type    = "logical",
                      default = FALSE,
                      help    = "Skip the SID verification i.e. map scores")
  parser$add_argument("-skip_thresh_verif",
                      type    = "logical",
                      default = FALSE,
                      help    = "Skip the threshold verification")

  args               <- parser$parse_args()
  start_date         <- args$start_date
  end_date           <- args$end_date
  config_file        <- args$config_file
  params_file        <- args$params_file
  params_list        <- args$params_list
  dynamic_sid_gen    <- args$dynamic_sid_gen
  plot_dynamic_sid   <- args$plot_dynamic_sid
  mod_def_rds        <- args$mod_def_rds
  add_proj_png       <- args$add_proj_png
  rolling_verif      <- args$rolling_verif
  gen_sc_only        <- args$gen_sc_only
  use_fixed_dates    <- args$use_fixed_dates
  skip_sid_verif     <- args$skip_sid_verif
  skip_thresh_verif  <- args$skip_thresh_verif

} else {
  
  # Source options from here instead of CLI
  cat("Assuming we are in debug mode!\n")
  source(here::here("verification/source_options.R"))
  
}

# Check if required arguments are missing
if (any(c(start_date,end_date,config_file) == "None")) {
  stop("You need to provide start_date, end_date, and a config file")
}

# Check if config_file exists
if (!file.exists(here::here(config_file))) {
  stop("Cannot find config file",here::here(config_file))
}

# Check if param file exists
if (!file.exists(here::here(params_file))) {
  stop("Cannot find parameter file",here::here(params_file))
}

# Check start/end dates
sedate     <- check_sedate(start_date,end_date)
start_date <- sedate$start_date
end_date   <- sedate$end_date

# Read in the parameter file
source(here::here(params_file))

#================================================#
# READ OPTIONS FROM THE CONFIG FILE
#================================================#

cat("%%%%%%%%% point_verif: Using config file",config_file,"and parameter file",params_file,"%%%%%%%%%\n")

CONFIG          <- yaml::yaml.load_file(here::here(config_file))
project_name    <- check_config_input(CONFIG,"verif","project_name")
fcst_model      <- check_config_input(CONFIG,"verif","fcst_model")
lead_time_str   <- check_config_input(CONFIG,"verif","lead_time")
lead_time       <- eval(parse(text = lead_time_str))
lead_time_UA    <- CONFIG$verif$lead_time_UA
by_step         <- check_config_input(CONFIG,"verif","by_step")
fcst_type       <- check_config_input(CONFIG,"verif","fcst_type")
fcst_path       <- check_config_input(CONFIG,"verif","fcst_path")
obs_path        <- check_config_input(CONFIG,"verif","obs_path")
verif_path      <- check_config_input(CONFIG,"verif","verif_path")
domains         <- check_config_input(CONFIG,"verif","domains",default="All")
members         <- CONFIG$verif$members
lags            <- check_config_input(CONFIG,"verif","lags")
shifts          <- check_config_input(CONFIG,"verif","shifts",default=list(NULL))
num_ref_members <- check_config_input(CONFIG,"verif","num_ref_members",default="Inf")
ua_fcst_cycle   <- check_config_input(CONFIG,"verif","ua_fcst_cycle",default=F)
lt_split        <- check_config_input(CONFIG,"verif","lt_split",default=F)
force_valid_thr <- check_config_input(CONFIG,"verif","force_valid_thr",default=F)
plot_output     <- check_config_input(CONFIG,"post","plot_output",default="default")
create_png      <- check_config_input(CONFIG,"post","create_png",default=T)
save_vofp       <- check_config_input(CONFIG,"post","save_vofp",default=F)
cmap            <- check_config_input(CONFIG,"post","cmap",default="Set2")
cmap_hex        <- check_config_input(CONFIG,"post","cmap_hex",default="magma")
map_cbar_d      <- check_config_input(CONFIG,"post","map_cbar_d",default=F)
create_scrd     <- check_config_input(CONFIG,"scorecards","create_scrd",default=F)

# Convert members and lags to named lists for read_point_forecast
members_list    <- get_named_list(members,fcst_model,"members")
lags_list       <- get_named_list(lags,fcst_model,"lags")
shifts_list     <- get_named_list(shifts,fcst_model,"shifts")

# Abort if directories do not exist
check_dirs_exist(c(fcst_path,obs_path,verif_path))

if ((!dir.exists(plot_output)) & (plot_output != "default")) {
  stop(plot_output," does not exist")
}

if (plot_output == "default") {
  plot_output   <- file.path(verif_path,"archive")
  if (!dir.exists(plot_output)) {
    dir.create(plot_output,showWarnings = TRUE,recursive = FALSE)
  }
}

# Create project_name directory if it does not exist
verif_path      <- file.path(verif_path,project_name)
plot_output     <- file.path(plot_output,project_name)
if (!dir.exists(verif_path)) {
  dir.create(verif_path,showWarnings = TRUE,recursive = FALSE)
}
if (!dir.exists(plot_output)) {
  dir.create(plot_output,showWarnings = TRUE,recursive = FALSE)
}

#================================================#
# DEFINE/GENERATE USEFUL VARIABLES
#================================================#

# Generate the parent cycles from start/end_date and by (used for EPS lagging)
all_pc        <- harpCore::seq_dttm(start_date,end_date,by_step)
all_pc        <- harpCore::as_YMDh(harpIO::str_datetime_to_datetime(all_pc))
parent_cycles <- sort(as.numeric(unique(substr(all_pc,9,10))))

# Number of days considered
num_days <- as.numeric(difftime(harpCore::as_dttm(end_date),
                                harpCore::as_dttm(start_date),
                                units = "days"))

# All possible UA variables considered (used during check in boostraping)
all_possible_UA_vars <- c("Z","T","RH","D","S","Q","Td")

# Generate the the stationlists (not required, should be done offline)
gen_station_lists <- FALSE
YYYY              <- substr(end_date,0,4)
sql_file          <- file.path(obs_path,
                               paste0("OBSTABLE_",YYYY,".sqlite"))
sl_dir            <- file.path(here::here("verification"))
sid_lists_fname   <- "sid_lists.rds" 
if ((gen_station_lists) & (!dynamic_sid_gen)) {
  # This is only required to generate the pre-defined station lists.
  # "DINI" and "T2m" are just dummy arguments in this case
  cat("Generating static SID lists in point_verif - do you really want to do this?\n")
  tmp_df   <- fn_station_selection(domain_choice    = "DINI",
                                   param            = "T2m",
                                   generate_domains = TRUE,
                                   domains_to_gen   = "All_Domains",
                                   plot_domains     = TRUE,
                                   sl_dir           = sl_dir,
                                   sql_file         = sql_file,
                                   png_path         = plot_output,
                                   domain_file      = sid_lists_fname,
                                   multlatlon_rmv   = TRUE)
  rm(tmp_df)
}

# Forecast type string
if (fcst_type == "eps") {
  ft_str <- "ens"
} else if (fcst_type == "det") {
  ft_str <- fcst_type
} else {
  stop("fcst_type ",fcst_type," is not recognised, aborting!")
}
# Define string for threhsolds
t_s_str <- paste0(ft_str,"_threshold_scores")

# Default missing data
missing_data <- list("verif"   = NA_character_,
                     "sc_data" = NA_character_)

#================================================#
# OPTION CHECKS
#================================================#

# Filter the params_list from "params_file" if specified
if (params_list == "ALL") {
  cat("Verify all parameters in",params_file," (this is NOT recommended!)\n")
} else {
  # Get rid of whitespace and split into parameters
  params_list <- stringr::str_split_1(gsub(" ","",params_list),",")
  params      <- params[params_list]
  # Remove empty entries
  params      <- params[lapply(params,length) > 0]
  if (length(params) == 0) {
    cat("Could not find any of the parameters:",
        paste(params_list,collapse = ","),"\n")
    stop("No parameters found!")
  } else {
    cat("Running for parameter(s):",paste(names(params),collapse = ","),"\n")
  }
}

# Make the grps for different verification types
grps_surface_default   <- harpCore::make_verif_groups(c("lead_time",
                     "valid_hour","valid_dttm"),c("fcst_cycle","station_group"))
grps_UA_default        <- harpCore::make_verif_groups(c("lead_time",
                     "valid_hour"),c("fcst_cycle","station_group"))
grps_SID_default       <- list("SID",c("SID","valid_hour"))
grps_threshold_default <- list("station_group",c("station_group","fcst_cycle"))

# Do not group over "station_group"!
grps_surface_default <- grps_surface_default[grepl("station_group",
                                             grps_surface_default,fixed = TRUE)]
grps_UA_default      <- grps_UA_default[grepl("station_group",
                                             grps_UA_default,fixed = TRUE)]

# Convert num_ref_members to Inf if required
if (!is.na(num_ref_members)) {
  if (num_ref_members == "Inf") {
    num_ref_members = Inf
  }
}

# Rolling verif stuff
if (rolling_verif) {
  
  # Check the number of days - if the period is too long, then abort!
  if (num_days >= 15) {
    cat("Rolling verif is TRUE but num_days=",num_days,", aborting!\n")
    stop("Period too long for rolling scores")
  }
  
  cat("Running in rolling verification mode!\n
      A reduced set of scores will be plotted with no rds files\n")
  cat("Changing plot_output in the config file to include a 'rolling' prefix\n")
  cat("You may need to create this directory if it does not already exist\n")
  po_split                       <- stringr::str_split_1(plot_output,"/")
  po_split[length(po_split) - 1] <- paste0("rolling_",
                                           po_split[length(po_split) - 1])
  plot_output                    <- paste(po_split,collapse = "/")
  create_scrd                    <- FALSE
  if (!dir.exists(plot_output)) {
    dir.create(plot_output,showWarnings = TRUE,recursive = FALSE)
  }
  
}

# Checks for the scorecard generation
if ((rolling_verif) & (gen_sc_only)) {
  stop("Cannot use rolling_verif and gen_sc_only options together!")
}
if (gen_sc_only) {
  if (create_scrd) {
    cat("Only generating scorecard data and plotting\n")
  } else {
    cat("gen_sc_only is TRUE but create_scrd is FALSE in config - the latter has priority!\n")
    stop("Switch create_scrd to TRUE in config")
  }
}
if (length(params) == 1) {
  cat("Only one input parameter, switching off scorecard generation\n")
  create_scrd <- FALSE
}

# png_projname is prefixed to the default png name
if (add_proj_png) {
  png_projname <- project_name
} else {
  png_projname <- NA_character_
}

# Checks for fixed dates
if (use_fixed_dates) {
  fsd <- start_date
  fed <- end_date
  cat("Using fixed start and end dates",fsd,"-",fed,"\n")
} else {
  fsd <- NA_character_
  fed <- NA_character_
}

# Set default lead_time
lead_time_default <- lead_time

#================================================#
# FIND MODEL WITH MIN NUMBER OF STATIONS TO 
# REDUCE INITIAL IO
#================================================#

model_domain_min <- create_station_filter(start_date,
                                          fcst_model,
                                          names(params)[1],
                                          fcst_path,
                                          sl_dir)

# Get members and lags corresponding to this model_domain_min
if (!is.na(model_domain_min)) {
  members_domain_min <- members_list[[model_domain_min]]
  lags_domain_min    <- lags_list[[model_domain_min]]
}

#================================================#
# MAIN VERIFICATION FUNCTION
#================================================#

run_verif <- function(prm_info, prm_name) {
  
  cat("<<<<<<<<<<<< Verifying",prm_name,">>>>>>>>>>>>>>\n")
  
  #================================================#
  # OPTION CHECKS
  #================================================#
  
  if (!is.null(prm_info$vc)) {
    vertical_coordinate <- prm_info$vc
  } else {
    vertical_coordinate <- NA_character_
  }
  
  # Reset to default groups
  grps_param     <- switch(
    vertical_coordinate,
    "pressure" = map(grps_UA_default, ~c(.x, "p")),
    "height"   = map(grps_UA_default, ~c(.x, "z")),
    grps_surface_default
  )
  grps_SID       <- grps_SID_default
  grps_threshold <- grps_threshold_default
  
  if (!is.na(vertical_coordinate)) {
    if (!ua_fcst_cycle) {
      # Remove grouping by fcst_cycle for UA vars
      grps_param <- lapply(grps_param,function(x) x[x != "fcst_cycle"])
      grps_param <- grps_param[lapply(grps_param,length) > 0] 
      grps_param <- unique(grps_param)
    }
    
    # Only run for certain "large" domains
    domains_to_run <- unique(c(base::intersect(domains,
                             c("All","Alps","IE_EN","NL_OP","SCD","FR","DE","Baltex"))))
    if (length(domains_to_run) == 0) {
      cat("For parameter",prm_name,", verify over All stations only\n")
      domains_to_run <- "All"
    }
    
  } else {
    domains_to_run <- domains
  }
  
  # Do not run threshold scores if in rolling verif (or if it is turned off)
  if ((rolling_verif) || (skip_thresh_verif)){
    thresholds_param <- NULL
  } else {
    thresholds_param <- prm_info$thresholds
  }
  
  # Allow for specific leadtimes for UA variables
  lead_time <- lead_time_default
  prm_name_nolevel <- gsub('[0-9]+','',prm_name)
  if (prm_name_nolevel %in% all_possible_UA_vars) {
    if (!is.null(lead_time_UA)) {
      lead_time <- eval(parse(text = lead_time_UA))
      cat("Using lead_time_UA option i.e. leadtimes=",lead_time,"for variable",prm_name,"\n")
    }
  }
  
  # Initialise timings
  dt_read  <- 0
  dt_aux   <- 0
  dt_verif <- 0
  dt_plot  <- 0
  dt_scrd  <- 0
  
  cat("Start the forecast/obs reading\n")
  st_read <- Sys.time()
  
  #================================================#
  # READ POINT FORCEAST
  #================================================#
  
  # This step reads the "smallest" model first, gets all stations, and then
  # uses this list when reading all models
  if (!is.na(model_domain_min)) {
    fcst_tmp <- try_rpforecast(start_date,
                               end_date,
                               by_step,
                               model_domain_min,
                               fcst_type,
                               prm_name,
                               lead_time,
                               members_domain_min,
                               lags_domain_min,
                               fcst_path,
                               NULL,
                               vertical_coordinate)
    
    if (is.null(fcst_tmp)) {
      warning("Failure during the FCTABLE reading process for ",prm_name,
              ", moving on to the next parameter")
      return(missing_data)
    }
    
    stations_filter <- sort(harpCore::unique_stations(fcst_tmp))
    rm(fcst_tmp)
  } else {
    stations_filter <- NULL
  }
  
  # Read all the models
  fcst <- try_rpforecast(start_date,
                         end_date,
                         by_step,
                         fcst_model,
                         fcst_type,
                         prm_name,
                         lead_time,
                         members_list,
                         lags_list,
                         fcst_path,
                         stations_filter,
                         vertical_coordinate)
  
  if (is.null(fcst)) {
    warning("Failure during the FCTABLE reading process for ",prm_name,
            ", moving on to the next parameter")
    return(missing_data)
  }
  
  #================================================#
  # LAG THE FORECAST FOR EPS EXPERIMENTS
  #================================================#
  
  if (fcst_type == "eps") {
    
    fcst <- try_epslag(fcst,
                       fcst_model,
                       parent_cycles,
                       vertical_coordinate)
    
    if (is.null(fcst)) {
      warning("Failure during the model data lagging process for ",prm_name,
              ", moving on to next parameter")
      return(missing_data)
    }
    
  } 
  
  #================================================#
  # SHIFT THE FORECAST (EXPERIMENTAL)
  #================================================#
  
  if ((!is.null(shifts_list[[1]])) & (fcst_type == "det")) {
    cat("Looks like you are trying to shift the models in time with:\n")
    print(shifts_list)
    fcst <- harpPoint::shift_forecast(
      fcst,
      fcst_shifts = shifts_list,
      keep_unshifted = F
    )
    # And do some renaming (to reduce length of legend)
    shifted_names <- gsub("shifted_","s",names(fcst))
    # If the shift is zero hours, remove this from the model name
    shifted_names <- gsub("_s0h","",shifted_names)
    for (ii in seq(1,length(fcst))) {
      fcst[[ii]][["fcst_model"]] <- shifted_names[ii]
    }
    names(fcst) <- shifted_names
  }
  
  #================================================#
  # COMMON CASE, SCALE, FILTER TO MAX FORECAST
  #================================================#

  fcst <- switch(
    vertical_coordinate,
    "pressure" = harpCore::common_cases(fcst, p),
    "height"   = harpCore::common_cases(fcst, z),
    harpCore::common_cases(fcst)
  )
  
  if (!is.null(prm_info$scale_fcst)) {
    # Check if we need to scale models differently for this parameter
    if (is.null(prm_info$models_to_scale)) {
      cat("Scaling all models using the same scale_fcst\n")
      fcst <- do.call(
        harpCore::scale_param,
        c(list(x = fcst), 
          prm_info$scale_fcst))
    } else {
      for (cm in prm_info$models_to_scale) {
        cat("Scaling the forecast for model",cm,"\n")
        if (!(cm %in% names(fcst))) {
          cat("You are tying to scale model",cm,"but it was not found!\n")
          stop("Exiting!")
        }
        fcst[[cm]] <- do.call(
          harpCore::scale_param,
          c(list(x = fcst[[cm]]), 
            prm_info$scale_fcst))
      }
    }
  }
  
  # Filter forecasts to max value if indicated in the param file
  # For ensembles, all members must be <= the maximum
  fcst <- fcst_fctmax_filter(fcst,
                             fcst_type,
                             prm_info,
                             vertical_coordinate)
  
  #================================================#
  # READ OBS, SCALE, AND JOIN
  #================================================#
  
  obs <- try_rpobs(fcst,
                   prm_name,
                   prm_info,
                   obs_path,
                   vertical_coordinate)

  if (is.null(obs)) {
    warning("No obs found for ",prm_name,", moving on to next parameter")
    return(missing_data)
  }
  if (nrow(obs) < 1) {
    warning("No obs found for ",prm_name,", moving on to next parameter")
    return(missing_data)
  }
  
  if (!is.null(prm_info$scale_obs)) {
    obs <- do.call(
      harpCore::scale_param,
      c(list(x = obs,
             col = {{prm_name}}),
             prm_info$scale_obs)
    )
  }
  
  fcst <- harpCore::join_to_fcst(fcst, obs)
  fcst <- switch(
    vertical_coordinate,
    "pressure" = harpCore::common_cases(fcst, p),
    "height"   = harpCore::common_cases(fcst, z),
    harpCore::common_cases(fcst)
  )
  
  et_read <- Sys.time()
  dt_read <- round(as.numeric(et_read - st_read,units = "secs"))
  cat("Finished the forecast and obs reading\n")

  #================================================#
  # QUALITY CHECKS, ADD VALID_HOUR
  #================================================#
  
  fcst <- fcst_qc(fcst,
                  {{prm_name}},
                  prm_info,
                  vertical_coordinate,
                  num_days)
  
  if (is.null(fcst)) {
    warning("Skipping parameter ",prm_name," as fcst is empty")
    return(missing_data)
  }
  
  # Add valid_hour to fcst
  fcst <- harpCore::expand_date(fcst,valid_dttm)
  fcst <- harpPoint::mutate_list(fcst,
                                 valid_hour = sprintf("%02d",valid_hour))
  
  #================================================#
  # HANDLE CASE OF ONE MEMBER FROM ENSEMBLES
  #================================================#
  
  # Deal with reading one particular member in each ensemble.
  # As such, fcst_type should be changed to "det" instead of "eps", and some
  # renaming needs to be carried out. This is relevant, e.g., when comparing 
  # control members in ensembles (and avoids having to re-convert)
  
  if (fcst_type == "eps") {
    
    # Sum over models and check if only one member exists in each ensemble
    sum_total_members <- 0
    for (cm in names(fcst)) {
      cur_memebrs       <- sum(grepl("mbr0",names(fcst[[cm]]),fixed = TRUE))
      sum_total_members <- sum_total_members + cur_memebrs
    }
    
    if (sum_total_members == length(names(fcst))) {
      cat("This looks like a determinstic comparison of ensemble members!\n")
      cat("Switching to fcst_type = det and renaming models\n")
     
      new_model_name_vec   <- NULL
      fcst                 <- harpCore::as_det(fcst)
      for (cm in names(fcst)) {
        new_model_name             <- paste0(cm,"mbr",
                                             sprintf("%03d",
                                                     members_list[[cm]]))
        new_model_name_vec         <- c(new_model_name_vec,new_model_name)
        fcst[[cm]][["fcst_model"]] <- new_model_name
      }
      names(fcst)          <- new_model_name_vec
      fcst_type            <- "det"
      t_s_str              <- "det_threshold_scores"
      ft_str               <- "det"
    }
  }
  
  #================================================#
  # HANDLE THE STATION GROUPING
  #================================================#
  
  # Dynamic generation of station lists
  if (dynamic_sid_gen){
    cs_list <- fn_station_selection(domain_choice = domains_to_run,
                         param            = prm_name,
                         generate_domains = FALSE,
                         domains_to_gen   = domains_to_run,
                         plot_domains     = plot_dynamic_sid,
                         fcst_object      = fcst,
                         obs_object       = obs,
                         poly_dir         = here::here("verification/poly_files"),
                         multlatlon_rmv   = TRUE)
  } else {
    # Get domains from station_selection
    cs_list <- fn_station_selection(domain_choice = base::setdiff(domains_to_run,"All"),
                                    param       = prm_name,
                                    domain_file = sid_lists_fname)
  }
  
  all_station_groups <- NULL
  
  # Convert into suitable format and add
  avail_domains <- base::intersect(names(cs_list),domains_to_run)
  if (length(avail_domains) > 0) {
    for (ad in avail_domains) {
      cl <- cs_list[[ad]] %>% dplyr::mutate("station_group" = ad) %>% 
        dplyr::select(SID,"station_group")
      all_station_groups <- dplyr::bind_rows(all_station_groups,cl)
    }
  }

  # Extract all SIDS as a group
  if (("All" %in% domains_to_run) & (!("All" %in% unique(all_station_groups$station_group)))) {
    all_sids <- data.frame(harpCore::unique_stations(fcst),"All") %>%
      tibble::as_tibble()
    names(all_sids) <- c("SID","station_group")
    # Filter out stations_to_rmv
    all_sids <- all_sids %>%
      dplyr::filter(!(SID %in% cs_list[["stations_to_rmv"]][["SID"]]))
    all_station_groups <- dplyr::bind_rows(all_station_groups,all_sids)
  }
  
  if (is.null(all_station_groups)) {
    warning("No domains found, skipping parameter ",prm_name)
    return(missing_data)
  } else {
    fcst <- harpCore::join_to_fcst(fcst,all_station_groups,force = TRUE)
    # Make sure something exists after joining
    if (length(fcst[[1]][["SID"]]) == 0) {
      warning("No domain data found after joining, skipping parameter ",prm_name)
      return(missing_data)
    }
  }
  
  # Define a list to store the sc data for this parameter over all domains
  list_scrd_data <- list()

  if (!gen_sc_only) {
    
    #================================================#
    # AUX SCORES
    #================================================#
    
    st_aux <- Sys.time()
    if (is.na(vertical_coordinate) & (create_png)) {
      fn_plot_aux_scores(fcst,
                              plot_output,
                              png_projname = png_projname,
                              rolling_verif = rolling_verif,
                              cmap = cmap,
                              cmap_hex = cmap_hex,
                              lt_split = lt_split,
                              fsd  = fsd,
                              fed  = fed)
    }
    et_aux <- Sys.time()
    dt_aux <- round(as.numeric(et_aux - st_aux,units = "secs"))
    
    #================================================#
    # VERIFICATION FOR DIFFERENT GROUPS
    #================================================#
    
    st_verif <- Sys.time()
    verif_all <- fn_run_verif_groups(fcst = fcst,
                                     prm_name = prm_name,
                                     vertical_coordinate = vertical_coordinate,
                                     fcst_type = fcst_type,
                                     num_ref_members = num_ref_members,
                                     grps_param = grps_param,
                                     grps_threshold = grps_threshold,
                                     grps_SID = grps_SID,
                                     all_station_groups = all_station_groups,
                                     thresholds_param = thresholds_param,
                                     create_png = create_png,
                                     force_valid_thr = force_valid_thr,
                                     t_s_str = t_s_str,
                                     run_sid = !skip_sid_verif)
    et_verif <- Sys.time()
    dt_verif <- round(as.numeric(et_verif - st_verif,units = "secs"))
    verif        <- verif_all$verif
    verif_toplot <- verif_all$verif_toplot
    verif_sid    <- verif_all$verif_sid
    
    #================================================#
    # CALL PLOTTING SCRIPT FOR DIFFERENT GROUPS
    #================================================#
  
    # When plotting, filter out situations where very few cases exist
    # Only do this for UA leadtime scores, generaly not required for surface
    if (rolling_verif) {
      min_cases_filter <- 1
    } else {
      min_cases_filter <- 100
    }
    
    if (prm_name %in% all_possible_UA_vars) {
      verif_toplot <- filter_verif(verif_toplot,
                                   ft_str,
                                   min_cases_filter)
    }
    
    st_plot <- Sys.time()
    if (create_png) {
      
      fn_plot_point_verif(verif_toplot,
                               plot_output,
                               png_projname = png_projname,
                               rolling_verif = rolling_verif,
                               cmap = cmap,
                               map_cbar_d = map_cbar_d,
                               fsd  = fsd,
                               fed  = fed)
      if ((is.na(vertical_coordinate)) & (!is.null(verif_sid))) {
        fn_plot_point_verif(verif_sid,
                                 plot_output,
                                 table_SIDS = FALSE,
                                 png_projname = png_projname,
                                 rolling_verif = rolling_verif,
                                 cmap = cmap,
                                 map_cbar_d = map_cbar_d,
                                 fsd  = fsd,
                                 fed  = fed)
      }
  
    }
    
    # Save the verification object used for plotting pngs if desired
    if (save_vofp) {
      vtpname <- paste(project_name,prm_name,start_date,end_date,sep = "_")
      saveRDS(object = verif_toplot,
              file   = file.path(plot_output,
                                 paste0(vtpname,".rds")))
    }
    
    et_plot <- Sys.time()
    dt_plot <- round(as.numeric(et_plot - st_plot,units = "secs"))
        
    #================================================#
    # SAVE TO RDS
    #================================================#
    
    if (!rolling_verif) {
      
      if (!dir.exists(file.path(verif_path))) {
        dir.create(file.path(verif_path),
                   showWarnings = TRUE,
                   recursive    = FALSE)
      }
      if (mod_def_rds) {
        custom_rds <- "{verif_path}/{projectname}.harpPointVerif.harp.{parameter}.harp.{start_date}-{end_date}.harp.{models}.rds"
        harpIO::save_point_verif(verif,
                                 verif_path          = verif_path,
                                 verif_file_template = custom_rds,
                                 projectname         = project_name)
      } else {
        harpIO::save_point_verif(verif,
                                 verif_path = verif_path)
      }
    }
        
  } else { # if gen_sc_only=TRUE
        
    # Set "verif" output to a dummy output in this case (but not NA)
    verif <- "Skipped due to gen_sc_only"
        
  } # Close the if (!gen_sc_only) condition
      
      
  #================================================#
  # SCORECARDS
  #================================================#
  
  st_scrd <- Sys.time()
  if (create_scrd) {
    
    for (scrd_domain in unique(fcst[[1]][["station_group"]])) {
      
      fcst_scrd      <- filter_list(fcst,station_group == scrd_domain)
      list_scrd_data <- gen_sc(list_scrd_data,
                               fcst_scrd,
                               {{prm_name}},
                               scrd_domain,
                               CONFIG,
                               fcst_type,
                               vertical_coordinate,
                               all_possible_UA_vars,
                               num_ref_members)
      rm(fcst_scrd)
    }
  }
  et_scrd <- Sys.time()
  dt_scrd <- round(as.numeric(et_scrd - st_scrd,units = "secs"))
  
  # Timings
  cat("Timing (s): Reading the data:",dt_read,"\n")
  cat("Timing (s): Aux plotting    :",dt_aux,"\n")
  cat("Timing (s): All verif       :",dt_verif,"\n")
  cat("Timing (s): Plotting        :",dt_plot,"\n")
  cat("Timing (s): Scorecard data  :",dt_scrd,"\n")
  
  # Return the data to the calling environment
  return(list("verif"   = verif,
              "sc_data" = list_scrd_data))
  
}

#================================================#
# CALL VERIF FUNCTION OVER ALL PARAMS
#================================================#

verif <- purrr::imap(params, run_verif)

# Check if any NAs exist in the verif output due to missing data
sc_data_exists <- FALSE
for (jj in names(params)) {
  if (all(is.na(verif[[jj]][["verif"]]))) {
    verif[[jj]] <- NULL # Remove this parameter from the list
  } else {
    # Check if scorecard data exists for at least one parameter
    if (length(verif[[jj]][["sc_data"]]) > 0) {
      sc_data_exists <- TRUE
    }
  }
}

#================================================#
# SCORECARD PLOTTING
#================================================#

# Generate scorecards/diffs if desired
if ((create_scrd) & (sc_data_exists)) {
  
  model_names <- NULL
  score_names <- NULL
  for (ii in seq(1,length(verif))) {
    if (length(verif[[ii]][["sc_data"]]) > 0) {
      # First domain, first tibble (i.e. summary scores)
      qwe         <- unique(verif[[ii]][["sc_data"]][[1]][[1]][["fcst_model"]])
      qwe2        <- unique(verif[[ii]][["sc_data"]][[1]][[1]][["score"]])
      model_names <- unique(c(model_names,qwe))
      score_names <- unique(c(score_names,qwe2))
    }
  }
  sc_models <- c(CONFIG$scorecards$ref_model,
                 CONFIG$scorecards$fcst_model)
  
  # Check fcst/ref model consistency
  for (ii in seq(1,length(sc_models))) {
    scm <- sc_models[ii]
    if (!(scm %in% model_names)) {
      # Look for renamed version
      if (any(grepl(scm,model_names,fixed = TRUE))) {
        scm_renamed <- model_names[grepl(scm,model_names,fixed = TRUE)]
        if (length(scm_renamed) == 1) {
          cat("Using model name",scm_renamed,"instead of",scm,"\n")
          sc_models[ii] <- scm_renamed
        } else {
          stop("Fringe case in scorecard model check, what's going on?")
        }
      } else {
        stop("Model ",scm," was not found in scorecard data, aborting!")
      }
    }
  }
  
  # Check score consistency
  if (all(CONFIG$scorecards$scores %in% score_names)) {
    sc_scores <- CONFIG$scorecards$scores
  } else {
    cat("Switch scorecard scores to default\n")
    if ("mean_bias" %in% score_names) {
      sc_scores <- c("mean_bias","rmse","crps","spread")
    } else {
      sc_scores <- c("bias","rmse","stde")
    }
  }
  
  scard_fname_out <- fn_scorecard_signif(verif,
                                              sc_scores,
                                              CONFIG$scorecards$parameters,
                                              sc_models[2],
                                              sc_models[1],
                                              plot_output,
                                              plot_signif = CONFIG$scorecards$plot_signif,
                                              verif_path = verif_path,
                                              png_projname = png_projname,
                                              fsd  = fsd,
                                              fed  = fed)
  # Plot the "tile" scorecards
  if (!is.na(scard_fname_out)) {
    if (file.exists(scard_fname_out)) {
      sc_data <- readRDS(scard_fname_out)
      fn_plot_tile_scorecard(sc_data,
                             sc_scores,
                             sc_models[2],
                             sc_models[1],
                             plot_output,
                             significance = 0.95,
                             png_projname = png_projname,
                             leadtimes = seq(3,66,3),
                             fsd  = fsd,
                             fed  = fed)
    }
  }
}

cat("%%%%%%%%%%%% point_verif: Finished config file",config_file,"%%%%%%%%%%%%\n")
