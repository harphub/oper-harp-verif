#!/usr/bin/env Rscript


#================================================#
# EXTRACT POINT DATA FROM GRIB OR ICM AND SAVE
# TO RDS. TO BE USED FOR NON-CONVENTIONAL FIELDS
# WHICH ARE NOT AVAILABLE IN VFLD.
#
# ONLY WORKS FOR READING DETERMINISTIC MODELS.
# TO GET A CERTAIN MEMBER OF AN ENSEMBLE, USE
# fcst_model=EXPmbrMMM TO EXTRACT
#================================================#


#================================================#
# PACKAGES
#================================================#

suppressPackageStartupMessages({
  library(harp)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(argparse)
  library(here)
  library(yaml)
})
source(here::here("verification/fn_verif_helpers.R"))

#================================================#
# READ COMMAND LINE ARGUMENTS
#================================================#

parser <- argparse::ArgumentParser()
parser$add_argument("-start_date",
                    type    = "character",
                    default = "None",
                    help    = "First date to process in YYYYMMDDHH format")
parser$add_argument("-end_date",
                    type    = "character",
                    default = "None",
                    help    = "Final date to process in YYYYMMDDHH format")
parser$add_argument("-config_file",
                    type    = "character",
                    default = "None",
                    help    = "Config file to use")
parser$add_argument("-param",
                    type    = "character",
                    default = "None",
                    help    = "What parameter to extract")
parser$add_argument("-deaccum",
                    type    = "logical",
                    default = FALSE,
                    help    = "Deaccumulate over the last hour?")
parser$add_argument("-convert_sl",
                    type    = "logical",
                    default = FALSE,
                    help    = "Flag to convert the input stationlist to a
                               suitable format for harp")

args           <- parser$parse_args()
start_date     <- args$start_date
end_date       <- args$end_date
config_file    <- args$config_file
param          <- args$param
deaccum        <- args$deaccum
convert_sl     <- args$convert_sl

#================================================#
# READ OPTIONS FROM THE CONFIG FILE
#================================================#

CONFIG        <- yaml::yaml.load_file(here::here(config_file))
fcst_models   <- CONFIG$fcst_models
file_path     <- CONFIG$file_path
file_template <- CONFIG$file_template
file_format   <- CONFIG$file_format
lead_time_str <- CONFIG$lead_times
lead_times    <- eval(parse(text = lead_time_str))
by_val        <- CONFIG$by_val
station_file  <- CONFIG$station_file
interp_method <- CONFIG$interp_method
out_path      <- CONFIG$out_path

# Check file format
if (file_format == "grib") {
  read_fn <- "read_det_grib_data"
} else if (file_format == "fa") {
  read_fn <- "read_det_icm_data"
} else {
  stop("File format ",file_format," not considered, exiting!")
}

#=====================================================#
# GET THE STATION LIST
#=====================================================#

# If convert_sl is switched off, then we assume the specified station_file
# is already in a suitable format for harp, e.g.:
#
# stations <- data.frame(
# SID  = 1,
# lat  = 2,
# lon  = 3
# )
#
# If convert_sl is switched on, there are two possibilities:
# 1) Convert the station_file to a suitable format
# 2) If station_file is not specified in the config, use Harp's default
#    station list

if (!convert_sl) {
  cat("Using station list",station_file,"\n")
  stations <- station_file
} else {
  if (is.null(station_file)) {
    cat("No station file is in the config file and convert_sl is switched on\n")
    cat("Defaulting to harp's sttion_list")
    stations <- harpIO::station_list
  } else {
    cat("Assuming",station_file,"is a Harmonie allsynop list\n")
    stations <- conv_allsynop(station_file)
  }
}

#=====================================================#
# READ DATA AND INTERPOLATE
#=====================================================#

# Function for reading grib (single dtg assumed)
read_det_grib_data <- function(dtg,
                               fcst_model,
                               param,
                               lt,
                               stations,
                               interp_method,
                               file_path,
                               file_format,
                               file_template){
  
  df  <- harpIO::read_forecast(
    dttm                = harpCore::seq_dttm(start_dttm = dtg,
                                             end_dttm   = dtg),
    fcst_model          = fcst_model,
    parameter           = harpIO::as_harp_parameter(param),
    lead_time           = lt,
    transformation      = "interpolate",
    transformation_opts = harpIO::interpolate_opts(
      stations          = stations,
      method            = interp_method,
      correct_t2m       = TRUE
    ),
    file_path           = file_path,
    file_format         = file_format,
    file_template       = file_template,
    return_data         = TRUE
  ) %>% harpCore::bind()
  
  r_c_name <- names(df)[grepl(fcst_model,names(df),fixed = TRUE)]
  df <- df %>% dplyr::mutate(forecast = get(r_c_name)) %>% 
    dplyr::select(-all_of(r_c_name))
  
  if (nrow(df) == 0) {
    df <- NULL
  }
  
  return(df)
}

# Using read_forecast is not working for fa data, use read_grid instead
read_det_icm_data <- function(dtg,
                              fcst_model,
                              param,
                              lt,
                              stations,
                              interp_method,
                              file_path,
                              file_format,
                              file_template){
  
  # Extract date info
  yyyy = substr(dtg,1,4)
  mm   = substr(dtg,5,6)
  dd   = substr(dtg,7,8)
  hh   = substr(dtg,9,10)
  
  # Extract member info from fcst_model
  mbr <- ""
  if (grepl("mbr0",fcst_model,fixed = TRUE)) {
    mbr <- substr(fcst_model,nchar(fcst_model) - 5,nchar(fcst_model))
    fcst_model <- substr(fcst_model,1,nchar(fcst_model) - 6)
  }
  
  # Leadtime format
  lt_pad3 <- sprintf("%03d",lt)
  
  # Do some substitutions for the ICM name
  file_path <- gsub("{YYYY}",yyyy,file_path,fixed = TRUE)
  file_path <- gsub("{MM}",mm,file_path,fixed = TRUE)
  file_path <- gsub("{DD}",dd,file_path,fixed = TRUE)
  file_path <- gsub("{HH}",hh,file_path,fixed = TRUE)
  file_path <- gsub("{fcst_model}",fcst_model,file_path,fixed = TRUE)
  file_path <- gsub("{MBR3}",mbr,file_path,fixed = TRUE)
  
  file_template <- gsub("{YYYY}",yyyy,file_template,fixed = TRUE)
  file_template <- gsub("{MM}",mm,file_template,fixed = TRUE)
  file_template <- gsub("{DD}",dd,file_template,fixed = TRUE)
  file_template <- gsub("{HH}",hh,file_template,fixed = TRUE)
  file_template <- gsub("{fcst_model}",fcst_model,file_template,fixed = TRUE)
  file_template <- gsub("{MBR3}",mbr,file_template,fixed = TRUE)
  file_template <- gsub("{LDT3}",lt_pad3,file_template,fixed = TRUE)
  
  df  <- harpIO::read_grid(
    file_name           = file.path(file_path,file_template),
    parameter           = harpIO::as_harp_parameter(param),
    transformation      = "interpolate",
    transformation_opts = harpIO::interpolate_opts(
      stations          = stations,
      method            = interp_method,
      correct_t2m       = TRUE
    )
  ) 
  
  if (nrow(df) == 0) {
    df <- NULL
  } else {
    # Convert to same format as from read_forecast
    df <- df %>% dplyr::mutate(forecast     = station_data,
                               "fcst_model" = fcst_model,
                               fcst_cycle   = hh) %>%
      dplyr::select(-station_data) %>%
      dplyr::select("fcst_model",
                    fcdate,
                    lead_time,
                    parameter,
                    validdate,
                    level_type,
                    level,
                    units,
                    SID,
                    lat,
                    lon,
                    fcst_cycle,
                    forecast)
  }
  
  return(df)
  
}

#=====================================================#
# LOOP OVER MODELS AND DTG
#=====================================================#

# Construct DTGs to consider
dtg_vec <- substr(harpIO::seq_dates(start_date,end_date,by = by_val),0,10)

# Loop over models
for (fcst_model in fcst_models) {
  
  # Loop over dtgs
  for (dtg in dtg_vec) {
    
    df   <- NULL
    YYYY <- substr(dtg,1,4)
    MM   <- substr(dtg,5,6)
    
    for (lt in lead_times) {
      
      tdf_1 <- tryCatch(
        {
         do.call(
           get(read_fn),
           list(dtg           = dtg,
                fcst_model    = fcst_model,
                param         = param,
                lt            = lt,
                stations      = stations,
                interp_method = interp_method,
                file_path     = file_path,
                file_format   = file_format,
                file_template = file_template)
         )
        },
        error = function(cond){
          cat("An error was deteceted when reading the data\n")
          cat("Here is the original message:\n")
          message(conditionMessage(cond))
          return(NULL)
        },
        finally = {
          cat("\n")
        }
      )
      
      if ((deaccum) & (lt != 0)) {
        lt_m  <- lt - 1
        tdf_2 <- tryCatch(
          {
            do.call(
              get(read_fn),
              list(dtg           = dtg,
                   fcst_model    = fcst_model,
                   param         = param,
                   lt            = lt_m,
                   stations      = stations,
                   interp_method = interp_method,
                   file_path     = file_path,
                   file_format   = file_format,
                   file_template = file_template)
            )
          },
          error = function(cond){
            cat("An error was deteceted when reading the data\n")
            cat("Here is the original message:\n")
            message(conditionMessage(cond))
            return(NULL)
          },
          finally = {
            cat("\n")
          }
        )
        if ((!is.null(tdf_1)) & (!is.null(tdf_2))) {
          tdf          <- tdf_1
          tdf$forecast <- tdf_1$forecast - tdf_2$forecast
          df           <- dplyr::bind_rows(df,tdf)
        }
      } else {
        if (!is.null(tdf_1)) {
          df <- dplyr::bind_rows(df,tdf_1)
        }
      } # Deaccum
    } # Lt
    
    if ("sub_model" %in% names(df)) {
      df <- df %>% dplyr::select(-sub_model)
    }
    if ("member" %in% names(df)) {
      df <- df %>% dplyr::select(-member)
    }
    
    # Convert units for grad
    if ((param == "grad") & (deaccum) & (!is.null(df))) {
      df <- df %>% dplyr::mutate(forecast = forecast/3600,units = "W m**-2")
    }
    
    #=====================================================#
    # SAVE FOR EACH DTG
    #=====================================================#
    
    if (deaccum) {
      t_str <- "1hr"
    } else {
      t_str <- "acc"
    }
    
    out_name <- paste0(paste(param,t_str,fcst_model,dtg,sep = "_"),".rds")
    if (dir.exists(out_path)) {
      out_path_full <- file.path(out_path,fcst_model,YYYY,MM)
      if (!dir.exists(out_path_full)) {
        dir.create(out_path_full,showWarnings = TRUE,recursive = TRUE)
      }
      if (!is.null(df)) {
        saveRDS(df,file = file.path(out_path_full,out_name))
      }
    } else {
      stop("Cannot find ",out_path,", aborting!")
    }
  
  } # DTG
  
} # Models
