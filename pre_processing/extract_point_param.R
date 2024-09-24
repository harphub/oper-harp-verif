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
  library(meteogrid)
})
source(here::here("verification/fn_verif_helpers.R"))

#=====================================================#
# FUNCTIONS TO READ DATA AND INTERPOLATE
#=====================================================#

# Function for reading grib (single dtg assumed)
read_det_grib_data <- function(dtg,
                               fcst_model,
                               param,
                               param_opts,  
                               lt,
                               stations,
                               interp_method,
                               file_path,
                               file_format,
                               file_template,
                               get_dextent=FALSE){
  
  # If getting the domain extent, just read the geofield and return
  if (get_dextent) {
    
    cat("Getting the domain extent...\n")
    df  <- harpIO::read_forecast(
      dttm                = harpCore::seq_dttm(start_dttm = dtg,
                                               end_dttm   = dtg),
      fcst_model          = fcst_model,
      parameter           = param,
      lead_time           = lt,
      file_path           = file_path,
      file_format         = file_format,
      file_format_opts    = param_opts,
      file_template       = file_template,
      return_data         = TRUE
    )
    if (!is.null(df)) {
      if ("fcst" %in% names(df)) {
        r_c_name <- "fcst"
      } else {
        r_c_name <- names(df)[grepl(fcst_model,names(df),fixed = TRUE)]
      }
      de <- meteogrid::DomainExtent(df[[r_c_name]][[1]])
      return(de)
    } else {
      cat("Trying to get the domain extent but no geofield found!\n")
      cat("Does the file and parameter exist?\n")
      return(NULL)
    } 
    
  }
  
  # Different methods depending on upscaling
  if (ups_radius > 0) {
    
    # Loop over all stations (slow for a long file!)
    df <- NULL
    for (cs in seq(1,nrow(stations))) {
      
      # Subgrid centered on nearest gridpoint to the station
      dfc  <- harpIO::read_forecast(
        dttm                = harpCore::seq_dttm(start_dttm = dtg,
                                                 end_dttm   = dtg),
        fcst_model          = fcst_model,
        parameter           = param,
        lead_time           = lt,
        transformation      = "subgrid",
        transformation_opts = harpIO::subgrid_opts(
          stations$i[cs] - ups_radius,
          stations$i[cs] + ups_radius,
          stations$j[cs] - ups_radius,
          stations$j[cs] + ups_radius
        ),
        file_path           = file_path,
        file_format         = file_format,
        file_format_opts    = param_opts,
        file_template       = file_template,
        return_data         = TRUE
      )
      if (nrow(dfc) != 1) {
        cat("Something went wrong during the upscaling, return NULL!\n")
        df <- NULL
        return(df)
      }
      
      # Rename to "forecast"
      if ("fcst" %in% names(dfc)) {
        r_c_name <- "fcst"
      } else {
        r_c_name <- names(dfc)[grepl(fcst_model,names(dfc),fixed = TRUE)]
      }
      dfc <- dfc %>% dplyr::mutate(forecast = get(r_c_name)) %>% 
        dplyr::select(-all_of(r_c_name))
      # Then upscale
      dfc$forecast <- get(ups_method)(dfc$forecast[[1]])
      # Add in SID, lat, lon info
      dfc$SID <- stations$SID[cs]
      dfc$lat <- stations$lat[cs]
      dfc$lon <- stations$lon[cs]
      # Bind to total
      df <- bind_rows(df,dfc)
      
    }
    
    # Reorder
    df <- df %>% select("fcst_model",
                        fcst_dttm,
                        lead_time,
                        parameter,
                        valid_dttm,
                        step_range,
                        level_type,
                        level,
                        units,
                        SID,
                        lat,
                        lon,
                        fcst_cycle,
                        forecast)
    
  } else {
    
    df  <- harpIO::read_forecast(
      dttm                = harpCore::seq_dttm(start_dttm = dtg,
                                               end_dttm   = dtg),
      fcst_model          = fcst_model,
      parameter           = param,
      lead_time           = lt,
      transformation      = "interpolate",
      transformation_opts = harpIO::interpolate_opts(
        stations          = stations,
        method            = interp_method,
        correct_t2m       = correct_T2m
      ),
      file_path           = file_path,
      file_format         = file_format,
      file_format_opts    = param_opts,
      file_template       = file_template,
      return_data         = TRUE
    ) %>% harpCore::bind()
    if (nrow(df) == 0) {
      cat("Something went wrong for",dtg,"+",lt,"\n")
      cat("Probably the file does not exist or there was a problem finding the param\n")
      df <- NULL
      return(df)
    }
    
    if ("fcst" %in% names(df)) {
      r_c_name <- "fcst"
    } else {
      r_c_name <- names(df)[grepl(fcst_model,names(df),fixed = TRUE)]
    }
    df <- df %>% dplyr::mutate(forecast = get(r_c_name)) %>% 
      dplyr::select(-all_of(r_c_name))
    
  }
  
  if (nrow(df) == 0) {
    df <- NULL
  }
  
  return(df)
}

# Using read_forecast is not working for fa data, use read_grid instead
read_det_icm_data <- function(dtg,
                              fcst_model,
                              param,
                              param_opts,
                              lt,
                              stations,
                              interp_method,
                              file_path,
                              file_format,
                              file_template,
                              get_dextent=FALSE){
  
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
  
  if (get_dextent) {
    
    cat("Getting the domain extent...\n")
    try_df  <- harpIO::read_grid(
      file_name           = file.path(file_path,file_template),
      parameter           = param,
    ) 
    if (exists("try_df")) {
      de <- meteogrid::DomainExtent(try_df)
      rm(try_df)
      return(de)
    } else {
      cat("Trying to get the domain extent but no geofield found!\n")
      cat("Does the file and parameter exist?\n")
      return(NULL)
    } 
    
  }
  
  try_df  <- harpIO::read_grid(
    file_name           = file.path(file_path,file_template),
    parameter           = param,
    transformation      = "interpolate",
    transformation_opts = harpIO::interpolate_opts(
      stations          = stations,
      method            = interp_method,
      correct_t2m       = correct_T2m
    )
  ) 
  
  if (!exists("try_df")) {
    df <- NULL
  } else {
    # Convert to same format as from read_forecast
    df <- try_df %>% dplyr::mutate(forecast     = station_data,
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
    rm(try_df)
  }
  
  return(df)
  
}

#================================================#
# READ COMMAND LINE ARGUMENTS
#================================================#

if (!interactive()) {
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
  # Note on "param" name: For grib files, you may also need to add the optional 
  # shortName, level, and level_type values below. For ICM files, param should 
  # be the CLS name. Note that "param" is used in the output filenames, so it
  # should be unique! E.g. for v at 50m above ground, you could use 
  # param="v50m", shortName="v", level=50, and level_type="heightAboveGround"
  parser$add_argument("-param",
                      type    = "character",
                      default = "None",
                      help    = "What parameter to extract? See notes above!")
  parser$add_argument("-shortName",
                      type    = "character",
                      default = NULL,
                      help    = "Specify the shortName for param")
  parser$add_argument("-level",
                      type    = "character",
                      default = NULL,
                      help    = "Specify the level for param")
  parser$add_argument("-level_type",
                      type    = "character",
                      default = NULL,
                      help    = "Specify the level_type for param.")
  parser$add_argument("-deaccum",
                      type    = "logical",
                      default = FALSE,
                      help    = "Deaccumulate over the last hour?")
  parser$add_argument("-convert_sl",
                      type    = "logical",
                      default = FALSE,
                      help    = "Flag to convert the input stationlist to a
                                 suitable format for harp")
  parser$add_argument("-ups_radius",
                      type    = "integer",
                      default = 0,
                      help    = "If > 0, upscale over 2*r+1 box")
  parser$add_argument("-ups_method",
                      type    = "character",
                      default = "Mean",
                      help    = "How to upscale over the boc i.e. mean, 
                                 median, or max")
  
  args           <- parser$parse_args()
  start_date     <- args$start_date
  end_date       <- args$end_date
  config_file    <- args$config_file
  param          <- args$param
  shortName      <- args$shortName
  level          <- args$level
  level_type     <- args$level_type
  deaccum        <- args$deaccum
  convert_sl     <- args$convert_sl
  convert_sl     <- args$convert_sl
  ups_radius     <- args$ups_radius
  ups_method     <- args$ups_method
  
} else {
  
  # Source options from here instead of CLI
  cat("Assuming we are in debug mode!\n")
  if (file.exists(here::here("pre_processing/source_extract_options.R"))) {
    source(here::here("pre_processing/source_extract_options.R"))
  } else {
    stop("Create a file to source in extract options\n")
  }
  
}

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
correct_T2m   <- CONFIG$correct_T2m

# Check file format
if (file_format == "grib") {
  read_fn <- "read_det_grib_data"
} else if (file_format == "fa") {
  read_fn <- "read_det_icm_data"
} else {
  stop("File format ",file_format," not considered, exiting!")
}

# Check correct_T2m
if (is.null(correct_T2m)) {
  cat("No correct_T2m found in config, assumed TRUE\n")
  correct_T2m <- TRUE
}

# Check if grib shortName etc has been specified 
if ((!is.null(shortName)) & (!is.null(level)) & (!is.null(level_type)) &
    (file_format == "grib")) {
  param_find <- list(use_grib_shortName(shortName))
  level_find <- list(use_grib_typeOfLevel(level_type,level = level))
  names(param_find) <- param
  names(level_find) <- param
  param_opts    = grib_opts(
    param_find = param_find,
    level_find = level_find
  )
} else {
  param_opts <- list()
}
cat("Try to read",param,"from the grib/fa files using:\n")
print(param_opts)

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
  all_stations <- readRDS(station_file)
} else {
  if (is.null(station_file)) {
    cat("No station file is in the config file and convert_sl is switched on\n")
    cat("Defaulting to harp's sttion_list")
    all_stations <- harpIO::station_list
  } else {
    cat("Assuming",station_file,"is a Harmonie allsynop list\n")
    all_stations <- conv_allsynop(station_file)
  }
}

# Check that stations is compatible with upscaling flags
if (ups_radius > 0) {
  if (ups_radius %% 1 == 0) {
    box_size = 2*ups_radius + 1
    if (ups_method %in% c("mean","median","max")) {
      cat("Request to upscale over",box_size,"x",box_size,"box using",ups_method,"method\n")
      if (("i" %in% names(all_stations)) & ("j" %in% names(all_stations))) {
        cat("Nearest point index is in stations file, continue\n")
        if (file_format != "grib") {
          stop("TODO: Upscaling only implemented for grib currently\n")
        }
      } else {
        stop("For upscaling, nearest point index for each station is required!\n")
      }
    } else {
      stop("Upscale method should be mean, median, or max!\n")
    }
  } else {
    stop("Upscaling radius should be a natural number!\n")
  }
}

#=====================================================#
# READ DATA AND INTERPOLATE
# LOOP OVER MODELS AND DTG
#=====================================================#

# Construct DTGs to consider
dtg_vec <- substr(harpIO::seq_dates(start_date,end_date,by = by_val),0,10)

# Loop over models
for (fcst_model in fcst_models) {
  
  # Try to extract the domain for this model - just look at the first dtg and try
  # each lead time. Once a file is found, break from the loop
  for (lt in lead_times) {
    de <- do.call(
      get(read_fn),
      list(dtg           = dtg_vec[1],
           fcst_model    = fcst_model,
           param         = param,
           param_opts    = param_opts,
           lt            = lt,
           stations      = all_stations,
           interp_method = interp_method,
           file_path     = file_path,
           file_format   = file_format,
           file_template = file_template,
           get_dextent   = TRUE)
    )
    if (!is.null(de)) {
      cat("Filtering stations to the domain extent\n")
      stations    <- all_stations %>% filter(
        dplyr::between(lat,de$latlim[1],de$latlim[2]),
        dplyr::between(lon,de$lonlim[1],de$lonlim[2]),
      )
      break
    } 
    if (lt == tail(lead_times,1)) {
      cat("No station filtering took place as no data could be found for",dtg,"\n")
      stations <- all_stations
    }
  }
  
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
                param_opts    = param_opts,
                lt            = lt,
                stations      = stations,
                interp_method = interp_method,
                file_path     = file_path,
                file_format   = file_format,
                file_template = file_template)
         )
        },
        error = function(cond){
          cat("An error was detected when reading the data\n")
          cat("Here is the original message:\n")
          message(conditionMessage(cond))
          return(NULL)
        },
        finally = {
          cat("\n")
        }
      )
      
      # Check that forecast only contains one output. Mulutple output can occur
      # if the same param is found twice
      if (!is.null(tdf_1)) {
        if (length(tdf_1$forecast[[1]]) > 1) {
          cat("Multiple parameters found for",param,"in grib, you need to specify!\n")
          stop("Aborting as param requested is not clear")
        }
      }
      
      if ((deaccum) & (lt != 0)) {
        lt_m  <- lt - 1
        tdf_2 <- tryCatch(
          {
            do.call(
              get(read_fn),
              list(dtg           = dtg,
                   fcst_model    = fcst_model,
                   param         = param,
                   param_opts    = param_opts,
                   lt            = lt_m,
                   stations      = stations,
                   interp_method = interp_method,
                   file_path     = file_path,
                   file_format   = file_format,
                   file_template = file_template)
            )
          },
          error = function(cond){
            cat("An error was detected when reading the data\n")
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
    
    # Remove names attribute from parameter in df
    if (!is.null(names(df$parameter))) {
      qwe  <- unique(names(df$parameter))
      qwe2 <- unique(df$parameter)
      if (qwe == qwe2) {
        df$parameter <- qwe 
      } else {
        stop("Someting weird is happening with the parameter name, abort!")
      }
    }
    
    #=====================================================#
    # SAVE FOR EACH DTG
    #=====================================================#
    
    if (deaccum) {
      t_str <- "1hr"
    } else {
      # List accumulated parameters here!
      if (param %in% c("grad")) {
        t_str <- "acc"
      } else {
        t_str <- "ins"
      }
    }
    
    if (ups_radius > 0) {
      out_name <- paste0(paste(param,t_str,paste0(ups_method,ups_radius),
                               fcst_model,dtg,sep = "_"),".rds")
    } else {
      out_name <- paste0(paste(param,t_str,fcst_model,dtg,sep = "_"),".rds")
    }
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
