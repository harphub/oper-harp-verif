#!/usr/bin/env Rscript

#================================================#
# READ VFLD DATA AND CONVERT TO SQLITE FORMAT
#================================================#

#================================================#
# PACKAGES
#================================================#

suppressPackageStartupMessages({
  library(harp)
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
                    help    = "First date to process [default %(default)s]",
                    metavar = "String")
parser$add_argument("-end_date",
                    type    = "character",
                    default = "None",
                    help    = "Last date to process [default %(default)s]",
                    metavar = "String")
parser$add_argument("-config_file",
                    type    = "character",
                    default = "None",
                    help    = "Config file to use [default %(default)s]",
                    metavar = "String")
parser$add_argument("-use_custom_asl",
                    type    = "logical",
                    default = FALSE,
                    help    = "Flag to use a specific Harmonie allsynop list when
                               converting the vfld data [default %(default)s]",
                    metavar = "Boolean")
parser$add_argument("-remove_m_elev",
                    type    = "logical",
                    default = FALSE,
                    help    = "Flag to remove model elevation from output sqlite
                               files. Useful when memebrs have different model elevations
                               e.g. multi-model ensembles [default %(default)s]",
                    metavar = "Boolean")

args           <- parser$parse_args()
start_date     <- args$start_date
end_date       <- args$end_date
config_file    <- args$config_file
use_custom_asl <- args$use_custom_asl
remove_m_elev  <- args$remove_m_elev

#================================================#
# READ OPTIONS FROM THE CONFIG FILE
#================================================#

CONFIG        <- yaml::yaml.load_file(here::here(config_file))
vfld_path     <- CONFIG$pre$vfld_path
fcst_model    <- CONFIG$pre$fcst_model
file_template <- CONFIG$pre$vfld_template
params        <- CONFIG$pre$params
by_val        <- CONFIG$pre$vfld_by
fcst_path     <- CONFIG$verif$fcst_path

# Create list of members (required when generating tables for eps experiments)
members_list  <- get_named_list(CONFIG$pre$members,fcst_model,"members")

# Create a list of lags (possibly required when generating tables for eps experiments)
lags_list     <- get_named_list(CONFIG$pre$lags,fcst_model,"lags")

# If lead_time is defined in CONFIG$pre, use that instead of CONFIG$verif
lead_time_str <- CONFIG$pre$lead_time
if (is.null(lead_time_str)) {
  cat("Using CONFIG$verif for the lead_times to convert\n")
  lead_time_str <- CONFIG$verif$lead_time
} else {
  cat("Using CONFIG$pre for the lead_times to convert\n")
}
lead_time <- eval(parse(text = lead_time_str))

#================================================#
# OPTION CHECKS
#================================================#

# Create a list of file_templates if required
if (length(file_template) > 1) {
  if (length(file_template) == length(fcst_model)) {
    file_template <- stats::setNames(as.list(file_template),fcst_model)
  } else {
    stop("Error: pre:vfld_template is neither of length 1 or the same length as fcst_model, aborting.")
  }
}

# If a custom station list has been specified, use it!
if (use_custom_asl) {
  # Look for the staionlist path in the config file
  custom_asl_path <- CONFIG$pre$custom_asl_path
  if (is.null(custom_asl_path)) {
    cat("The allsynop list path is not specified in the config file, using harp default\n")
    stations_rf <- harpCore::station_list
  } else {
    cat("Using allsynop list",custom_asl_path,"for vfld conversion\n")
    # Convert the allsynop list into a suitable format for harp
    stations_rf <- conv_allsynop(custom_asl_path)
  }
} else {
  cat("Use harp's default station list\n")
  stations_rf <- harpCore::station_list
}

if (remove_m_elev) {
  cat("Do not include model_elevation in the sqlite files\n")
}
  
# If it is a deterministic experiment, reset members_list to just NULL (this
# avoids a crash...)
if (length(CONFIG$pre$members) == 1) {
  if (is.null(CONFIG$pre$members[[1]])) {
    members_list <- NULL
  }
}

#================================================#
# VFLD CONVERT
#================================================#

if (is.null(params[[1]])) {
  params <- NULL
  cat("<<<<<< PROCESSING ALL PARAMETERS for",fcst_model,">>>>>>>\n")
} else {
  cat("<<<<<< PROCESSING PARAMETERS",params,"for",fcst_model,">>>>>>>\n")
}

harpIO::read_forecast(
  dttm                = harpCore::seq_dttm(start_date,end_date,by = by_val),
  fcst_model          = fcst_model,
  parameter           = params,
  lead_time           = lead_time,
  members             = members_list,
  lags                = lags_list,
  file_path           = vfld_path,
  file_template       = file_template,
  output_file_opts    = harpIO::sqlite_opts(path = fcst_path,
                                            remove_model_elev = remove_m_elev),
  transformation_opts = harpIO::interpolate_opts(correct_t2m = TRUE,
                                                 stations = stations_rf),
  return_data         = FALSE
)

cat("Finished vfld conversion\n")
