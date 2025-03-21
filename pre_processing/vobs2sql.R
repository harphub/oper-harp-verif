#!/usr/bin/env Rscript

#================================================#
# READ VOBS DATA AND CONVERT TO SQLITE FORMAT
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

#================================================#
# READ COMMAND LINE ARGUMENTS
#================================================#

if (!interactive()) {
  
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
  
  args        <- parser$parse_args()
  start_date  <- args$start_date
  end_date    <- args$end_date
  config_file <- args$config_file

} else {
  
  start_date     <- "2025010100"
  end_date       <- "2025010123"
  config_file    <- "config_files/config_det_example.yml"
  
}

# Check if required arguments are missing
if (any(c(start_date,end_date,config_file) == "None")) {
  stop("You need to provide start_date, end_date, and a config file")
}

# Check if config_file exists
if (!file.exists(here::here(config_file))) {
  stop("Cannot find config file",here::here(config_file))
}

#================================================#
# READ OPTIONS FROM THE CONFIG FILE
#================================================#

CONFIG     <- yaml::yaml.load_file(here::here(config_file))
vobs_path  <- check_config_input(CONFIG,"pre","vobs_path")
obs_path   <- check_config_input(CONFIG,"verif","obs_path")
by_val     <- check_config_input(CONFIG,"pre","vobs_by")

#================================================#
# OPTION CHECKS
#================================================#

check_dirs_exist(c(vobs_path,obs_path))

if (!(by_val %in% paste0(seq(0,200),"h"))) {
  cat("vobs_by =",by_val,"is not valid - use e.g. 1h\n")
  stop("Aborting")
}

#================================================#
# VOBS CONVERT
#================================================#

cat("Collecting vobs data from",start_date,"to",end_date,"\n")

obs_data <- harpIO::read_obs(
  dttm               = harpCore::seq_dttm(start_date,end_date,by = by_val),
  file_path          = vobs_path,
  output_format_opts = harpIO::obstable_opts(path = obs_path)
)

cat("Finished vobs conversion\n")
