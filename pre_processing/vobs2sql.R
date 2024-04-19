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

#================================================#
# READ OPTIONS FROM THE CONFIG FILE
#================================================#

CONFIG     <- yaml::yaml.load_file(here::here(config_file))
vobs_path  <- CONFIG$pre$vobs_path
obs_path   <- CONFIG$verif$obs_path
by_val     <- CONFIG$pre$vobs_by

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
