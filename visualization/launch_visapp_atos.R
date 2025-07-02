#!/usr/bin/env Rscript

library(here)
library(shiny)
library(argparse)

# Capture command-line arguments
if (!interactive()) {
  
  parser <- argparse::ArgumentParser()
  parser$add_argument("-app_dir",
                      type    = "character",
                      default = here('visualization/visapp'),
                      help    = "visapp directory on your system. [Default %(default)s]",
                      metavar = "String")
  parser$add_argument("-img_dir",
                      type    = "character",
                      default = here('visualization/visapp/sample_images'),
                      help    = "Directory containing verif images on your system. [Default %(default)s]",
                      metavar = "String")
  parser$add_argument("-port",
                      type    = "character",
                      default = "9999",
                      help    = "Port number for shiny app. [Default %(default)s]",
                      metavar = "String")
  parser$add_argument("-smr_ind",
                      type    = "logical",
                      default = FALSE,
                      help    = "Use Seasonal/Monthly/Rolling structure. [Default %(default)s]",
                      metavar = "Boolean")
  parser$add_argument("-panel_ind",
                      type    = "logical",
                      default = FALSE,
                      help    = "Dsiplay panelification tab. [Default %(default)s]",
                      metavar = "Boolean")
  
  args      <- parser$parse_args()
  app_dir   <- args$app_dir
  img_dir   <- args$img_dir
  port      <- args$port
  smr_ind   <- args$smr_ind
  panel_ind <- args$panel_ind
  
  # Always print help
  cat('\n')
  cat('========================================================================\n')
  cat('Start of help\n')
  cat('\n')
  parser$print_help()
  
} else {
  
  app_dir <- here('visualization/visapp')
  img_dir <- here('visualization/visapp/sample_images')
  port    <- "9999"
  smr_ind <- FALSE
  panel_ind <- FALSE
  
}

# Define port and host
shinyport <- port
host      <- Sys.getenv("HOST")

# Display information for setting up the Shiny app
cat('\n')
cat('To display your verification figures in the visapp in a Firefox window at ATOS:\n')
cat('\n')
cat('1: Provide the path to your directory containing verification images as argument "-img_dir" to this script.\n')
cat('2: Open a new terminal.\n')
cat(paste0('3: Execute this command: ssh -L ',shinyport,':localhost:',shinyport,' ', host),"\n")
cat(paste0('4: Open a Firefox window and go to http://127.0.0.1:', shinyport,'/'),"\n")
cat('\n')
cat('End of help\n')
cat('========================================================================\n')
cat('\n')

# Set shinyOptions
shiny::shinyOptions(app_dir = app_dir,
                    img_dir = img_dir,
                    smr_ind = smr_ind,
                    panelification_ind = panel_ind)
# Run the Shiny app
runApp(file.path(app_dir,"app.R"), port = strtoi(shinyport))
