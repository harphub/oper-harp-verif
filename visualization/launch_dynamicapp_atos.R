#!/usr/bin/env Rscript

library(here)
library(shiny)

# Function to display help message
display_help <- function() {
  cat("Usage: script_name [-h | --help] <folder_path> [port]\n")
  cat("\nArguments:\n")
  cat("  -h, --help       Show this help message and exit.\n")
  cat("  folder_path      Mandatory. Path to the folder containing subfolders with verifications.\n")
  cat("  port             Optional. Port number to use for hosting the Shiny app (default: 9999).\n")
  quit(status = 0)
}

# Capture command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if help flag is present or insufficient arguments are provided
if ("-h" %in% args || "--help" %in% args || length(args) < 1) {
  display_help()
}

# Define the mandatory folder path and optional port number
folder_path <- args[1]
shinyport <- ifelse(length(args) >= 2, args[2], "9999")
host <- Sys.getenv("HOST")

# Display information for setting up the Shiny app
print('To display the Shiny app in a Firefox window at ATOS:')
print('1: Open a new terminal.')
print(paste0('2: Execute this command: ssh -L ', shinyport, ':localhost:', shinyport, ' ', host))
print(paste0('3: Open a Firefox window and go to http://127.0.0.1:', shinyport, '/'))

# Run the Shiny app
app_dir <- system.file("shiny_apps/plot_point_verif", package = "harpVis")
shiny::shinyOptions(app_start_dir = folder_path, online = TRUE, full_dir_navigation = TRUE, theme = "light")
shiny::runApp(app_dir, port = strtoi(shinyport))

