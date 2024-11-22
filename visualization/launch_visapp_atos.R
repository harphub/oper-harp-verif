#!/usr/bin/env Rscript

library(here)
library(shiny)

# Function to display help message
display_help <- function() {
  cat("Usage: script_name [-h | --help] [port]\n")
  cat("\nOptions:\n")
  cat("  -h, --help   Show this help message and exit.\n")
  cat("  port         Optional port number to use for hosting the Shiny app (default: 9999).\n")
  quit(status = 0)
}

# Capture command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if help flag is present
if ("-h" %in% args || "--help" %in% args) {
  display_help()
}

# Define default values for optional arguments
shinyport <- ifelse(length(args) >= 1, args[1], "9999")
host <- Sys.getenv("HOST")

# Display information for setting up the Shiny app
print('To display your verification figures in the visapp in a Firefox window at ATOS:')
print('1: Copy or link your directory containing verifications into the {verif_scripts_path}/visualization/visapp/sample_images/ folder')
print('2: Open a new terminal.')
print(paste0('3: Execute this command: ssh -L ', shinyport, ':localhost:', shinyport, ' ', host))
print(paste0('4: Open a Firefox window and go to http://127.0.0.1:', shinyport, '/'))

# Run the Shiny app
runApp(here('visualization/visapp/app.R'), port = strtoi(shinyport))

