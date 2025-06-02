#========================================================================================#
# SIMPLE SHINY SCRIPT FOR DISPLAYING STATIC IMAGES
# 
# THIS APP ASSUMES THAT THE PNGS ARE STORED USING THE FOLLOWING DIRECTORY STRUCTURE:
# img_dir/EXPNAME/DTGSTART-DTGEND/*.png
# WHERE img_dir IS SET BELOW
#
# EXPNAME AND DTGSTART-DTGEND WILL BE USED TO DISPLAY THE EXPERIMENTS AVAILABLE AND 
# THE VALID DATES. DTGSTART/END SHOULD FOLLOW THE YYYYMMDDHH FORMAT
# 
# THE APP ASSUMES A FIXED FILENAME CONVENTION OF THE FORM OUTLINED IN fn_png_name (fn_plot_helpers.R)
#
# PARAM-FTPE-SCORE-XAXIS-FC-DTGSTART-DTGEND-STATION-VALID(LT)(LEVEL)-THRESHOLD.png
#
# WHERE:
#
# PARAM     - THE harp PARAMETER NAME E.G. T2m for 2M TEMPERATURE, S FOR 
#             UPPER-AIR WIND SPEED
# FTYP      - DET OR ENS
# SCORE     - A STRING REPRESENTING THE SCORE OF INTEREST 
# XAXIS     - AN INDICATION OF WHAT XAXIS IS USED WHEN PLOTTING THE SCORE
# FC        - THE INITIAL FORECAST TIME (E.G. 00, 12, ALL (AVERAING OVER ALL))
# DTGSTART  - AS ABOVE
# DTGEND    - AS ABOVE
# STATION   - A STRING REPRESENTING THE STATION SELECTION
# VALID(    - THE VALID TIME (IN THE CASE OF SURFACE SUMMARY OR VERTICAL PROFILES) OR 
#             (LT): THE LEAD TIME (IN THE CASE OF SKILL/THRESHOLD SCORES
#             (LEVEL): IN THE CASE OF PLOTTING AT A GIVEN PRESSURE LEVEL IN hPa
# THRESHOLD - THE THRESHOLD 
#
# USER INTERACTION IS REQUIRED TO:
# 1) CHANGE img_dir (WHERE ARE IMAGES STORED)
# 2) INCLUDE THE EXPERIMENT NAME IN all_exp_names (NO LONGER REQUIRED)
#
# 06/2022
# A BROWSER BUTTON HAS BEEN ADDED TO ALLOW NAVIGATING THROUGH SUBFOLDERS INSIDE
# img_dir, IN CASE THAT ONE HAS A LOT OF VERIFICATION PROJECTS ARRANGED IN SEVERAL
# LEVELS OF SUBFOLDERS - E.G. YEAR, MONTH, DATE...THIS IS FOR EXAMPLE THE CASE OF 
# OD-DT DAILY RUNS FROM THE DESTINATION EARTH - EXTREMES PROJECT (DEODE)
# THE USE OF THE BROWSER BUTTON IS COMPLETELY OPTIONAL, AND ITS FUNCTIONALITIES 
# ARE ONLY INTENDED FOR USE WITH smr_ind = FALSE
# 01/2025
#========================================================================================#

library(shiny)
library(shinyWidgets)
library(stringr)
library(here)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(shinyFiles)
rm(list=ls()) # clear all

#================================================#
# USER INTERACTION HERE
#================================================#

# Point to the app directory
app_dir <- here::here("visualization/visapp")
if ((is.null(app_dir)) || (!dir.exists(app_dir))){
  stop("Error: Please set app_dir to the visapp directory on your system")
}

# Point to where the image files are stored
img_dir <- NULL
if (is.null(img_dir)){
  # Look a default image dirctory in the app directory
  img_dirname <- "sample_images" # Relative to the app_dir
  img_dir     <- file.path(app_dir,img_dirname)
}

# Seasonal/Monthly/Rolling switch for date display in the app.
# To be use in oper context, assumes that Seasonal/Monthly/Rolling directories
# exist under img_dir. Then search for exps in e.g. img_dir/Monthly
# Default value is FALSE
smr_ind=FALSE

#================================================#
# DEFINE COMMON VARIABLES:
# 1) EXPNAME (AND LABEL)
# 2) ALL SURFACE PARAMETERS  
# 3) ALL UPPER-AIR PARAMETERS
# 4) ALL STATION SELECTIONS
# 5) ALL SURFACE SCORES 
# 6) ALL UPPER-AIR SCORES
#================================================#

# Note: These lists follow the convention: "label" = "relevant directory/string in filename"
# New labels should be added as appropriate for new options

# Display name of experiments
source(file.path(app_dir,"all_experiment_names.R"))

#================================================#
# END OF USER INTERACTION
# NORMALLY NO NEED TO EDIT BELOW THIS
# (UNLESS YOU WANT TO ADD IN EXTRA SCORES ETC.)
#================================================#

# What are the available experiments?
c_exps <- list.dirs(path=file.path(img_dir),full.names=FALSE,recursive=FALSE)
if (length(c_exps) > 0){
  exps_for_tab1 <- all_exp_names[unlist(all_exp_names,use.names = FALSE) %in% c_exps]
  # If the display name does not exist in the above, then create a default name
  qwe <- c_exps[!(c_exps %in% unlist(all_exp_names,use.names = FALSE))]
  exp_display_names <- stringr::str_to_title(gsub("_"," ",qwe))
  exps_for_tab2 <- setNames(as.list(qwe),exp_display_names)
  exps_for_tab <- c(exps_for_tab1,exps_for_tab2)
} else {
  exps_for_tab <- "Error: No experiments listed in the directory"
}

# All possible surface parameters
all_surface_params        <- list("MSLP"              = "Pmsl",
                                  "Surface pressure"  = "Ps",
                                  "10m Wind"          = "S10m",
                                  "Wind Direction"    = "D10m",
                                  "Max Wind Gust"     = "Gmax",
                                  "T2m"               = "T2m",
                                  "Max T2m"           = "Tmax",
                                  "Min T2m"           = "Tmin",
                                  "Td2m"              = "Td2m",
                                  "RH2m"              = "RH2m",
                                  "Q2m"               = "Q2m",
                                  "Visibility"        = "vis",
                                  "Cloud Cover"       = "CCtot",
                                  "Low Cloud"         = "CClow",
                                  "Medium Cloud"      = "CCmed",
                                  "High Cloud"        = "CChigh",
                                  "Cloud Base"        = "Cbase",
                                  "1h Precipitation"  = "AccPcp1h",
                                  "3h Precipitation"  = "AccPcp3h",
                                  "6h Precipitation"  = "AccPcp6h",
                                  "12h Precipitation" = "AccPcp12h",
                                  "24h Precipitation" = "AccPcp24h",
                                  "Clear Sky Index"   = "CSI"
)

# Add more surface obs types
more_surface_obstypes <- c("ascat","buoy","ship")
df1                   <- expand_grid(more_surface_obstypes,unname(unlist(all_surface_params)))
df1                   <- paste0(df1[[1]],"_",df1[[2]])
df2                   <- expand_grid(str_to_title(more_surface_obstypes),
                                     names(all_surface_params))
df2                   <- paste0(df2[[1]]," ",df2[[2]])
add_surface_obstypes  <- setNames(as.list(df1),df2)
all_surface_params    <- c(all_surface_params,add_surface_obstypes)

# All possible upper-air parameters
all_UA_params            <- list("Temperature"       = "T",
                                 "Bright. Temp"      = "tb",
                                 "Dew Point T"       = "Td",
                                 "Wind Speed"        = "S",
                                 "Wind Direction"    = "D",
                                 "Geopotential"      = "Z",
                                 "Relative Humidity" = "RH",
                                 "Specific Humidity" = "Q"
)

# Add more upper-air obs types
more_UA_obstypes <- c("airep","amsua","amv","atms","mwhs2","temp")
df1              <- expand_grid(more_UA_obstypes,unname(unlist(all_UA_params)))
df1              <- paste0(df1[[1]],"_",df1[[2]])
df2              <- expand_grid(str_to_title(more_UA_obstypes),
                                names(all_UA_params))
df2              <- paste0(df2[[1]]," ",df2[[2]])
add_UA_obstypes  <- setNames(as.list(df1),df2)
all_UA_params    <- c(all_UA_params,add_UA_obstypes)

# Need an extra list for UA signif plots
all_available_harp_UA   <- c("Z","T","Td","S","D","RH","Q")
all_available_pl        <- c(50,100,200,300,500,700,850,925,1000)
all_available_UApl      <- expand.grid(all_available_harp_UA,all_available_pl)
all_available_UApl      <- paste0(all_available_UApl$Var1,all_available_UApl$Var2)
all_available_UApl      <- setNames(as.list(all_available_UApl),all_available_UApl)
all_UA_params_signif    <- all_available_UApl

# All possible scorecard parameter combinations
all_scorecard_params     <- list("All available"    = "All")

# All possible station selections
all_stations             <- list("All stations"      = "All",
                                 "DINI"              = "DINI",
                                 "Denmark"           = "DK",
                                 "Denmark Synop"     = "DenmarkSynop",
                                 "Netherlands"       = "NL",
                                 "Netherlands Oper"  = "NL_OP",
                                 "Netherlands Synop" = "NetherlandsSynop",
                                 "Iceland"           = "IS",
                                 "Iceland Synop"     = "IcelandSynop",
                                 "Greenland"         = "Greenland",
                                 "Greenland Synop"   = "GreenlandSynop",
                                 "Ireland"           = "IRL",
                                 "Ireland+UK"        = "IE_EN",
                                 "Ireland Synop"     = "IrelandSynop",
                                 "Cork Airport"      = "EICK",
                                 "Dublin Airport"    = "EIDW",
                                 "Shannon Airport"   = "EINN",
                                 "Casement"          = "EIME",
                                 "Knock Airport"     = "EIKN",
                                 "M Buoys"           = "MBuoys",
                                 "M2 Buoy"           = "M2Buoy",
                                 "M3 Buoy"           = "M3Buoy",
                                 "M4 Buoy"           = "M4Buoy",
                                 "M5 Buoy"           = "M5Buoy",
                                 "M6 Buoy"           = "M6Buoy",
                                 "UK Synop"          = "UnitedKingdomSynop",
                                 "Scandinavia"       = "SCD",
                                 "Finland Synop"     = "FinlandSynop",
                                 "Sweden Synop"      = "SwedenSynop",
                                 "Norway Synop"      = "NorwaySynop",
                                 "Estonia Synop"     = "EstoniaSynop",
                                 "Latvia Synop"      = "LatviaSynop",
                                 "Lithuania Synop"   = "LithuaniaSynop",
                                 "France"            = "FR",
                                 "France Synop"      = "FranceSynop",
                                 "North North Sea"   = "NNS",
                                 "North Sea"         = "NorthSea",
                                 "Germany"           = "DE",
                                 "Germany Synop"     = "GermanySynop",
                                 "Alps"              = "Alps",
                                 "Spain Synop"       = "SpainSynop",
                                 "Switzerland Synop" = "SwitzerlandSynop"
) 

# Score separator
score_sep="AND"

# Surface summary scores
# Ensemble
all_ens_ssum_scores      <- list("Spread Skill"              = paste0("rmse",score_sep,"spread-lt"),
                                 "Mean Bias RMSE"            = paste0("mean_bias",score_sep,"rmse-lt"),
                                 "Mean Bias STDV"            = paste0("mean_bias",score_sep,"stde-lt"),
                                 "Spread Skill Ratio"        = "spread_skill_ratio-lt",
                                 "CRPS"                      = "crps-lt",
                                 "Fair CRPS"                 = "fair_crps-lt",
                                 "Member Bias"               = "mbrbias-lt",
                                 "Member RMSE"               = "mbrrmse-lt",
                                 "Member Bias Timeseries"    = "mbrbias-vd",
                                 "Member RMSE Timeseries"    = "mbrrmse-vd",
                                 "Rank Histogram"            = "rank_histogram-NA",
                                 "Normalized Rank Histogram" = "normalized_rank_histogram-NA",
                                 "Mean DailyVar"             = "dailyvar-vh",
                                 "Mean Bias STDV Timeseries" = paste0("mean_bias",score_sep,"stde-vd"),
                                 "Mean Forecast Timeseries"  = "timeseries-vd",
                                 "Mean Frequency Dist"       = "freqdist-NA",
                                 "Mean Frequency Hist"       = "freqhist-cls",
                                 "Mean Frequency Bias"       = "freq_bias-cls",
                                 "Mean Scatterplot"          = "scatterplot-NA"
)

# Ensemble control member
all_ensctrl_ssum_scores  <- list("Bias RMSE"            = paste0("bias",score_sep,"rmse-lt"),
                                 "Bias STDV"            = paste0("bias",score_sep,"stde-lt"),
                                 "Bias MAE"             = paste0("bias",score_sep,"mae-lt"),
                                 "DailyVar"             = "dailyvar-vh",
                                 "Forecast Timeseries"  = "timeseries-vd",
                                 "Bias STDV Timeseries" = paste0("bias",score_sep,"stde-vd"),
                                 "Frequency Dist"       = "freqdist-NA",
                                 "Frequency Hist"       = "freqhist-cls",
                                 "Frequency Bias"       = "freq_bias-cls",
                                 "Scatterplot"          = "scatterplot-NA"
)
all_ensctrl_ssum_scores  <- lapply(all_ensctrl_ssum_scores,function(x) paste0("ctrl",x))

# Deterministic experiment 
all_det_ssum_scores      <- list("Bias RMSE"            = paste0("bias",score_sep,"rmse-lt"),
                                 "Bias STDV"            = paste0("bias",score_sep,"stde-lt"),
                                 "Bias MAE"             = paste0("bias",score_sep,"mae-lt"),
                                 "DailyVar"             = "dailyvar-vh",
                                 "Forecast Timeseries"  = "timeseries-vd",
                                 "Bias STDV Timeseries" = paste0("bias",score_sep,"stde-vd"),
                                 "Frequency Dist"       = "freqdist-NA",
                                 "Frequency Hist"       = "freqhist-cls",
                                 "Frequency Heatmap"    = "2dfreqhist-cls",
                                 "Frequency Bias"       = "freq_bias-cls",
                                 "Scatterplot"          = "scatterplot-NA"
)
# Add SCAT scores
all_scat_ssum_scores_2b  <- list("HY-2B Bias RMSE"      = paste0("SCAThy2bbias",score_sep,"rmse-lt"),
                                 "HY-2B Bias STDV"      = paste0("SCAThy2bbias",score_sep,"stde-lt"))
all_scat_ssum_scores_2c  <- list("HY-2C Bias RMSE"      = paste0("SCAThy2cbias",score_sep,"rmse-lt"),
                                 "HY-2C Bias STDV"      = paste0("SCAThy2cbias",score_sep,"stde-lt"))

# Combine
all_det_ssum_scores      <- c(all_det_ssum_scores,all_scat_ssum_scores_2b,
                              all_scat_ssum_scores_2c)

# Surface skill scores
skill_scores_ens         <- c("fair_brier_score","brier_score","brier_skill_score","brier_score_reliability",
                              "brier_score_resolution","brier_score_uncertainty","roc_area")
skill_scores_ens_other   <- c("reliability","roc","economic_value")
skill_scores_det         <- c("threat_score","false_alarm_rate","false_alarm_ratio","kuiper_skill_score",
                              "frequency_bias","equitable_threat_score","extreme_dependency_score",
                              "symmetric_eds","extreme_dependency_index","symmetric_edi")

skill_scores_ens         <- setNames(as.list(skill_scores_ens),gsub("_"," ",str_to_title(skill_scores_ens)))
skill_scores_ens_other   <- setNames(as.list(skill_scores_ens_other),gsub("_"," ",str_to_title(skill_scores_ens_other)))
skill_scores_det         <- setNames(as.list(skill_scores_det),gsub("_"," ",str_to_title(skill_scores_det)))

# Ensemble scores (threshold and leadtime plotting)
all_ens_skill_scores_th  <- lapply(skill_scores_ens,function(x) paste0(x,"-th"))
all_ens_skill_scores_lt  <- lapply(skill_scores_ens,function(x) paste0(x,"-lt"))
all_ens_skill_scores_ot  <- lapply(skill_scores_ens_other,function(x) paste0(x,"-NA"))

# Deterministic experiment (thresold and leadtime plotting) 
all_det_skill_scores_th  <- lapply(skill_scores_det,function(x) paste0(x,"-th"))
all_det_skill_scores_lt  <- lapply(skill_scores_det,function(x) paste0(x,"-lt"))

# Surface maps
# Ensemble
all_ens_map_scores       <- list("Mean Bias" = "mean_bias",
                                 "Mean RMSE" = "rmse")
all_ens_map_scores       <- lapply(all_ens_map_scores,function(x) paste0(x,"-mp"))

# Ensemble control member
all_ensctrl_map_scores   <- list("Bias" = "bias",
                                 "RMSE" = "rmse")
all_ensctrl_map_scores   <- lapply(all_ensctrl_map_scores,function(x) paste0("ctrl",x,"-mp"))

# Deterministic experiment 
all_det_map_scores       <- list("Obs Frequency" = "obsfreq",
                                 "Bias"          = "bias",
                                 "RMSE"          = "rmse",
                                 "Cases"         = "num_cases")
all_det_map_scores       <- lapply(all_det_map_scores,function(x) paste0(x,"-mp"))

# Add SCAT scores
all_scat_map_scores_2b   <- list("HY-2B Bias Map"      = paste0("SCAThy2bbias-mp"),
                                 "HY-2B RMSE Map"      = paste0("SCAThy2brmse-mp"),
                                 "HY-2B Num Cases Map" = paste0("SCAThy2bnum_cases-mp")
                                 )
all_scat_map_scores_2c   <- list("HY-2C Bias Map"      = paste0("SCAThy2cbias-mp"),
                                 "HY-2C RMSE Map"      = paste0("SCAThy2crmse-mp"),
                                 "HY-2C Num Cases Map" = paste0("SCAThy2cnum_cases-mp")
                                 )
# Combine
all_det_map_scores       <- c(all_det_map_scores,all_scat_map_scores_2b,
                              all_scat_map_scores_2c)

# Upper air scores
# Leadtime pressure levels
all_ens_pl_scores        <- list("Mean Bias RMSE" = paste0("mean_bias",score_sep,"rmse"))
all_ens_pl_scores        <- lapply(all_ens_pl_scores,function(x) paste0(x,"-lt"))

all_ensctrl_pl_scores    <- list("Bias RMSE" = paste0("bias",score_sep,"rmse"))
all_ensctrl_pl_scores    <- lapply(all_ensctrl_pl_scores,function(x) paste0("ctrl",x,"-lt"))

all_det_pl_scores        <- list("Bias RMSE" = paste0("bias",score_sep,"rmse"))
all_det_pl_scores        <- lapply(all_det_pl_scores,function(x) paste0(x,"-lt"))
# Add obsfreq
add_det_pl_scores        <- list("Obs Frequency" = "obsfreq-mp")
all_det_pl_scores        <- c(all_det_pl_scores,add_det_pl_scores)

# Profiles
# Ensemble
all_ens_prof_scores      <- list("Mean Bias RMSE" = paste0("mean_bias",score_sep,"rmse"),
                                 "Mean Bias STDV" = paste0("mean_bias",score_sep,"stde"),
                                 "Spread Skill"   = paste0("rmse",score_sep,"spread"),
                                 "CRPS"           = "crps",
                                 "Fair CRPS"      = "fair_crps")
all_ens_prof_scores      <- lapply(all_ens_prof_scores,function(x) paste0(x,"-pr"))

# Ensemble control member
all_ensctrl_prof_scores  <- list("Bias RMSE" = paste0("bias",score_sep,"rmse"),
                                 "Bias STDV" = paste0("bias",score_sep,"stde"))
all_ensctrl_prof_scores  <- lapply(all_ensctrl_prof_scores,function(x) paste0("ctrl",x,"-pr"))

# Deterministic experiment 
all_det_prof_scores      <- list("Bias RMSE" = paste0("bias",score_sep,"rmse"),
                                 "Bias STDV" = paste0("bias",score_sep,"stde"))
all_det_prof_scores      <- lapply(all_det_prof_scores,function(x) paste0(x,"-pr"))
# Add UAC scores
all_det_prof_UACscores   <- list("Drift corrected Bias RMSE" = paste0("UACbias",score_sep,"rmse"),
                                 "Drift corrected Bias STDV" = paste0("UACbias",score_sep,"stde"))
all_det_prof_UACscores   <- lapply(all_det_prof_UACscores,function(x) paste0(x,"-pr"))
all_det_prof_scores      <- c(all_det_prof_scores,all_det_prof_UACscores)
# Add pressure/channel leadtime heatmaps
all_lhmaps_ssum_scores   <- list("Bias Heatmap"         = "bias-plt",
                                 "RMSE Heatmap"         = "rmse-plt",
                                 "STDV Heatmap"         = "stde-plt",
                                 "Cases Heatmap"        = "num_cases-plt")
all_det_prof_scores      <- c(all_det_prof_scores,all_lhmaps_ssum_scores)

# Statistical signif scores and scorecards
# Ensemble score diffs
all_ens_sdiffs_scores <- list("Mean Bias"    = "mean_bias-lt",
                              "RMSE"         = "rmse-lt",
                              "Spread"       = "spread-lt",
                              "CRPS"         = "crps-lt",
                              "Fair CRPS"    = "fair_crps-lt")
all_ens_sdiffs_scores <- lapply(all_ens_sdiffs_scores,function(x) paste0("sdiff",x))

# Deterministic score diffs
all_det_sdiffs_scores <- list("Bias"    = "bias-lt",
                              "RMSE"    = "rmse-lt",
                              "STDV"    = "stde-lt",
                              "MAE"     = "mae-lt")
all_det_sdiffs_scores <- lapply(all_det_sdiffs_scores,function(x) paste0("sdiff",x))

# Scorecard 
all_scorecard_scores <- list("Scorecard" = "scard-lt",
                             "Tile Scorecard" = "altscard-lt")

#================================================#
# UI
#================================================#

# UI
ui <- shiny::tags$html(
  
  # Resizes the window, removes blank space
  shiny::tags$head(
    shiny::tags$script('
                       var dimension = [0, 0];
                       $(document).on("shiny:connected", function(e) {
                       dimension[0] = window.innerWidth;
                       dimension[1] = window.innerHeight;
                       Shiny.onInputChange("dimension", dimension);
                       });
                       $(window).resize(function(e) {
                       dimension[0] = window.innerWidth;
                       dimension[1] = window.innerHeight;
                       Shiny.onInputChange("dimension", dimension);
                       });
                       ')
    ),
  
  # Start
  shiny::includeCSS("sample_style.css"),
  
  shiny::navbarPage( # Add the logo at the top
    if (file.exists(file.path(app_dir,"www/logo.png"))) {
      title=div(img(src="logo.png",
                    style="margin-top: -14px;
                    padding-right:10px;
                    padding-bottom:2px;",
                    height = "50px"))
    } else {
      title = ""
    },
    windowTitle="harp-verif shiny app",
    # Just one  panel: Point verif
    shiny::tabPanel("Point Verification",
                    shiny::fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          #shinyWidgets::prettyRadioButtons('expname',label='Experiment',
                          #                                 #choices = stats::setNames(dir_names,all_exp_names),
                          #                                 choices  = exps_for_tab,
                          #                                 selected = exps_for_tab[[1]],
                          #                                 outline = TRUE),
                          shinyFiles::shinyDirButton("folder", "Browse", "Select a root directory containing verification 'projects' and presss 'Select': "),
                          shiny::uiOutput("timetypeui"),
                          shiny::selectInput('expname',label='Experiment',
                                             choices = "Waiting..."),
                          shiny::selectInput('currentyear',label='Year',
                                             choices = "Waiting..."),
                          shiny::selectInput('dates',label='Date',
                                             choices = "Waiting..."),
                          shiny::tabsetPanel(
                            id = "vartype",
                            shiny::tabPanel("Surface",
                                            shiny::tabsetPanel(
                                              id="surfacetype",
                                              shiny::tabPanel("Summary"),
                                              shiny::tabPanel("Skill"),
                                              shiny::tabPanel("Map"),
                                              shiny::tabPanel("Signif")
                                            )),
                            shiny::tabPanel("Upper Air",
                                            shiny::tabsetPanel(
                                              id="temptype",
                                              shiny::tabPanel("Summary"),
                                              shiny::tabPanel("Prof"),
                                              shiny::tabPanel("Signif")
                                            )),
                            shiny::tabPanel("Scorecards")),
                          br(),
                          shinyWidgets::prettyRadioButtons('station',label='Station selection',
                                                           choices = "Waiting...",
                                                           outline = TRUE)
                        ),
                        mainPanel(fluidRow(
                          column(2,
                                 shiny::selectInput('param',label='Parameter',
                                                    choices = "Waiting...")),
                          column(3,
                                 shiny::selectInput('score',label='Score',
                                                    choices = "Waiting...")),
                          column(7,
                                 column(2,
                                        shiny::selectInput('inittime',label='Cycle',
                                                           choices = "Waiting...")),
                                 column(5,
                                        shiny::uiOutput("validselect")),
                                 column(5,
                                        shiny::uiOutput("thresholdselect"))
                          ), # column 8
                          br(),
                          shiny::imageOutput("pngImage")
                        ) # Fluid Row
                        ) # MainPanel
                      ) # Sidebar layout
                    ) # Fluid Page
    ) # Tab Panel
  ) # tags$body
) # tags$html

#================================================#
# SERVER
#================================================#

server <- function(input, output, session) {
  
  
  # Indicator for missing data
  mdi <- "No data"
   # Set up a root path for folder browsing
  roots <- c(home = normalizePath(img_dir))
  
  # Enable folder browser
  shinyFiles::shinyDirChoose(input, "folder", roots = roots, session = session)

  # Reactive: Get selected directory
  selected_dir <- reactiveVal(img_dir)  # Initialize with img_dir

  observeEvent(input$folder, {
    parsed_dir <- shinyFiles::parseDirPath(roots, input$folder)
    if (is.character(parsed_dir) && length(parsed_dir) == 1 && nzchar(parsed_dir)) {
     selected_dir(parsed_dir)
    }
  })

  # Render UI to change the "date" section format if the Seasonal/Monthly/Rolling
  # switch is TRUE (to be used for oper)
  output$timetypeui <- shiny::renderUI({
    if (smr_ind){
      dso = tagList()
      dso[[1]] <- shiny::tabsetPanel(
        id="timetype",
        shiny::tabPanel("Monthly"),
        shiny::tabPanel("Rolling"),
        shiny::tabPanel("Seasonal")
      )
      dso
    } else {
      # Just a dummy output
      dso = tagList()
      dso
    }
   })
  
  # Get the available experiments by
  # updating c_exps based on the selected directory
  c_exps <- shiny::reactive({
    selected_path <- selected_dir()
    if (!is.null(selected_path)) {
      dl <- list.dirs(path = selected_path, full.names = FALSE, recursive = FALSE)
      #dl <- setdiff(dl, c("Monthly", "Rolling", "Seasonal"))
      if (length(dl) > 0) {
        exps_for_tab1 <- all_exp_names[unlist(all_exp_names, use.names = FALSE) %in% dl]
        qwe <- dl[!(dl %in% unlist(all_exp_names, use.names = FALSE))]
        exp_display_names <- stringr::str_to_title(gsub("_", " ", qwe))
        exps_for_tab2 <- setNames(as.list(qwe), exp_display_names)
        exps_for_tab <- c(exps_for_tab1, exps_for_tab2)
      } else {
        exps_for_tab <- "Error: No experiments listed in the directory"
      }
    } else {
      exps_for_tab <- "Error: No experiments listed in the directory"
    }
    exps_for_tab
  })
  
  # Is exp already selected and valid?
  selected_exp <- shiny::reactive({
    se_tmp <- NULL
    if (!is.null(input$expname) && (input$expname %in% unlist(c_exps(),use.names = FALSE))){
      se_tmp <- input$expname
    } else if (c_dates()[1] == "Error: No experiments listed in the directory"){
      se_tmp <- mdi
    }
    se_tmp
  })
  
  # Update "expname"
  shiny::observe(
    shiny::updateSelectInput(
      session,
      "expname",
      choices = c_exps(),
      selected = selected_exp()
    )
  )
    
  # Get available years for a given experiment and update selection
  c_years <- shiny::reactive({
    # Again add a switch if smr_ind is TRUE
    if (smr_ind){
      dl <- list.dirs(path=file.path(img_dir,input$timetype,input$expname),
                      full.names=FALSE,recursive=FALSE)
    } else {
      dl <- list.dirs(path=file.path(selected_dir(),input$expname),
                      full.names=FALSE,recursive=FALSE)
    }
    # Match only directories with YYYYMMDDHH-YYYYMMDDHH format
    dl <- dl[str_detect(dl,'^[0-9]{10}-[0-9]{10}$')]
    # Date formatting
    if (!is.null(dl) && length(dl)>0) {
      files_years <- data.frame(dates = dl) %>%
        tidyr::separate(.data$dates, c("startdate", "enddate"), "-") %>%
        dplyr::arrange(.data$startdate, .data$enddate) %>%
        dplyr::mutate(
          years = substr(.data$startdate,1,4)
        ) %>% dplyr::pull(.data$years) %>% sort(.,decreasing = TRUE)
      files_years
    } else {
      mdi
    }
  })
  
  # Is year already selected and valid?
  selected_year <- shiny::reactive({
    yc_tmp <- NULL
    if (!is.null(input$currentyear) && (input$currentyear %in% unlist(c_years(),use.names = FALSE))){
      yc_tmp <- input$currentyear
    } else if (c_years()[1] == mdi){
      yc_tmp <- mdi
    }
    yc_tmp
  })
  
  # Render a tabpanel for the available years
  #output$yearselect <- shiny::renderUI({
  #  mytabs <- lapply(c_years(), tabPanel)
  #  do.call(tabsetPanel,c(mytabs,list(id="currentyear",selected=selected_year())))
  #})
  # Use select input for the years
  shiny::observe(
    shiny::updateSelectInput(
      session,
      "currentyear",
      choices = c_years(),
      selected = selected_year()
    )
  )

  # Get current dates for a given experiment and update selection
  c_dates <- shiny::reactive({
    req(input$currentyear)
    # Again add a switch if smr_ind is TRUE
    if (smr_ind){
      dl <- list.dirs(path=file.path(img_dir,input$timetype,input$expname),
                      full.names=FALSE,recursive=FALSE)
    } else {
      dl <- list.dirs(path=file.path(selected_dir(),input$expname),
                      full.names=FALSE,recursive=FALSE)
    }
    # Match only directories with YYYYMMDDHH-YYYYMMDDHH format
    dl <- dl[str_detect(dl,'^[0-9]{10}-[0-9]{10}$')]
    # Date formatting
    if (!is.null(dl) && length(dl)>0) {
      files_dates <- data.frame(dates = dl) %>%
        tidyr::separate(.data$dates, c("startdate", "enddate"), "-") %>%
        dplyr::arrange(.data$startdate, .data$enddate) %>%
        dplyr::mutate(
          dates = paste(.data$startdate, .data$enddate, sep = "-"),
          years = substr(.data$startdate,1,4)
        ) %>%
        dplyr::filter(.data$years == input$currentyear) %>%
        dplyr::pull(.data$dates) %>% rev(.)
      
      req(length(files_dates)>0) # Avoids an abort for selected date below!
      split_dates <- strsplit(files_dates, "-")
      dates_start <- purrr::map(split_dates, ~format(lubridate::ymd_h(.x[1]),"%Y/%m/%d %HZ"))
      dates_end   <- purrr::map(split_dates, ~format(lubridate::ymd_h(.x[2]),"%Y/%m/%d %HZ"))
      
      files_dates_labels <- purrr::map2_chr(dates_start, dates_end, paste, sep = " - ")
      stats::setNames(files_dates,files_dates_labels)
    } else {
      mdi
    }
  })
  
  # Is date already selected and valid?
  selected_date <- shiny::reactive({
    sd_tmp <- NULL
    if (!is.null(input$dates) && (input$dates %in% unlist(c_dates(),use.names = FALSE))){
      sd_tmp <- input$dates
    } else if (c_dates()[1] == mdi){
      sd_tmp <- mdi
    }
    sd_tmp
  })
  
  # Update "dates"
  shiny::observe(
    shiny::updateSelectInput(
      session,
      "dates",
      choices = c_dates(),
      selected = selected_date()
    )
  )
  
  # Based on experiment name and dates, define the directory where images are stored
  data_dirname <- shiny::reactive({
    if (smr_ind){
      file.path(img_dir,input$timetype,input$expname,input$dates)
    } else {
      file.path(selected_dir(),input$expname,input$dates)
    }
  })
  # Add an extra check to handle the case where the project name is included
  # as the first element of the png name
  proj_name_flag <- shiny::reactive({
    raw_files <- list.files(path = data_dirname(),pattern="*.png")
    fa <- unique(unlist(lapply(strsplit(raw_files,"-"),'[',1)))
    input$expname %in% fa
  })
  # All images in this directory
  all_files <- shiny::reactive({
    raw_files <- list.files(path = data_dirname(),pattern="*.png")
    # Now rename based on proj_name_flag
    if (proj_name_flag()){
      # Get the files without the project name
      f1 <- raw_files[lapply(strsplit(raw_files,"-"),'[',1) != input$expname]
      # And with the project name
      f2 <- raw_files[lapply(strsplit(raw_files,"-"),'[',1) == input$expname]
      # Then remove the project name before the first "-"
      f3 <- sapply(str_split(f2,"-",n=2),'[',2)
      # Join and take the unique files
      unique(c(f1,f3))
    } else {
      raw_files
    }
  })
  
  # Get all available parameters for this expname+date and update possible parameters based on tab selection
  params_out <- shiny::reactive({
    if (length(all_files())>0){
      param_avail <- unique(unlist(lapply(strsplit(all_files(),"-"),'[',1)))
      #param_for_tab <- switch(input$vartype,
      #                        "Surface"    = all_surface_params,
      #                        "Upper Air"  = all_UA_params,
      #                        "Scorecards" = all_scorecard_params)
      if (input$vartype == "Surface"){
        param_for_tab <- all_surface_params
      } else if (input$vartype == "Upper Air"){
        param_for_tab <- switch(input$temptype,
                                "Summary" = all_UA_params,
                                "Prof"    = all_UA_params,
                                "Signif"  = all_UA_params_signif)
      } else if (input$vartype == "Scorecards"){
        param_for_tab <- all_scorecard_params
      }
      cc <- param_for_tab[unlist(param_for_tab,use.names = FALSE) %in% param_avail]
      if (length(cc)>0){
        cc
      } else {
        mdi
      }
    } else{
      mdi
    }
  })
  
  # Current parameter selected
  selected_param <- shiny::reactive({
    sp_tmp <- NULL
    if (!is.null(input$param) && (input$param %in% unlist(params_out(),use.names = FALSE))){
      sp_tmp <- input$param
    } else if (params_out()[1] == mdi){
      sp_tmp <- mdi
    }
    sp_tmp
  })
  
  # Update "param"
  shiny::observe(
    shiny::updateSelectInput(
      session,
      "param",
      choices = params_out(),
      selected = selected_param()
    )
  )
  
  # Files relevant for this parameter
  rel_files <- shiny::reactive({ 
    all_files()[unlist(lapply(strsplit(all_files(),"-"),'[',1)) == input$param]
  })
  
  # Now filter by the station choice
  stations_out <- shiny::reactive({
    if (length(rel_files())>0){
      stations_avail <- unique(unlist(lapply(strsplit(rel_files(),"-"),'[',8)))
      all_stations[unlist(all_stations,use.names = FALSE) %in% stations_avail]
    } else {
      mdi
    }
  })
  
  # Selected station
  selected_station <- shiny::reactive({
    sstn_tmp <- NULL
    if (!is.null(input$station) && (input$station %in% unlist(stations_out(),use.names = FALSE))){
      sstn_tmp <- input$station
    } else if (stations_out()[1] == mdi){
      sstn_tmp <- mdi
    }
    sstn_tmp
  })
  
  # Update "station"
  shiny::observe(
    shinyWidgets::updatePrettyRadioButtons(
      session,
      "station",
      choices = stations_out(),
      prettyOptions = list(outline = TRUE),
      selected = selected_station()
    )
  )
  
  # For the selected parameter+station, get the available scores
  # Filter the files again
  rrel_files <- shiny::reactive({ 
    rel_files()[unlist(lapply(strsplit(rel_files(),"-"),'[',8)) == input$station]
  })
  # What scores are available in these files
  scores_avail <- shiny::reactive({
    if (length(rrel_files())>0){
      sp1 <- unlist(lapply(strsplit(rrel_files(),"-"),'[',3))
      sp2 <- unlist(lapply(strsplit(rrel_files(),"-"),'[',4))
      unique(paste0(sp1,"-",sp2))
      #unique(unlist(lapply(strsplit(rrel_files(),"-"),'[',3)))
    } else {
      mdi
    }
  })
  
  # Split into ENS and DET scores
  # Do any ENS scores exits?
  is_ens_scores <- shiny::reactive({
    ftyp <- unique(unlist(lapply(strsplit(rrel_files(),"-"),'[',2)))
    if (length(ftyp) == 1){
      if (ftyp == "ens"){
        TRUE
      } else if (ftyp == "det"){
        FALSE
      } 
    } else {
      FALSE
    }
  })
  
  # Get either ens or det scores
  scores_for_tab1 <- shiny::reactive({
    if (is_ens_scores()){
      mty <- "ens"  
    } else {
      mty <- "det"
    }
    if (input$vartype == "Surface"){
      switch(input$surfacetype,
             "Summary" = get(paste0("all_",mty,"_ssum_scores")),
             "Skill"   = get(paste0("all_",mty,"_skill_scores_th")),
             "Map"     = get(paste0("all_",mty,"_map_scores")),
             "Signif"  = get(paste0("all_",mty,"_sdiffs_scores")))
    } else if (input$vartype == "Upper Air"){
      switch(input$temptype,
             "Summary" = get(paste0("all_",mty,"_pl_scores")),
             "Prof"    = get(paste0("all_",mty,"_prof_scores")),
             "Signif"  = get(paste0("all_",mty,"_sdiffs_scores")))
    } else if (input$vartype == "Scorecards"){
      all_scorecard_scores
    }
  })
  
  # If ens scores exist, this will contain the control scores from the ensemble
  scores_for_tab2 <- shiny::reactive({
    if (is_ens_scores()){
      if (input$vartype == "Surface"){
        switch(input$surfacetype,
               "Summary" = all_ensctrl_ssum_scores,
               "Skill"   = all_ens_skill_scores_lt,
               "Map"     = all_ensctrl_map_scores,
               "Signif"  = list("L" = -9999))
      } else if (input$vartype == "Upper Air"){
        switch(input$temptype,
               "Summary" = all_ensctrl_pl_scores,
               "Prof"    = all_ensctrl_prof_scores,
               "Signif"  = list("L" = -9999))
      } else if (input$vartype == "Scorecards"){
        list("L" = -9999)
      }
    } else {
      if (input$vartype == "Surface"){
        switch(input$surfacetype,
               "Summary" = list("L" = -9999),
               "Skill"   = all_det_skill_scores_lt,
               "Map"     = list("L" = -9999),
               "Signif"  = list("L" = -9999))
      } else if (input$vartype == "Upper Air"){
        switch(input$temptype,
               "Summary" = list("L" = -9999),
               "Prof"    = list("L" = -9999),
               "Signif"  = list("L" = -9999))
      } else if (input$vartype == "Scorecards"){
        list("L" = -9999)
      }
    }
  })
  
  # And the last one (for other threshold scores)
  scores_for_tab3 <- shiny::reactive({
    if (is_ens_scores()){
      if (input$vartype == "Surface"){
        switch(input$surfacetype,
               "Summary" = list("L" = -9999),
               "Skill"   = all_ens_skill_scores_ot,
               "Map"     = list("L" = -9999),
               "Signif"  = list("L" = -9999))
      } else if (input$vartype == "Upper Air"){
        switch(input$temptype,
               "Summary" = list("L" = -9999),
               "Prof"    = list("L" = -9999),
               "Signif"  = list("L" = -9999))
      } else if (input$vartype == "Scorecards"){
        list("L" = -9999)
      }
    } else {
      list("L" = -9999)
    }
  })
  
  scores_out1 <- shiny::reactive({
    scores_for_tab1()[unlist(scores_for_tab1(),use.names = FALSE) %in% scores_avail()]
  })
  scores_out2 <- shiny::reactive({
    scores_for_tab2()[unlist(scores_for_tab2(),use.names = FALSE) %in% scores_avail()]
  })
  scores_out3 <- shiny::reactive({
    scores_for_tab3()[unlist(scores_for_tab3(),use.names = FALSE) %in% scores_avail()]
  })
  
  # Combine scores depending on whether it is ens or det
  scores_out <- shiny::reactive({
    if (is_ens_scores()){
      if ((length(scores_out1()) == 0) & (length(scores_out2()) == 0) & (length(scores_out3()) == 0)){
        mdi
      } else {
        if (input$vartype == "Surface"){
          if(input$surfacetype == "Skill"){
            list(`Skill scores vs threshold` = scores_out1(),
                 `Skill scores vs leadtime` = scores_out2(),
                 `Additional scores` = scores_out3())
          } else {
            list(`Ens scores` = scores_out1(),
                 `Control scores` = scores_out2())
          }
        } else {
          list(`Ens scores` = scores_out1(),
               `Control scores` = scores_out2())
        }
      }
    } else {
      if ((length(scores_out1()) == 0) & (length(scores_out2()) == 0)){
        mdi
      } else {
        if (input$vartype == "Surface"){
          if(input$surfacetype == "Skill"){
            list(`Skill scores vs threshold` = scores_out1(),
                 `Skill scores vs leadtime` = scores_out2())
          } else {
            list(`Det scores` = scores_out1())
          }
        } else {
          list(`Det scores` = scores_out1())
        }
      }
    }
  })
  
  # Selected score
  selected_score <- shiny::reactive({
    ss_tmp <- NULL
    if (!is.null(input$score) && (input$score %in% unlist(scores_out(),use.names=FALSE))){
      ss_tmp <- input$score
    } else if (scores_out()[1] == mdi){
      ss_tmp <- mdi
    }
    ss_tmp
  })
  
  # Update "score"
  shiny::observe(
    shiny::updateSelectInput(
      session,
      "score",
      choices = scores_out(),
      selected = selected_score()
    )
  )
  
  # Get the inital forecast time (e.g. 00, 12, ALL)
  # For the selected parameter+station+score, filter the files again
  rrrel_files <- shiny::reactive({ 
    sp1 <- unlist(lapply(strsplit(rrel_files(),"-"),'[',3))
    sp2 <- unlist(lapply(strsplit(rrel_files(),"-"),'[',4))
    sp3 <- paste0(sp1,"-",sp2)
    #rrel_files()[unlist(lapply(strsplit(rrel_files(),"-"),'[',3)) == input$score]
    rrel_files()[sp3 == input$score]
  })
  
  # Now get the available initial forecast times
  inittime_out <- shiny::reactive({
    if (length(rrrel_files())>0){
      it <- unique(unlist(lapply(strsplit(rrrel_files(),"-"),'[',5)))
      # Get numerical/string values
      qwe <- str_extract(it,"[aA-zZ]+"); ps <- qwe[!is.na(qwe)]; pn <- it[is.na(qwe)]; pn <- sort(as.numeric(pn))
      pn <- sprintf("%02d",pn); it <- c(ps,pn)
      it
    } else{
      mdi
    }
  })
  
  # Selected initial time
  selected_inittime <- shiny::reactive({
    it_tmp <- NULL
    if (!is.null(input$inittime) && (input$inittime %in% inittime_out())){
      it_tmp <- input$inittime
    } else if (inittime_out()[1] == mdi){
      it_tmp <- mdi
    }
    it_tmp
  })
  
  # Update "inittime"
  shiny::observe(
    shiny::updateSelectInput(
      session,
      "inittime",
      choices = inittime_out(),
      selected = selected_inittime()
    )
  )
  
  # Render UI to change the label for lead time/valid time based on metric type
  pp_label <- shiny::reactive({
    if (input$vartype == "Surface"){
      switch(input$surfacetype,
             "Summary" = "Lead time/Valid time",
             "Skill"   = "Lead time",
             "Map"     = "Valid time",
             "Signif"  = "New model")
    } else if (input$vartype == "Upper Air"){
      switch(input$temptype,
             "Summary" = "Level (hPa) / Channel",
             "Prof"    = "Valid time",
             "Signif"  = "New model")
    } else if (input$vartype == "Scorecards"){
      "New model"
    }
  })
  output$validselect <- shiny::renderUI({
    shiny::selectInput('validtime',label=pp_label(),
                       choices = "Waiting...")
  })
  
  # Valid (or lead) times available in the files
  # For the selected parameter+station+score+initial time, filter the files again
  rrrrel_files <- shiny::reactive({ 
    rrrel_files()[unlist(lapply(strsplit(rrrel_files(),"-"),'[',5)) == input$inittime]
  })
  
  # Now get the available valid/lead times
  validtime_out <- shiny::reactive({
    vo <- gsub(".png","",unique(unlist(lapply(strsplit(rrrrel_files(),"-"),'[',9))))
    vo <- vo[!is.na(vo)] # Drop any NA
    rname_flag = TRUE
    if (input$vartype == "Surface"){
      if (input$surfacetype == "Signif"){
        rname_flag = FALSE
      }
    } else if (input$vartype == "Upper Air"){
      if (input$temptype == "Signif"){
        rname_flag = FALSE
      }
    } else if (input$vartype == "Scorecards"){
      rname_flag = FALSE
    }
    if (length(vo)>0){
      if (rname_flag){
        # Get numerical/string values
        qwe <- str_extract(gsub("_","",vo),"[aA-zZ]+")
        ps <- qwe[!is.na(qwe)]
        pn <- vo[is.na(qwe)]
        pr <- pn[grepl("_",pn,fixed = T)]
        if (length(pr)>0) {
          pr <- paste0(gsub("_","-",pr),"h")
        }
        pn <- pn[!grepl("_",pn,fixed=T)]
        pnsort <- sort(as.numeric(pn)) # Sorting character is not enough
        pnsort <- sprintf("%02d",pnsort) # Pad to valid hours
        pnsort[!(pnsort %in% pn)] <- as.numeric(pnsort[!(pnsort %in% pn)]) # In case padding is not required
        vo <- c(ps,pr,pnsort)
        vom <- gsub("h","",gsub("-","_",vo))
        #qwe <- as.numeric(str_extract(vo,"[0-9]+")); qwe <- qwe[!is.na(qwe)]; qwe <- sort(qwe); qwe <- sprintf("%02d",qwe)
      } else {
        vom <- vo
      }
      setNames(as.list(vom),vo)
    } else {
      mdi
    }
  })
  
  # Selected valid/lead time
  selected_validtime <- shiny::reactive({
    vt_tmp <- NULL
    if (!is.null(input$validtime) && (input$validtime %in% unlist(validtime_out(),use.names=FALSE))){
      vt_tmp <- input$validtime
    } else if (validtime_out()[1] == mdi){
      vt_tmp <- mdi
    }
    vt_tmp
  })
  
  # Update "validtime" (note: this is used for both valid and lead times)
  shiny::observe(
    shiny::updateSelectInput(
      session,
      "validtime",
      choices = validtime_out(),
      selected = selected_validtime()
    )
  )
  
  # Render UI to change the label for threshold based on metric type
  tt_label <- shiny::reactive({
    if (input$vartype == "Surface"){
      switch(input$surfacetype,
             "Summary" = "Threshold",
             "Skill"   = "Threshold",
             "Map"     = "Threshold",
             "Signif"  = "Ref model")
    } else if (input$vartype == "Upper Air"){
      switch(input$temptype,
             "Summary" = "Threshold",
             "Prof"    = "Threshold",
             "Signif"  = "Ref model")
    } else if (input$vartype == "Scorecards"){
      "Ref model"
    }
  })
  output$thresholdselect <- shiny::renderUI({
    shiny::selectInput('threshold',label=tt_label(),
                       choices = "Waiting...")
  })
  
  
  # Now get the available thresholds (again for a selected parameter+station+score+initial time)
  threshold_out <- shiny::reactive({
    tho <- gsub(".png","",unique(unlist(lapply(strsplit(rrrrel_files(),"-"),'[',10))))
    tho <- tho[!is.na(tho)] # Drop any NA
    rname_flag = TRUE
    if (input$vartype == "Surface"){
      if (input$surfacetype == "Signif"){
        rname_flag = FALSE
      }
    } else if (input$vartype == "Upper Air"){
      if (input$temptype == "Signif"){
        rname_flag = FALSE
      }
    } else if (input$vartype == "Scorecards"){
      rname_flag = FALSE
    }
    if (length(tho)>0){
      if (rname_flag){
        thm <- gsub("m","-",tho) # Some manipulation to deal with All and "m"
        qwe <- str_extract(thm,"[aA-zZ]+"); ps <- qwe[!is.na(qwe)]; pn <- thm[is.na(qwe)]; pn <- sort(as.numeric(pn)) 
        thm <- c(ps,pn); thom <- gsub("-","m",thm)
        setNames(as.list(thom),thm)
      } else {
        setNames(as.list(tho),tho)
      }
    } else {
      mdi
    }
  })
  
  # Selected threshold
  selected_threshold <- shiny::reactive({
    st_tmp <- NULL
    if (!is.null(input$threshold) && (input$threshold %in% unlist(threshold_out(),use.names=FALSE))){
      st_tmp <- input$threshold
    } else if (threshold_out()[1] == mdi){
      st_tmp <- mdi
    } 
    st_tmp
  })
  
  # Updated "threshold"
  shiny::observe(
    shiny::updateSelectInput(
      session,
      "threshold",
      choices = threshold_out(),
      selected = selected_threshold()
    )
  )
  
  
  # Now load the static image output
  output$pngImage <- renderImage({
    
    # Read image width and height according to display
    #c_width  <- session$clientData$output_pngImage_width
    #c_height <- session$clientData$output_pngImage_height 
    
    req(input$validtime)
    req(input$threshold)
    req(input$param)
    req(input$score)
    req(input$inittime)
    req(input$dates)
    req(input$station)
    req(data_dirname())

    
    # Fixed dimensions
    scale_f  <- 0.95
    c_width  <- 700*scale_f
    c_height <- 450*scale_f
    
    if (input$vartype == "Surface"){
      if(input$surfacetype == "Map"){
        c_width  <- 1000*scale_f
        c_height <- 450*scale_f
      }
    } else if (input$vartype == "Scorecards"){
      c_width  <- 1200*scale_f
      c_height <- 800*scale_f
    } else if (input$vartype == "Upper Air"){
      if (grepl("UAC",input$score,ignore.case = FALSE,fixed = TRUE)){
        c_width  <- 700*scale_f
        c_height <- 650*scale_f
      }
    }
    # Add valid_dttm and obsfreq cases
    if (grepl("-vd",input$score,ignore.case = FALSE,fixed = TRUE)){
      c_width  <- 1000*scale_f
    }
    if (grepl("obsfreq",input$score,ignore.case = FALSE,fixed = TRUE)){
      c_width  <- 1000*scale_f
      c_height <- 800*scale_f
    }
    
    
    if (is_ens_scores()){
      mty <- "ens"  
    } else {
      mty <- "det"
    }
    
    # Define the filename
    fname_base <- paste0(input$param,"-",mty,"-",input$score,"-",input$inittime,"-",
                         input$dates,"-",input$station,"-",input$validtime,"-",input$threshold)
    fname    <- paste0(fname_base,".png")
    if (proj_name_flag()){
      fname <- paste0(input$expname,"-",fname)
    }

     # Ensure data_dirname() is valid
     dirname <- data_dirname()
     if (is.null(dirname) || dirname == "") {
       stop("Error: data_dirname() returned NULL or empty.")
     }


    filename <- file.path(data_dirname(),fname)
 
    # Return a list containing the filename
    list(src    = filename,
         width  = c_width,
         height = c_height,
         alt    = "Loading...if this message persists, the image file may be missing for the given selection.")
  },deleteFile = FALSE) # renderImage
  
} # End of server

#================================================#
# RUN APP
#================================================#

shiny::shinyApp(ui,server)
