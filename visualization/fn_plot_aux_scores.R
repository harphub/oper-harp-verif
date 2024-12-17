
#================================================#
# CONSTRUCT AUXILIARY SCORES FROM A HARP FCST
#
# 1) DAILY VARIATION 
# 2) FORECAST TIMESERIES
# 3) FREQUENCY DIST/BIAS BY CLASSES
# 4) SCATTERPLOT
# 
# NOTE: THIS SCRIPT IS TO BE DEPRECATED AND 
# SHOULD BE USED WITH CAUTION!
#================================================#

fn_plot_aux_scores <- function(fcst_input,
                                    png_archive,
                                    png_projname = NA_character_,
                                    plot_num_cases = TRUE,
                                    cmap = "Set2",
                                    compare_mbr000 = TRUE,
                                    mbr_plots = FALSE,
                                    rolling_verif = FALSE,
                                    cmap_hex = "magma",
                                    fsd = NA_character_,
                                    fed = NA_character_){
  
  #=================================================#
  # INITIAL CHECKS 
  #=================================================#
  
  if (!is.list(fcst_input)) {
    stop("Aux scores plotting: A harp forecast object is required, aborting")
  }
  
  station_group_var <- "station_group"
  models_vec        <- names(fcst_input)
  num_models        <- length(models_vec)
  
  # DET or ENS?
  fcst_type <- "det"
  for (ii in seq(1,num_models,1)) {
    c_names     <- names(fcst_input[[ii]])
    c_names     <- c_names[!(grepl("_det",c_names))]
    if (any(grepl("_mbr",c_names))) {
      fcst_type <- "ens"
      break
    }
  }
  
  # Bind fcst to create one fcst data frame
  if (fcst_type == "ens") {
    
    # Add in ensemble mean and spread
    fcst_stats <- harpCore::ens_stats(fcst_input,
                                      keep_members = FALSE) %>%
      harpCore::bind()
    fcst       <- harpCore::bind(fcst_input) 
    fcst_names <- names(fcst)
    
    # When you bind_fcst after using ens_stats, the mean and spread are added 
    # on as columns ens_mean/spread (these repeat for each member). Thus do some
    # manipulation here to add the mean/spread to the "member" column to tidy
    # things up 
    df_meanspread <- NULL
    for (c_model in models_vec) {
      df_tmp        <- fcst_stats %>% dplyr::filter(fcst_model == c_model)
      df_mean       <- df_tmp %>% dplyr::mutate(member = "mean",
                                                fcst   = ens_mean)
      df_spread     <- df_tmp %>% dplyr::mutate(member = "spread",
                                                fcst   = ens_sd)
      df_meanspread <- dplyr::bind_rows(df_meanspread,df_mean,df_spread)
    }
    fcst <- bind_rows(fcst,df_meanspread)
    fcst <- select(fcst,-ens_mean,-ens_sd)
    rm(df_tmp,df_mean,df_spread,df_meanspread)
    
  } else if (fcst_type == "det") {
    fcst       <- harpCore::bind(fcst_input)
    fcst_names <- names(fcst)
  }

  # Add in validhour and station group if they do not exist
  if (!("valid_hour" %in% fcst_names)) {
    fcst <- harpCore::expand_date(fcst,valid_dttm)
    fcst <- harpPoint::mutate_list(fcst,
                                   valid_hour = sprintf("%02d",valid_hour))
  }
  if (!(station_group_var %in% fcst_names)) {
    fcst <- harpPoint::mutate_list(fcst,"{station_group_var}" := "All")
  }
  
  # Useful variables
  param    <- unique(fcst[["parameter"]])
  # Need a workaround for AccPcp
  if (param == "Pcp") { 
    all_fcst_names <- names(fcst)
    param          <- all_fcst_names[grepl("AccPcp",all_fcst_names,fixed = TRUE)]
    if (length(param) > 1) {
      stop("Error in param name for AccPcp")
    }
  }
  fcst       <- dplyr::rename(fcst,"OBS" = all_of(param))
  cycles     <- sort(unique(fcst[["fcst_cycle"]]))
  stations   <- unique(fcst[[station_group_var]])
  par_unit   <- unique(fcst[["units"]])

  # Get start and end dates
  d_out      <- get_sedate(fsd,
                           fed,
                           dplyr::first(sort(unique(fcst[["fcst_dttm"]]))),
                           dplyr::last(sort(unique(fcst[["fcst_dttm"]]))))
  sdate      <- d_out$sdate
  edate      <- d_out$edate
  tsdate     <- d_out$tsdate
  tedate     <- d_out$tedate
  
  # Check png_archive for s/edate
  png_archive <- check_png_sedate(sdate,edate,png_archive)
  
  #=================================================#
  # USER INTERACTION
  #=================================================#
  
  lt_to_use   <- seq(3,48,3) # What lead_times to use?
  all_lts     <- sort(unique(fcst[["lead_time"]]))
  lt_filter   <- intersect(all_lts,lt_to_use)
  if (length(lt_filter) > 0) {
    cat("Using lead times",paste(lt_filter,collapse = ","),"in aux plots\n")
  } else {
    cat("Only found lead times",paste(all_lts,collapse = ","),"\n")
    cat("Do not filter lead times in aux plotting\n")
    lt_to_use <- all_lts
  }
  cycles_oi   <- c("All") # What cycles to plot for?
  if (rolling_verif) {
    cycles_oi <- c("All","00","12")
  }
  cycles_oi   <- base::intersect(cycles_oi,c("All",cycles))
  stations_oi <- unique(stations)
  fd_adjust   <- 1  # Adjust parameter in freq dist plotting
  
  # Scatterplot colormap
  
  #cmap_rgb    <- c("213,213,213", "195,195,231", "164,164,220", "143,143,204",
  #              "42,159,255", "42,205,255", "54,249,235", "163,255,126",
  #              "235,255,54", "255,219,42", "255,179,42", "255,137,42",
  #              "251,52,42", "199,42,42", "149,42,42", "118,0,248")
  #cpal_hex    <- sapply(strsplit(cmap_rgb, ","), function(x)
  #  rgb(x[1], x[2], x[3], maxColorValue=255))
  
  if (cmap_hex == "paired") {
    cpal_hex    <- RColorBrewer::brewer.pal(12,"Paired")
  } else if (cmap_hex == "magma") {
    cpal_hex    <- "A"
  } else if (cmap_hex == "viridis") {
    cpal_hex    <- "D"
  } else if (cmap_hex %in% scico::scico_palette_names()) {
    cpal_hex    <- cmap_hex
  } else {
    warning("cmap_hex ",cmap_hex," is not available, reverting to default\n")
    cpal_hex    <- "A"
  }
  scat_bins   <- 50
  
  #=================================================#
  # END OF USER INTERACTION 
  #=================================================#
  
  #================================================#
  # FIGURE OPTIONS
  #================================================#
  
  # Define some figure widths/heights
  fw          <- 7
  fh          <- 4.5
  fig_units   <- "in"
  fig_dpi     <- 200

  # Define the colour scheme used in line plots 
  mcolors     <- fn_gen_model_colors(models_vec,cmap,withobs = TRUE)
  
  # Line stylces
  line_styles <- c("solid","dashed","dotted","dotdash")
  
  # Some sizes
  line_size   <- 1
  point_size  <- 1.0
  stroke_size <- 1.0
  
  # Score spearator
  score_sep <- "AND"

  # Define various themes
  ptheme_l <- ggplot2::theme_bw() + 
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 10),
      plot.subtitle   = ggplot2::element_text(size = 8),
      axis.text       = ggplot2::element_text(size = 10),
      axis.title      = ggplot2::element_text(size = 10),
      strip.text      = ggplot2::element_text(size = 10),
      legend.text     = ggplot2::element_text(size = 10),
      legend.position = "top"
    )
  ptheme_nc <- ggplot2::theme_bw() + 
    ggplot2::theme(
      axis.text       = ggplot2::element_text(size = 10),
      axis.title      = ggplot2::element_text(size = 10),
      legend.position = "none"
    )
  
  title_date_str = suppressMessages(paste0(stringr::str_to_title(param)," : ",
              format(harpIO::str_datetime_to_datetime(tsdate),"%Y-%m-%d-%H")," - ",
              format(harpIO::str_datetime_to_datetime(tedate),"%Y-%m-%d-%H")))
  
  fxoption_list <- list("param"             = param,
                        "sdate"             = sdate,
                        "edate"             = edate, 
                        "num_models"        = num_models,
                        "par_unit"          = par_unit,
                        "fw"                = fw,
                        "fh"                = fh,
                        "mcolors"           = mcolors,
                        "line_styles"       = line_styles,
                        "line_size"         = line_size, 
                        "stroke_size"       = stroke_size,
                        "point_size"        = point_size,
                        "ens_spec"          = "NA",
                        "c_ftyp"            = fcst_type,
                        "ptheme_l"          = ptheme_l,
                        "ptheme_nc"         = ptheme_nc,
                        "png_archive"       = png_archive,
                        "cmap_hex"          = cmap_hex,
                        "cpal_hex"          = cpal_hex,
                        "station_group_var" = station_group_var,
                        "fd_adjust"         = fd_adjust,
                        "fig_units"         = fig_units,
                        "scat_bins"         = scat_bins,
                        "plot_num_cases"    = plot_num_cases,
                        "fig_dpi"           = fig_dpi,
                        "png_projname"      = png_projname,
                        "score_sep"         = score_sep)
  
  #=================================================#
  # COMPUTE VARIOUS AUX SCORES
  #=================================================#
  
  fcst      <- dplyr::filter(fcst,lead_time %in% lt_to_use)
  leadtimes <- sort(unique(fcst[["lead_time"]]))
  
  if (length(leadtimes) > 5) {
    lt_used_fig <- c(leadtimes[1],
                     leadtimes[2],
                     "... ",
                     leadtimes[length(leadtimes)])
  } else {
    lt_used_fig <- leadtimes
  }
  
  for (cycle in cycles_oi) {
    
    if (cycle == "All") {
      cyc_used_fig <- cycles 
      c_fcst       <- fcst 
    } else {
      cyc_used_fig <- cycle
      c_fcst       <- fcst %>% dplyr::filter(fcst_cycle == cycle)
    }
    
    num_cycles <- length(unique(c_fcst[["fcst_dttm"]])) 
    title_str = paste0(title_date_str," (",num_cycles," cycles)")
    
    for (station in stations_oi) {
    
      cc_fcst <- c_fcst %>% dplyr::filter(get(station_group_var) == station)
      
      subtitle_str  <- paste0("Used {",paste0(cyc_used_fig,collapse = ","),
                              "} +",paste0(lt_used_fig,collapse = ", ")," : ",
                              station," stations")
      
      # Define vroption list (with some dummy variables to be filled in later)
      vroption_list <- list("xgroup"   = "xgroup",
                            "score"    = "score",
                            "cycle"    = cycle,
                            "station"  = station,
                            "c_typ"    = "summary",
                            "c_ftyp"   = fxoption_list$c_ftyp,
                            "xg_str"   = "xg_str", 
                            "p_breaks" = "p_breaks",
                            "log_ind"  = FALSE)
      
      # Split by fcst_type
      if (fcst_type == "det") {
        
        # All aux scores
        fn_aux(cc_fcst,
               title_str,
               subtitle_str,
               fxoption_list,
               vroption_list,
               rolling_verif)
          
      } else if (fcst_type == "ens") {
        
        # Compute scores for the ensemble mean
        cc_fcst_mean <- cc_fcst %>% dplyr::filter(member == "mean")
        c_title_str  <- paste0("Ensemble mean : ",title_str)
        fn_aux(cc_fcst_mean,
               c_title_str,
               subtitle_str,
               fxoption_list,
               vroption_list,
               rolling_verif)
        
        # Compute scores for the control members
        if (compare_mbr000) {
          cc_fcst_ctrl <- cc_fcst %>% dplyr::filter(member == "mbr000")
          c_title_str  <- paste0("Mbr000 : ",title_str)
          fn_aux(cc_fcst_ctrl,
                 c_title_str,
                 subtitle_str,
                 fxoption_list,
                 vroption_list,
                 rolling_verif)
        }
        
        # Member scores (not really required)
        if (mbr_plots) {
          
          group_vars           <- c("fcst_model","valid_hour","member")
          vroption_list$score  <- "mbrdailyvar"
          vroption_list$xgroup <- "valid_hour"
          vroption_list$xg_str <- "vh"
          fn_dvar_ts(cc_fcst,
                     group_vars,
                     title_str,
                     subtitle_str,
                     fxoption_list,
                     vroption_list)
          
          group_vars           <- c("fcst_model","valid_dttm","member")
          vroption_list$score  <- "mbrtimeseries"
          vroption_list$xgroup <- "valid_dttm"
          vroption_list$xg_str <- "vd"
          fn_dvar_ts(cc_fcst,
                     group_vars,
                     title_str,
                     subtitle_str,
                     fxoption_list,
                     vroption_list)
          
          vroption_list$xgroup <- "NA"
          vroption_list$score  <- "mbrfreqdist"
          vroption_list$xg_str <- "NA";
          fn_freqdist(cc_fcst,
                      title_str,
                      subtitle_str,
                      fxoption_list,
                      vroption_list)
          
        }
        
      } # Det/eps
        
    } # station
    
  } # cycle
  
} # End of function
