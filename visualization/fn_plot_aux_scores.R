
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
                                    lt_split = FALSE,
                                    plot_fd = FALSE,
                                    plot_scat = TRUE,
                                    use_parallel = FALSE,
                                    clim_name = NULL,
                                    clim_in_plots = FALSE,
                                    lt_to_use = seq(3,48,3),
                                    vc = NA_character_,
                                    fsd = NA_character_,
                                    fed = NA_character_){
  
  #=================================================#
  # INITIAL CHECKS 
  #=================================================#
  
  if (!is.na(vc)) {
    if (is.null(clim_name)) {
      cat("Skipping fn_plot_aux_scores for this UA parameter\n")
      return()
    } else {
      cat("Running fn_plot_aux_scores for this UA parameter as climatology was found!\n")
    }
  }
  
  if (!is.list(fcst_input)) {
    stop("Aux scores plotting: A harp forecast object is required, aborting")
  }
  
  station_group_var <- "station_group"
  models_vec        <- names(fcst_input)
  num_models        <- length(models_vec)
  if (clim_in_plots) {
    num_models_plot <- num_models 
  } else {
    num_models_plot <- models_vec[!grepl("climatology",models_vec,fixed=T)] %>% length()
  }
  
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
  # If valid_ymd exists, use this as the grouping variable
  if ("valid_ymd" %in% fcst_names) {
    valid_dttm_col <- "valid_ymd"
  } else {
    valid_dttm_col <- "valid_dttm"
  }
  
  # Useful variables
  param    <- unique(fcst[["parameter"]])
  # Need a workaround for AccPcp
  if (param == "Pcp") { 
    all_fcst_names <- names(fcst)
    param          <- all_fcst_names[grepl("AccPcp",all_fcst_names,fixed = TRUE)]
    if (length(param) != 1) {
      cat("Found the following for param in fn_plot_aux_score:")
      print(param)
      stop("Error in param name for AccPcp")
    }
  }
  # Need a workaround for single UA levels e.g. T925
  if (!(param %in% names(fcst))) {
    all_fcst_names <- names(fcst)
    param          <- all_fcst_names[grepl(param,all_fcst_names,fixed = TRUE)]
    if (length(param) != 1) {
      cat("Found the following for param in fn_plot_aux_score:")
      print(param)
      stop("Error when looking for single UA level")
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
  
  #lt_to_use   <- seq(3,48,3) # What lead_times to use (hours)?
  if (!is.null(attr(fcst_input,"lt_unit"))){
    if (attr(fcst_input,"lt_unit") != "h") {
      stop("Lead time generalisation required - raise an issue!")
    }
  }
  all_lts     <- sort(unique(fcst[["lead_time"]]))
  lt_to_use   <- intersect(all_lts,lt_to_use)
  if (length(lt_to_use) > 0) {
    cat("Using lead times",paste(lt_to_use,collapse = ","),"in aux plots\n")
  } else {
    cat("Only found lead times",paste(all_lts,collapse = ","),"\n")
    cat("Do not filter lead times in aux plotting\n")
    lt_to_use <- all_lts
  }
  # Introduce a split between "short" and "long" leadtimes
  if ((max(lt_to_use) > 24) & (lt_split)) {
    lt_to_use_short <- lt_to_use[lt_to_use <= 24]
    lt_to_use_long  <- lt_to_use[lt_to_use > 24]
    lt_to_use_list  <- list("lt_all"   = lt_to_use,
                            "lt_short" = lt_to_use_short,
                            "lt_long"  = lt_to_use_long)
  } else {
    lt_to_use_list  <- list("lt_all" = lt_to_use)
  }
  # If climatology exists, use all lead times for forecast activity
  if (!is.null(clim_name)) {
    if (!(all(all_lts %in% lt_to_use))) {
      lt_to_use_list  <- c(lt_to_use_list,list("lt_clm" = all_lts))
    }
  }
  cycles_oi   <- c("All","00","12") # What cycles to plot for?
  if (rolling_verif) {
    cycles_oi <- c("All","00","12")
  }
  cycles_oi   <- base::intersect(cycles_oi,c("All",cycles))
  # If there is only one cycle in the forecast data, then just use "All" (to avoid double jobs)
  if (length(cycles) == 1) {
    cycles_oi <- "All"
  }
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
  # Specific single leadtimes to plot for a scatterplot
  scat_single_lts <- seq(24,48,12)
  if (any(scat_single_lts %in% lt_to_use)) {
    scat_single_lts <- lt_to_use[lt_to_use %in% scat_single_lts]
    # Extra check in the case of a single leadtime
    if (length(lt_to_use) == 1){
      scat_single_lts <- NULL
    }
  } else {
    scat_single_lts <- NULL
  }
  
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
  # If cmap is a named list, then it comes from a mc_file. If not, then
  # use the standard (old) method
  if (is.null(names(cmap))) {
    mcolors     <- fn_gen_model_colors(models_vec,cmap,withobs = TRUE)
  } else {
    mcolors     <- fn_populate_model_colors(models_vec,cmap,withobs = TRUE)
  }
  
  # Line stylces
  line_styles <- c("solid","dashed","dotted","dotdash")
  
  # Some sizes
  line_size   <- 1
  point_size  <- 0.5
  stroke_size <- 1.0
  
  # Score spearator
  score_sep <- "AND"

  # Define various themes
  ptheme_l <- ggplot2::theme_bw() + 
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 10, margin = margin(1,0,1,0)),
      plot.subtitle   = ggplot2::element_text(size = 8, margin = margin(1,0,0,0)),
      axis.text       = ggplot2::element_text(size = 8),
      axis.title      = ggplot2::element_text(size = 8, margin = margin(0,0,0,0)),
      strip.text      = ggplot2::element_text(size = 8),
      legend.text     = ggplot2::element_text(size = 8),
      legend.box.spacing   = unit(0,"pt"),
      legend.margin        = margin(0,0,0,0),
      legend.box.margin    = margin(0,0,0,0),
      legend.position = "top"
    )
  # legend.key.spacing.x only available for ggplot2 version 3.5.0
  if (as.numeric(gsub(".","",packageVersion("ggplot2"),fixed=T)) >= 350) {
    ptheme_l <- ptheme_l +
      ggplot2::theme(legend.key.spacing.x = unit(1,"lines"))
  }
  ptheme_nc <- ggplot2::theme_bw() + 
    ggplot2::theme(
      axis.text       = ggplot2::element_text(size = 8),
      axis.title      = ggplot2::element_text(size = 8, margin = margin(0,0,0,0)),
      legend.position = "none"
    )
  
  title_date_str = suppressMessages(paste0(stringr::str_to_title(param)," : ",
              format(harpIO::str_datetime_to_datetime(tsdate),"%Y-%m-%d-%H")," - ",
              format(harpIO::str_datetime_to_datetime(tedate),"%Y-%m-%d-%H")))
  
  fxoption_list <- list("param"             = param,
                        "sdate"             = sdate,
                        "edate"             = edate, 
                        "num_models"        = num_models_plot,
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
  
  fcst_all <- fcst
  p_list   <- list()
  nfa_data <- NULL
  
  # Add a loop over leadtime combinations
  for (jj in seq(1,length(lt_to_use_list))) {
    
  lt_jj     <- lt_to_use_list[[jj]]
  fcst      <- dplyr::filter(fcst_all,lead_time %in% lt_jj)
  leadtimes <- sort(unique(fcst[["lead_time"]]))
  if (length(leadtimes) > 1) {
    lt_used <- paste0(leadtimes[1],"_",leadtimes[length(leadtimes)])
  } else {
    lt_used <- leadtimes[1]
  }
  
  if (length(leadtimes) > 5) {
    lt_used_fig <- c(leadtimes[1],
                     leadtimes[2],
                     "... ",
                     leadtimes[length(leadtimes)])
  } else {
    lt_used_fig <- leadtimes
  }
  
  # If jj=1 (i.e. lt_to_use), then set lt_used to "All" and add in scatterplots at single leadtimes
  if (jj == 1) {
    scat_lts <- scat_single_lts
    lt_used  <- "All"
  } else {
    scat_lts <- NULL
  }
  
  for (cycle in cycles_oi) {
    
    if (cycle == "All") {
      cyc_used_fig <- cycles 
      c_fcst       <- fcst 
    } else {
      cyc_used_fig <- cycle
      c_fcst       <- fcst %>% dplyr::filter(fcst_cycle == cycle)
    }
    
    # Extra check in case leadtimes are not consistent over cycles
    if (nrow(c_fcst) == 0) {
      cat("Skipping aux plots for cycle",cycle,"and leadtimes",lt_used,"as no data found\n")
      next
    }
    
    num_cycles <- length(unique(c_fcst[["fcst_dttm"]])) 
    title_str = paste0(title_date_str," (",num_cycles," cycles)")
    
    for (station in stations_oi) {
    
      cc_fcst <- c_fcst %>% dplyr::filter(get(station_group_var) == station)
      
      # Extra check in case leadtimes+cycles are not consistent over stations
      if (nrow(cc_fcst) == 0) {
        cat("Skipping aux plots for cycle",cycle,"leadtimes",lt_used,"and station",station,"as no data found\n")
        next
      }
      
      subtitle_str  <- paste0("Used {",paste0(cyc_used_fig,collapse = ","),
                              "} +",paste0(lt_used_fig,collapse = ", ")," : ",
                              station," stations")
      
      # Define vroption list (with some dummy variables to be filled in later)
      vroption_list <- list("xgroup"   = "xgroup",
                            "score"    = "score",
                            "cycle"    = cycle,
                            "lt_used"  = lt_used,
                            "station"  = station,
                            "c_typ"    = "summary",
                            "c_ftyp"   = fxoption_list$c_ftyp,
                            "xg_str"   = "xg_str", 
                            "scat_lts" = scat_lts,
                            "p_breaks" = "p_breaks",
                            "log_ind"  = FALSE)
      
      # Split by fcst_type
      if (fcst_type == "det") {
        
        if (!is.null(clim_name)) {
          
          # Compute NFA if you are using all leadtimes possible
          if (all(all_lts %in% leadtimes)) {
            # Compute normalised forecast activity
            vroption_list$score   <- "nfa"
            vroption_list$xgroup  <- "lead_time"
            vroption_list$lt_used <- "NA"
            vroption_list$xg_str  <- "lt"
            nfa <- fn_nfa(cc_fcst,
                        title_str,
                        subtitle_str,
                        fxoption_list,
                        vroption_list,
                        clim_name,
                        vc = vc)
            nfa[["fcst_cycle"]] <- cycle
            nfa[[station_group_var]] <- station 
            if (is.null(nfa_data)) {
              nfa_data <- nfa
            } else {
              nfa_data <- dplyr::bind_rows(nfa_data,nfa)
            }
            
            # Reset
            vroption_list$score   <- "score"
            vroption_list$xgroup  <- "xgroup"
            vroption_list$lt_used <- lt_used
            vroption_list$xg_str  <- "xg_str"
            
            # Skip the rest of the below, no need to run for this set of leadtimes
            # However if we are on the first leadtime loop, then continue to below!
            # (case when all_lts are in lt_to_use)
            if (jj > 1) {
              next
            }
          }
          
          # Remove climatology from plots if specified
          if (!clim_in_plots) {
            cc_fcst <- cc_fcst %>% dplyr::filter(fcst_model != clim_name)
          }
          
        }
        
        # All aux scores
        if (is.na(vc)) {
        p <- fn_aux(cc_fcst,
               title_str,
               subtitle_str,
               fxoption_list,
               vroption_list,
               rolling_verif,
               plot_fd = plot_fd,
               plot_scat = plot_scat,
               use_parallel = use_parallel)
        p_list <- c(p_list,p)
        }
          
      } else if (fcst_type == "ens") {
        
        if (is.na(vc)){
        # Compute scores for the ensemble mean
        cc_fcst_mean <- cc_fcst %>% dplyr::filter(member == "mean")
        c_title_str  <- paste0("Ensemble mean : ",title_str)
        p <- fn_aux(cc_fcst_mean,
               c_title_str,
               subtitle_str,
               fxoption_list,
               vroption_list,
               rolling_verif,
               plot_fd = plot_fd,
               plot_scat = plot_scat,
               use_parallel = use_parallel)
        p_list <- c(p_list,p)
        
        # Compute scores for the control members
        if (compare_mbr000) {
          cc_fcst_ctrl <- cc_fcst %>% dplyr::filter(member == "mbr000")
          c_title_str  <- paste0("Mbr000 : ",title_str)
          p <- fn_aux(cc_fcst_ctrl,
                 c_title_str,
                 subtitle_str,
                 fxoption_list,
                 vroption_list,
                 rolling_verif,
                 plot_fd = plot_fd,
                 plot_scat = plot_scat,
                 use_parallel = use_parallel)
          p_list <- c(p_list,p)
        }
        
        # Member scores (not really required)
        if (mbr_plots) {
          
          group_vars           <- c("fcst_model","valid_hour","member")
          vroption_list$score  <- "mbrdailyvar"
          vroption_list$xgroup <- "valid_hour"
          vroption_list$xg_str <- "vh"
          p <- fn_dvar_ts(cc_fcst,
                     group_vars,
                     title_str,
                     subtitle_str,
                     fxoption_list,
                     vroption_list,
                     use_parallel = use_parallel)
          p_list <- c(p_list,p)
          
          group_vars           <- c("fcst_model",valid_dttm_col,"member")
          vroption_list$score  <- "mbrtimeseries"
          vroption_list$xgroup <- valid_dttm_col
          vroption_list$xg_str <- "vd"
          p <- fn_dvar_ts(cc_fcst,
                     group_vars,
                     title_str,
                     subtitle_str,
                     fxoption_list,
                     vroption_list,
                     use_parallel = use_parallel)
          p_list <- c(p_list,p)
          
          if (plot_fd) {
            vroption_list$xgroup <- "NA"
            vroption_list$score  <- "mbrfreqdist"
            vroption_list$xg_str <- "NA";
            fn_freqdist(cc_fcst,
                        title_str,
                        subtitle_str,
                        fxoption_list,
                        vroption_list)
          }
          
        }
        } # vc check
        
      } # Det/eps
        
    } # station
    
  } # cycle
  
  } # Loop over leadtime lists
  
  if (use_parallel) {
    fn_plot_parallel(p_list,fxoption_list)
  }
  
  out_list <- list()
  if (!is.null(nfa_data)) {
    out_list <- list("nfa" = nfa_data)
  }
  return(out_list)
} # End of function
