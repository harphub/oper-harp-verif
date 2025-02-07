
#================================================#
# SURFACE AND UA PLOTTING FROM A HARP VERIF OBJECT
# 
# NOTE: THIS SCRIPT IS TO BE DEPRECATED AND 
# SHOULD BE USED WITH CAUTION!
#================================================#

fn_plot_point_verif <- function(harp_verif_input,
                                     png_archive,
                                     plot_num_cases = TRUE,
                                     cmap = "Set2",
                                     map_cbar_d = FALSE,
                                     table_SIDS = FALSE,
                                     compare_mbr000 = TRUE,
                                     png_projname = NA_character_,
                                     rolling_verif = FALSE,
                                     fsd = NA_character_,
                                     fed = NA_character_,
                                     plevel_filter = TRUE){
 
  #=================================================#
  # INITIAL CHECKS
  #=================================================#
  
  # Is the input a file path to an rds file or is it a harp verification object?
  if (is.list(harp_verif_input)) {
    verif       <- harp_verif_input
  } else {
    verif       <- readRDS(harp_verif_input)
  }
  scores_tables <- names(verif)

  if (all(grepl("det_",scores_tables))) {
    fcst_type <- "det"
    if ("fcst_model" %in% names(verif$det_threshold_scores)) {
      model_names <- unique(c(verif$det_summary_scores$fcst_model,
                              verif$det_threshold_scores$fcst_model))
    } else {
      model_names <- unique(verif$det_summary_scores$fcst_model)
    }
  } else if (any(grepl("ens_",scores_tables))) {
    fcst_type <- "ens"
    if ("fcst_model" %in% names(verif$ens_threshold_scores)) {
      model_names <- unique(c(verif$ens_summary_scores$fcst_model,
                              verif$ens_threshold_scores$fcst_model))
    } else {
      model_names <- unique(verif$ens_summary_scores$fcst_model)
    }

  } else {
    stop("Input does not look like a harpPoint verification object, aborting")
  }
  
  # Add in check on whether to split up threshold column
  verif <- fn_split_thr(verif)

  #=================================================#
  # USER INTERACTION REQUIRED HERE TO DEFINE
  # SCORES AND GROUPS TO PLOT 
  #
  # BY DEFAULT THIS WILL PRODUCE A SIMULAR SUITE OF 
  # PLOTS AS IN MONITOR
  #
  # THESE OPTIONS ARE SPLIT BY FCST_TYPE
  #================================================#
  
  # DETERMINISTIC EXP
  if (fcst_type == "det") {
    
    # NB: scores/groups should match the names available in the harp verification object.
    # If plotting multiple scores use score_sep, defined below, as a separator e.g.
    # "biasANDrmse", but still stick to the same base names.
    # Note: avoid using "~", ".", and "_" as score_sep. The first two can cause
    # problems in the figure name, while the latter is also used in the name of
    # harp scores. Default choice here is "AND".
    score_sep <- "AND"
    
    # START OF SURFACE SCORES
    
    # Scores as a fn of lead_time
    scores_lt  <- c(paste0("bias",score_sep,"rmse"),
                    paste0("bias",score_sep,"stde"))
    
    # Scores as a fn of valid_dttm
    scores_vd  <- c(paste0("bias",score_sep,"stde"))
    
    # Scores as a fn of threshold
    scores_th  <- c("threat_score",
                    "false_alarm_rate",
                    "false_alarm_ratio",
                    "kuiper_skill_score",
                    "frequency_bias",
                    "equitable_threat_score",
                    "extreme_dependency_score")
    
    # Map scores (combining scores won't work here)
    scores_mp  <- c("bias","rmse")
    
    # Lead_times to plot when looking at scores_th
    #tleadtimes <- c("24","48","All")
    tleadtimes  <- c("All")
    
    # Validhours to look at for map scores 
    # (leave empty to use all hours available in the data)
    #mpvalidhours <- c()
    mpvalidhours <- c("All","00","12") 
    if (rolling_verif) {
      mpvalidhours <- c("All","00","06","12","18") 
    }
    
    # Cycles to consider when plotting:
    # summary
    # threshold scores as fn of lead_time
    # threshold scores as fn of threshold)
    cycles_summary      <- c("00","12","All")
    cycles_threshold_lt <- c("All")
    cycles_threshold_th <- c("00","12","All")
    
    # END OF SURFACE SCORES
    
    # START OF UA SCORES
    # Scores as a fn of lead_time
    p_scores_lt <- c(paste0("bias",score_sep,"rmse"))
    
    # Profile scores
    p_scores_pr <- c(paste0("bias",score_sep,"stde"))
    
    # END OF UA SCORES
    
    # NB: What groups are considered? 
    # This controls what group to look for in the verif object and 
    # then do something sensible from there).
    # There are several options available (note that the "p_*" 
    # refers to UA plotting):
    # 1) "lead_time", "valid_dttm", "threshold", "p_leadtime"
    #   (here the groups are used for the x-axis)
    # 2) "SID" - For map scores
    # 3) "p_prof" - For UA profiles
    # 4) "other" - Special ens scores using by harp's plot_point_verif
    # In general one can leave xgroups as all of the above; if the 
    # data does not exist in the verif object, it will be skipped
    
    xgroups    <- c("lead_time",
                    "valid_dttm",
                    "threshold_val",
                    "SID",
                    "p_leadtime",
                    "p_prof")
    if (rolling_verif) {
      xgroups  <- c("lead_time","valid_dttm","SID")
    }

    # EPS EXPERIMENT
  } else if (fcst_type == "ens") {
    
    score_sep <- "AND"
    
    # START OF SURFACE SCORE
    # Ensemble scores as a fn of lead_time
    scores_lt  <- c(paste0("mean_bias",score_sep,"rmse"),
                    paste0("mean_bias",score_sep,"stde"),
                    paste0("rmse",score_sep,"spread"),
                    "crps",
                    "fair_crps",
                    "spread_skill_ratio")
    
    # Ensemble scores as a fn of valid_dttm
    scores_vd  <- c(paste0("mean_bias",score_sep,"stde"))
    
    # Ensemble scores as a fn of thresholds
    scores_th  <- c("brier_score",
                    "brier_skill_score",
                    "roc_area")
    
    # Some of the most relevant scores:
    #scores_th    <- c("fair_brier_score",
    #                  "brier_score",
    #                  "brier_skill_score",
    #                  "brier_score_reliability",
    #                  "brier_score_resolution",
    #                  "brier_score_uncertainty",
    #                  "roc_area")
    # TODO: include option for brier score decomposition
    
    # Other scores (this covers rank_histogram, reliability, roc,
    # and economic_value which have to be treated seperately)
    scores_ot  <- c("rank_histogram","roc","reliability")
    
    # Map scores
    scores_mp  <- c("mean_bias","rmse")
    
    # Finally, add in what member specific scores to consider.
    # The naming convention here is is important: For control member 
    # comparison, use prefix "ctrl" e.g. "ctrlbias"score_sep"rmse" or
    # "ctrlmae" (here scores can be combined with "score_sep").
    # For all members, use prefix "mbr" e.g. "mbrbias" which will 
    # plot all members and facet by model (here scores
    # cannot be combined with "score_sep")
    scores_lt  <- c(scores_lt,"mbrbias","mbrrmse")
    scores_vd  <- c(scores_vd,"mbrbias","mbrrmse")
    
    if (compare_mbr000) {
      scores_lt <- c(scores_lt,
                     paste0("ctrlbias",score_sep,"rmse"),
                     paste0("ctrlbias",score_sep,"stde"))
      scores_vd <- c(scores_vd,
                     paste0("ctrlbias",score_sep,"stde"))
      scores_mp <- c(scores_mp,
                     "ctrlbias",
                     "ctrlrmse")
    }
    
    # Lead_times to plot when looking at scores_th
    #tleadtimes <- c(seq(12,48,12),"All")
    tleadtimes <- c("24","48","All")
    
    # Validhours to look at for map scores (leave empty to use 
    # all hours available in the data)
    #mpvalidhours <- c()
    mpvalidhours <- c("All","00","12") 
    if (rolling_verif) {
      mpvalidhours <- c("All","00","06","12","18") 
    }
    
    # Cycle to consider when plotting:
    # summary
    # threshold scores as fn of lead_time
    # threshold scores as fn of threshold
    cycles_summary      <- c("00","12","All")
    cycles_threshold_lt <- c("All")
    cycles_threshold_th <- c("00","12","All")
    
    # END OF SURFACE SCORES
    
    # START OF UA SCORES
    # Scores as a fn of lead_time
    p_scores_lt <- c(paste0("mean_bias",score_sep,"rmse"))
    
    # Profile scores
    p_scores_pr <- c(paste0("mean_bias",score_sep,"stde"),
                     paste0("rmse",score_sep,"spread"),
                     "crps",
                     "fair_crps")
    
    if (compare_mbr000) {
      p_scores_lt <- c(p_scores_lt,
                       paste0("ctrlbias",score_sep,"rmse"))
      p_scores_pr <- c(p_scores_pr,
                       paste0("ctrlbias",score_sep,"stde"))
    }
    
    # END OF UA SCORES
    
    # What groups are considered? (see description in "det" section above) 
    xgroups    <- c("lead_time",
                    "valid_dttm",
                    "threshold_val",
                    "SID",
                    "other",
                    "p_leadtime",
                    "p_prof")
    if (rolling_verif) {
      xgroups  <- c("lead_time","valid_dttm","SID")
    }

  }
  
  #================================================#
  # END OF USER INTERACTION
  # NORMALLY NO NEED TO EDIT BELOW THIS
  #================================================#
  
  #================================================#
  # SOME USEFUL ATTRIBUTES AND DEFINITIONS
  #================================================#
  
  # Get useful attributes
  num_models   <- length(model_names)
  param        <- attr(verif,"parameter")
  # Add option for fixed start/end dates in directory and png names
  dttm_avail   <- attributes(harp_verif_input)$dttm
  dttm_avail   <- dttm_avail[order(dttm_avail)]
  num_cycles_a <- length(unique(dttm_avail))
  
  # Get start and end dates
  d_out      <- get_sedate(fsd,
                           fed,
                           dttm_avail[1],
                           tail(dttm_avail,1))
  sdate      <- d_out$sdate
  edate      <- d_out$edate
  tsdate     <- d_out$tsdate
  tedate     <- d_out$tedate

  # Note: The unit of the parameter is not included in the harp verif
  # object by default. It should be manually added as an extra 
  # attribute upstream. If not, it will be set to empty
  par_unit     <- attr(verif,"par_unit") 
  if (is.null(par_unit)) {
    par_unit <- " "
  }
  
  # Check png_archive for s/edate
  png_archive <- check_png_sedate(sdate,edate,png_archive)
  
  # Ens scores to be dealt with separately (using harp's plot_point_verif)
  ens_spec   <- c("rank_histogram",
                  "reliability",
                  "roc",
                  "economic_value")
  
  # Title string
  title_date_str = suppressMessages(paste0(stringr::str_to_title(param)," : ",
              format(harpIO::str_datetime_to_datetime(tsdate),"%Y-%m-%d-%H")," - ",
              format(harpIO::str_datetime_to_datetime(tedate),"%Y-%m-%d-%H")))
  
  # More useful attributes
  all_summary_scores   <- names(verif[[paste0(fcst_type,"_summary_scores")]])
  all_threshold_scores <- names(verif[[paste0(fcst_type,"_threshold_scores")]])
  threshold_vals       <- unique(verif[[paste0(fcst_type,"_threshold_scores")]][["threshold_val"]])
  
  #================================================#
  # FIGURE OPTIONS
  #================================================#
  
  # Define some figure widths/heights
  fw        <- 7
  fh        <- 4.5
  fw_map    <- 10
  fh_map    <- 4.5
  fig_units <- "in"
  fig_dpi   <- 200
  if (num_models == 3) {
    fw_map  <- 12
  } else if (num_models > 3) {
    warning("Width/height for map plotting may not be optimal!\n")
    fw_map  <- 12
    fh_map  <- 7.5
  }
  
  # Define the colour scheme used in line plots
  mcolors <- fn_gen_model_colors(model_names,cmap)
  
  # Line stylces
  line_styles <- c("solid","dashed","dotted","dotdash")
  
  # Some sizes
  line_size   <- 1
  point_size  <- 0.5
  stroke_size <- 1.0
  
  # Define various themes
  ptheme_l <- ggplot2::theme_bw() + 
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 10, margin = margin(1,0,1,0)),
      plot.subtitle   = ggplot2::element_text(size = 8, margin = margin(1,0,0,0)),
      axis.text       = ggplot2::element_text(size = 8),
      axis.title      = ggplot2::element_text(size = 8, margin = margin(0,0,0,0)),
      strip.text      = ggplot2::element_text(size = 8),
      legend.text     = ggplot2::element_text(size = 8),
      legend.key.spacing.x = unit(1,"lines"),
      legend.box.spacing   = unit(0,"pt"),
      legend.margin        = margin(0,0,0,0),
      legend.box.margin    = margin(0,0,0,0),
      legend.position = "top"
    )
  ptheme_nc <- ggplot2::theme_bw() + 
    ggplot2::theme(
      axis.text       = ggplot2::element_text(size = 8),
      axis.title      = ggplot2::element_text(size = 8, margin = margin(0,0,0,0)),
      legend.position = "none"
    )
  
  #=====================================================#
  # LIST OF FIXED VALUES (USED FOR PASSING TO FUNCTIONS)
  #=====================================================#
  
  fxoption_list <- list("param"        = param,
                        "sdate"        = sdate,
                        "edate"        = edate,
                        "num_models"   = num_models,
                        "par_unit"     = par_unit,
                        "fw"           = fw,
                        "fh"           = fh,
                        "fw_map"       = fw_map,
                        "fh_map"       = fh_map,
                        "mcolors"      = mcolors,
                        "map_cbar_d"   = map_cbar_d,
                        "ens_spec"     = ens_spec,
                        "line_styles"  = line_styles,
                        "line_size"    = line_size,
                        "stroke_size"  = stroke_size,
                        "point_size"   = point_size,
                        "ptheme_l"     = ptheme_l,
                        "ptheme_nc"    = ptheme_nc,
                        "png_archive"  = png_archive,
                        "fig_units"    = fig_units,
                        "fig_dpi"      = fig_dpi,
                        "png_projname" = png_projname,
                        "score_sep"    = score_sep, 
                        "comp_val"     = attributes(verif)$comp_val,
                        "thr_brks"     = attributes(verif)$thr_brks)
  
  #=====================================================#
  # NOW DO THE PLOTTING
  # "xgroup" DECIDES WHAT HAPPENS HERE
  # THIS INCLUDES ALL SUMMARY, THREHSOLD, VERTICAL PLOTS
  #=====================================================#
  
  # Loop structure is:
  # xgroup
  #   score
  #     fcst_cycle
  #       station group/domain

  # Set verif_input as the original verif object
  verif_input <- verif
  
  for (xgroup in xgroups) {
    
    c_ind <- TRUE # Just a counter for printing
    
    # Define some options based on the x-axis
    if (xgroup == "lead_time") {
      xg_str <- "lt"
      scores <- scores_lt
    } else if (xgroup == "valid_dttm") {
      xg_str <- "vd"
      scores <- scores_vd
    } else if (xgroup == "valid_hour") {
      xg_str <- "vh"
      scores <- sscores_vh
    } else if (xgroup == "threshold_val") {
      xg_str <- "th"
      scores <- scores_th
    } else if (xgroup == "other") {
      xg_str <- "NA"
      scores <- scores_ot
    } else if (xgroup == "SID") {
      xg_str <- "mp"
      scores <- scores_mp
    } else if (xgroup == "p_leadtime") {
      xg_str <- "lt"
      scores <- p_scores_lt
    } else if (xgroup == "p_prof") {
      xg_str <- "pr"
      scores <- p_scores_pr
    } else {
      stop("The xgroup ",xgroup," is not considered! Aborting")
    }

    for (score in scores) {
      
      # Summary or threshold table?
      if ((grepl("ctrl",score)) || (grepl("mbr",score))) {
        # Looking at member det scores 
        c_typ       <- "summary"
        c_ftyp      <- "det"
        score_orig  <- score
        score       <- gsub("mbr","",score)
        score       <- gsub("ctrl","",score)
        cycles_oi   <- cycles_summary
      } else if (all(strsplit(score,score_sep)[[1]] %in% all_threshold_scores)) {
        c_typ       <- "threshold"
        c_ftyp      <- fcst_type
        score_orig  <- score
        if (xgroup == "threshold_val") {
          cycles_oi <- cycles_threshold_th
        } else {
          cycles_oi <- cycles_threshold_lt
        }
      } else if (all(strsplit(score,score_sep)[[1]] %in% all_summary_scores)) {
        c_typ       <- "summary"
        c_ftyp      <- fcst_type
        score_orig  <- score
        cycles_oi   <- cycles_summary
      } else { 
        c_typ       <- NA_character_ # Score is not present and will be skipped
        c_ftyp      <- fcst_type
        score_orig  <- score
        cycles_oi   <- cycles_summary
        #warning(score," is not in the verif table and will be skipped")
      }
      
      # Check for stations/cycles/lts/vhs available in the verif object
      c_tstr           <- paste0(c_ftyp,"_",c_typ,"_scores")
      c_ver            <- verif_input[[c_tstr]]
      cnames           <- names(c_ver)
      
      # Need to add an additional filter in order to get the correct cycles
      # when multiple xgroups are available. Not relevant for UA variables
      # as fcst_cycle is not a grouping variable.
      # This ensures that values relevant to this xgroup are used
      if ((xgroup %in% c("lead_time","valid_hour","valid_dttm","threshold_val")) &
          (xgroup %in% cnames)) {
        c_ver          <- c_ver %>% dplyr::filter(get(xgroup) != "All")
      }
      if (is.null(cnames)) {
        cnames         <- "MISSING"
      }
      tmp_out          <- fn_check_verif(c_ver,verif_input,cnames)
      verif            <- tmp_out$verif
      stations         <- tmp_out$stations
      allcycles        <- tmp_out$cycles
      cycles           <- allcycles
      # If fcst_cycle is just equal to "All", we can get all cycles used by
      # looking at verif$dttm
      if (!(any(allcycles != "All"))){
        if (!is.null(attributes(verif_input)$dttm)) {
          # Get all the cycles in here
          all_dttm  <- harpCore::as_YMDh(harpIO::str_datetime_to_datetime(attributes(verif_input)$dttm))
          allcycles <- sort(unique(substr(all_dttm,9,10)))
        }
      }
      if (length(cycles) > 1) {
        cycles         <- base::intersect(cycles,cycles_oi)
      }
      leadtimes        <- tmp_out$leadtimes
      leadtime_vals    <- base::intersect(tleadtimes,leadtimes)
      # If all_lts_avail is an attribute, we can also use this in the subtitle
      if (!is.null(attributes(verif_input)$all_lts_avail)) {
        alta <- attributes(verif_input)$all_lts_avail
        if (length(alta) > 5) {
          lt_used_fig <- c(alta[1],
                           alta[2],
                           "... ",
                           alta[length(alta)-1],
                           alta[length(alta)])
          lt_used_fig <- paste0(lt_used_fig,collapse = ", ")
        } else {
          lt_used_fig <- paste0(alta,collapse = ", ")
        }
      } else {
        alta <- NULL
      }
      allvalidh        <- tmp_out$validhours
      validhours       <- allvalidh
      if ((!is.na(validhours[1])) & (length(mpvalidhours) > 0)) {
        validhours     <- base::intersect(validhours,mpvalidhours)
        if (length(validhours) == 0) {
          oavh         <- unique(stringr::str_trim(allvalidh))
          validhours   <- oavh
          warning("Fringe case where only one validhour ",oavh," exists")
        }
      }
      station_group_var <- tmp_out$station_group_var
      rm(tmp_out)
      
      for (cycle in cycles) {
        
        verif_I <- harpPoint::filter_list(verif,fcst_cycle == cycle)

        if (cycle == "All") {
          if (any(allcycles != "All")) {
            ac_str <- sort(setdiff(allcycles,"All"))
            cy_str <- paste0(paste(ac_str,collapse = ","),
                             "Z cycles")
          } else {
            cy_str <- "All cycles"
          }
          num_cycles <- num_cycles_a
        } else {
          cy_str <- paste0(cycle,"Z cycle")
          # Get the correct number of cycles used!
          used_dttm  <- dttm_avail[substr(dttm_avail,9,10) %in% cycle]
          num_cycles <- length(unique(used_dttm))
        }
        # Title string
        title_str = paste0(title_date_str," (",num_cycles," cycles)")
        
        for (station in stations) {
          
          # Variable options
          vroption_list <- list("xgroup"  = xgroup,
                                "score"   = score,
                                "cycle"   = cycle,
                                "station" = station,
                                "c_typ"   = c_typ,
                                "c_ftyp"  = c_ftyp,
                                "xg_str"  = xg_str)
            
          verif_II <- harpPoint::filter_list(verif_I,
                                             get(station_group_var) == station)
          # Filter in two parts in case this xgroup does not exist!
          verif_II <- harpPoint::filter_list(verif_II,
                                             get(xgroup) != "All")
          # Now do a final check to make sure some data exists
          if (!is.null(verif_II[[c_tstr]])) {
            if (nrow(verif_II[[c_tstr]]) == 0) {
              cat("No data found for stations",station,", cycle",cycle,", and xgroup",xgroup,", skipping!\n")
              next
            }
          }
          if (("valid_dttm" %in% names(verif_II[[1]])) &
              (xgroup == "valid_dttm")) {
            verif_II <- verif_II %>%
              harpPoint::mutate_list(valid_dttm = harpCore::as_dttm(gsub("-| |:|UTC",
                                                                         "",
                                                                         valid_dttm)))
            # Only plot 3 hourly data
            avddtm   <- harpCore::unique_valid_dttm(verif_II[[1]])
            exddtm   <- harpCore::as_dttm(harpCore::seq_dttm(avddtm[1],
                                                             tail(avddtm,1),
                                                             by = "3h"))
            verif_II <- verif_II %>%
              harpPoint::filter_list(valid_dttm %in% exddtm)
            
          }

          # Don't use attributes(stations) as this contains all stations, 
          # not just station_group stations
          if (xgroup == "SID") {
            num_stations <- length(unique(verif_II[[1]][["SID"]]))
          } else {
            num_stations <- max(verif_II[[1]][["num_stations"]])
          }
          if (numbers_only(station)) {
            subtitle_str <- paste0("Station ",station," (",num_stations,") : ",cy_str)
          } else {
            subtitle_str <- paste0(station," stations (",num_stations,") : ",cy_str)
          }
          # Add in lead times used for valid_dttm
          if ((xgroup %in% c("valid_dttm")) & (!is.null(alta))) {
            subtitle_str <- paste0(subtitle_str,": +",lt_used_fig)
          }

          # Does the score+xgroup exist in table? Also split according to xgroup
          # This works for standard threshold and summary scores
          if ((!(xgroup %in% c("SID"))) & (xgroup %in% cnames) &
              (!("p" %in% cnames)) &
              (all(strsplit(score,score_sep)[[1]] %in% cnames))) {
            
            if (c_ind) {
              cat("Plotting for xgroup:",xgroup,"\n")
              c_ind <- FALSE
            }
            
            # Plot
            if (c_typ == "threshold") {
              
              # Here we are either plotting as a fn of lead_time or threshold
              if (xgroup == "threshold_val") {
                loop_values <- leadtime_vals
                filter_col  <- "lead_time"
                verif_III   <- verif_II %>%
                  harpPoint::filter_list(valid_hour == "All",
                                         valid_dttm == "All")
              } else if (xgroup == "lead_time") {
                loop_values <- threshold_vals
                filter_col  <- "threshold_val"
                # Remove case where we have threshold scores over all leadtimes
                verif_III   <- harpPoint::filter_list(verif_II,
                                                      lead_time != "All",
                                                      valid_hour == "All",
                                                      valid_dttm == "All") 
              } 
              for (ii in loop_values) {
                verif_IIII <- harpPoint::filter_list(verif_III,
                                                     get(filter_col) == ii)
                c_subtitle <- paste0(subtitle_str," : ",
                                     stringr::str_to_title(filter_col)," = ",ii)
                if (xgroup == "threshold_val") {
                  vlt <- ii
                  vth <- "NA"
                  if ( (ii == "All") & (!is.null(alta))) {
                    c_subtitle <- paste0(subtitle_str," (",lt_used_fig,")")
                  }
                } else if (xgroup == "lead_time") {
                  vlt <- "NA"
                  if (ii < 0) {
                    vth <- paste0("m",abs(ii))
                  } else {
                    vth <- ii
                  }
                }
                if (nrow(verif_IIII[[c_tstr]]) == 0) {
                  next
                }
                p_c  <- fn_plot_point(verif_IIII,
                                      title_str,
                                      c_subtitle,
                                      fxoption_list,
                                      vroption_list,
                                      vlt = vlt,
                                      vth = vth)
                
                # Call numcases plot if desired
                if (plot_num_cases) {
                  p_numcases <- fn_plot_numcases(verif_IIII,
                                                 fxoption_list,
                                                 vroption_list)
                  p_c        <- fn_nc_combine(p_c,p_numcases)
                }
                # Save png
                fn_save_png(p_c           = p_c,
                            fxoption_list = fxoption_list,
                            vroption_list = vroption_list,
                            fcst_type     = fcst_type,
                            score         = score_orig,
                            vlt           = vlt,
                            vth           = vth)
              } # lt
              
            } else if (c_typ == "summary") { 
              verif_III <- verif_II
              
              if (grepl("ctrl",score_orig)) {
                verif_III[["det_summary_scores"]] <- dplyr::filter(verif_III[["det_summary_scores"]],
                                                                   member == "mbr000")
                c_title_str                       <- paste0("Mbr000 : ",title_str)
              } else if (grepl("mbr",score_orig)) {
                c_title_str         <- paste0("All Members : ",title_str)
                vroption_list$score <- score_orig
              } else {
                c_title_str <- title_str
              }
              
              if (nrow(verif_III[[c_tstr]]) == 0) {
                next
              }
              p_c <- fn_plot_point(verif_III,
                                   c_title_str,
                                   subtitle_str,
                                   fxoption_list,
                                   vroption_list)
  
              if ((plot_num_cases) & (!grepl("mbr",score_orig))) {
                p_numcases <- fn_plot_numcases(verif_III,
                                               fxoption_list,
                                               vroption_list)
                p_c        <- fn_nc_combine(p_c,p_numcases)
              }
              
              fn_save_png(p_c           = p_c,
                          fxoption_list = fxoption_list,
                          vroption_list = vroption_list,
                          fcst_type     = fcst_type,
                          score         = score_orig)
              
            } # if c_typ
           
          # Now look at the "other" group (only for surface variables)
          # Add in a check to make sure the threshold scores are non-empty
          } else if ((xgroup == "other") & (score %in% ens_spec) &
                     (!("p" %in% cnames)) & (!is.null(c_ver)) &
                     (!("SID" %in% cnames))) {
        
            verif_III <- harpPoint::filter_list(verif_II,
                                                valid_hour == "All",
                                                valid_dttm == "All")
            
            if (c_ind) {
              cat("Plotting for xgroup:",xgroup,"\n")
              c_ind <- FALSE
            }
            
            if (score == "rank_histogram") {
              if (nrow(verif_III[[c_tstr]]) == 0) {
                next
              }
              if (!is.null(alta)) {
                subtitle_str <- paste0(subtitle_str,": +",lt_used_fig)
              }
              p_out <- fn_plot_point(verif_III,
                                     title_str,
                                     subtitle_str,
                                     fxoption_list,
                                     vroption_list)
            } else {
              # Loop over all lead_time+threshold pairs
              for (lt in leadtime_vals) {
                for (th in threshold_vals) {
                  verif_IIII           <- verif_III
                  verif_IIII[[c_tstr]] <- dplyr::filter(verif_IIII[[c_tstr]],
                                                        lead_time == lt,
                                                        threshold_val == th)
                  if (th < 0) {
                    vth <- paste0("m",abs(th))
                  } else {
                    vth <- th
                  }
                  
                  # Check if this combination of lead_time+threhsold exists first!
                  if (nrow(verif_IIII[[c_tstr]]) > 0) {
                    if ( (lt == "All") & (!is.null(alta))) {
                      c_subtitle <- paste0(subtitle_str,
                                           "\n Leadtime = ",lt,
                                           " (",lt_used_fig,")",
                                           " : Threshold = ",th)
                    } else {
                      c_subtitle <- paste0(subtitle_str,
                                           "\n Leadtime = ",lt,
                                           " : Threshold = ",th)
                    }
                    if (nrow(verif_IIII[[c_tstr]]) == 0) {
                      next
                    }
                    p_out <- fn_plot_point(verif_IIII,
                                           title_str,
                                           c_subtitle,
                                           fxoption_list,
                                           vroption_list,
                                           vlt = lt,
                                           vth = vth)
                  }
                }
              }
            } # score
          
          # Map scores
          } else if ((xgroup == "SID") & (xgroup %in% cnames) &
                     (all(strsplit(score,score_sep)[[1]] %in% cnames))) {
            
            if (c_ind) {
              cat("Plotting for xgroup:",xgroup,"\n")
              c_ind <- FALSE
            }
            
            # Not sure if this will ever be used since SID and valid_hour are
            # currently grouped together. Hence in fn_check_verif, validhours
            # will be set to whatever is available.
            if (validhours[1] == "NA") { # I don't think this will ever happen
              # Average scores only
              c_subtitle <- paste0(subtitle_str," : Average")
              verif_III  <- verif_II
              if (grepl("ctrl",score_orig)) {
                verif_III[[c_tstr]] <- dplyr::filter(verif_III[[c_tstr]],
                                                     member == "mbr000")
                c_title_str         <- paste0("Mbr000 : ",title_str)
              } else {
                c_title_str <- title_str
              }
              if (nrow(verif_III[[c_tstr]]) == 0) {
                next
              }
              p_c <- fn_plot_map(verif_III,
                                 c_title_str,
                                 c_subtitle,
                                 fxoption_list,
                                 vroption_list)
              
              fn_save_png(p_c           = p_c,
                          fxoption_list = fxoption_list,
                          vroption_list = vroption_list,
                          fcst_type     = fcst_type,
                          score         = score_orig,
                          vlt           = "All",
                          map_ind       = TRUE)
              
            } else {
              
              for (vhour in validhours) {
                verif_III <- verif_II
                if (!is.null(alta)) {
                  lt_used    <- fn_get_lts_used(vhour,cycle,allcycles,alta)
                  c_subtitle <- paste0(subtitle_str,": ",lt_used)
                } else {
                  c_subtitle <- subtitle_str
                }
                if (vhour != "All") {
                  c_subtitle  <- paste0(c_subtitle," : Valid hour = ",vhour,"Z")
                } else {
                  c_subtitle  <- paste0(c_subtitle," : Average")
                }
                
                if (grepl("ctrl",score_orig)) {
                  verif_III[[c_tstr]] <- dplyr::filter(verif_III[[c_tstr]],
                                                       member == "mbr000",
                                                       valid_hour == vhour)
                  c_title_str <- paste0("Mbr000 : ",title_str)
                } else {
                  verif_III[[c_tstr]] <- dplyr::filter(verif_III[[c_tstr]],
                                                       valid_hour == vhour)
                  c_title_str <- title_str
                }
                if (nrow(verif_III[[c_tstr]]) == 0) {
                  next
                }
                p_c <- fn_plot_map(verif_III,
                                   c_title_str,
                                   c_subtitle,
                                   fxoption_list,
                                   vroption_list)
                
                if (is.list(p_c)) {
                  fn_save_png(p_c           = p_c,
                              fxoption_list = fxoption_list,
                              vroption_list = vroption_list,
                              fcst_type     = fcst_type,
                              score         = score_orig,
                              vlt           = vhour,
                              map_ind       = TRUE)
                }
                
                # Optional SID table output (for debugging/filtering stations)
                # Lists 10 stations with largest RMSE
                if ((table_SIDS) & (score == "rmse")) {
                  fn_sid_issue_table(verif_III,
                                     fxoption_list,
                                     vroption_list,
                                     vlt = vhour)
                }
                
              } # validhour
              
            } # if validhour exists 
              
          # UA scores
          } else if ((xgroup %in% c("p_leadtime","p_prof")) &
                     (all(strsplit(score,score_sep)[[1]] %in% cnames)) &
                     ("p" %in% cnames)) {
           
            if ((xgroup == "p_leadtime") & ("lead_time" %in% cnames)) {
              
              if (c_ind) {
                cat("Plotting for xgroup:",xgroup,"\n")
                c_ind <- FALSE
              }
              
              vroption_list$xgroup <- "lead_time"
              
              # Get the available pressure levels
              p_label <- "hPa"
              if (plevel_filter) {
                p_levels <- intersect(unique(verif_II[[1]][["p"]]),
                                      c(100,150,200,300,400,500,700,850,925,1000))
              } else {
                p_levels <- unique(verif_II[[1]][["p"]])
                if (max(p_levels) < 100) {
                  p_label <- " Channel"
                }
              }
              
              # Loop over pressure levels and plot summary scores
              for (pl in p_levels) {
                
                verif_III    <- harpPoint::filter_list(verif_II,
                                                       p == pl,
                                                       valid_hour == "All")
                param_rename <- paste0(stringr::str_to_title(param),pl)
                c_title_str  <- gsub(paste0(stringr::str_to_title(param)," : "),
                                     paste0(param_rename,p_label," : "),title_str)
                
                if (grepl("ctrl",score_orig)) {
                  verif_III[[c_tstr]] <- dplyr::filter(verif_III[[c_tstr]],
                                                       member == "mbr000")
                  c_title_str <- paste0("Mbr000 : ",c_title_str)
                }
                
                if (nrow(verif_III[[c_tstr]]) == 0) {
                  next
                }
                p_c <- fn_plot_point(verif_III,
                                     c_title_str,
                                     subtitle_str,
                                     fxoption_list,
                                     vroption_list)
                
                if ((plot_num_cases) & (!grepl("mbr",score_orig))) {
                  p_numcases <- fn_plot_numcases(verif_III,
                                                 fxoption_list,
                                                 vroption_list)
                  p_c        <- fn_nc_combine(p_c,p_numcases)
                }
                
                fn_save_png(p_c           = p_c,
                            fxoption_list = fxoption_list,
                            vroption_list = vroption_list,
                            fcst_type     = fcst_type,
                            score         = score_orig,
                            vlt           = pl)

              } # pressure levels
              
            } # if p_leadtime
            
            if ((xgroup == "p_prof") & ("valid_hour" %in% cnames)) {
              
              if (c_ind) {
                cat("Plotting for xgroup:",xgroup,"\n")
                c_ind <- FALSE
              }
              
              vhours <- base::intersect(allvalidh,c("00","12"))

              if (length(vhours) > 0) {
              for (vh in vhours) {
                
                # If vh = "All", only run for fcst_cycle = "All"
                if ((vh == "All") & (cycle != "All")) {
                  next
                }
                verif_III    <- harpPoint::filter_list(verif_II,
                                                       valid_hour == vh,
                                                       lead_time == "All")
                if (grepl("ctrl",score_orig)) {
                  verif_III[[c_tstr]] <- dplyr::filter(verif_III[[c_tstr]],
                                                       member == "mbr000")
                  c_title_str <- paste0("Mbr000 : ",title_str)
                } else {
                  c_title_str <- title_str
                }
              
                # Add in lead times used
                lt_used <- fn_get_lts_used(vh,cycle,allcycles,leadtimes)
                c_subtitle <- paste0(subtitle_str,": ",lt_used)
                
                if (vh != "All") {
                  c_subtitle  <- paste0(c_subtitle," : Valid hour = ",vh,"Z")
                } else {
                  c_subtitle  <- paste0(c_subtitle," : Average")
                }
                
                if (nrow(verif_III[[c_tstr]]) == 0) {
                  next
                }
                p_c <- fn_plot_profile(verif_III,
                                       c_title_str,
                                       c_subtitle,
                                       plot_num_cases,
                                       fxoption_list,
                                       vroption_list)
                
                fn_save_png(p_c           = p_c,
                            fxoption_list = fxoption_list,
                            vroption_list = vroption_list,
                            fcst_type     = fcst_type,
                            score         = score_orig,
                            vlt           = vh)

              } # validhour
              } else {
                warning("Profile plotting skipped as 00,12Z were not found")
              }

            } # if p_prof
            
          } else { # If xgroup+score exists
        
            #cat("Combination xgroup=",xgroup,"and score=",score," not found"))
            
          } # If xgroup exists
        } # station
      } # cycle
    } # Scores
  } # xgroup

} # End of function
