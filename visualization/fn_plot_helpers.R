
#================================================#
# HELPER FUNCTIONS FOR SURFACE AND UA PLOTTING
# 
# NOTE: THIS SCRIPT IS TO BE DEPRECATED AND 
# SHOULD BE USED WITH CAUTION!
#================================================#

numbers_only <- function(x) !grepl("\\D", x)

#================================================#
# HELPER FUNCTION TO COMBINE IMAGE WITH NUM_CASES
#================================================#

fn_nc_combine <- function(p,p_nc,h1=4,h2=1) {
  
  if (!exists("cowplot_available")) {
    if ("cowplot" %in% rownames(installed.packages())) {
      library(cowplot)
      cowplot_available <- T
    } else {
      cowplot_available <- F
    }
  }
  
  if (cowplot_available) {
    p_c <- cowplot::plot_grid(p,
                              p_nc,
                              align = "v",
                              ncol  = 1,
                              nrow  = 2,
                              rel_heights = c(h1/5,h2/5))
  } else {
    p_c <- gridExtra::grid.arrange(p,
                                   p_nc,
                                   ncol = 1,
                                   nrow = 2,
                                   heights = c(h1,h2))
  }
  
  return(p_c)
  
}

#================================================#
# HELPER FUNCTION TO DEFINE THE PNG NAME
#================================================#

fn_png_name <- function(fcst_type,
                        score,
                        xg_str,
                        param,
                        cycle,
                        sdate,
                        edate,
                        station,
                        vlt="NA",
                        vth="NA",
                        projectname=NA_character_){
  
  # Save pngs using the following file template:
  # PARAM-FTYPE-SCORE-XAXIS-FC-DTGSTART-DTGEND-STATION-VALID(LT)-THRESHOLD.png
  SCORE       <- paste0(fcst_type,"-",score,"-",xg_str)
  png_fname   <- paste(param,SCORE,cycle,sdate,edate,station,vlt,vth,sep = "-")
  png_fname   <- paste0(png_fname,".png")
  if (!is.na(projectname)) {
    png_fname <- paste0(projectname,"-",png_fname)
  }
  
  return(png_fname)
}

#================================================#
# HELPER FUNCTION FOR SAVING PNGS
#================================================#

fn_save_png <- function(p_c           = "",
                        fxoption_list = "",
                        vroption_list = "",
                        fcst_type     = "",
                        score         = "",
                        vlt           = "NA",
                        vth           = "NA",
                        map_ind       = FALSE){
  

  png_fname <- fn_png_name(fcst_type,
                           score,
                           vroption_list$xg_str,
                           fxoption_list$param,
                           vroption_list$cycle,
                           fxoption_list$sdate,
                           fxoption_list$edate,
                           vroption_list$station,
                           vlt = vlt,
                           vth = vth,
                           projectname = fxoption_list$png_projname)
  
  if (map_ind) {
    fw <- fxoption_list$fw_map
    fh <- fxoption_list$fh_map
  } else {
    fw <- fxoption_list$fw
    fh <- fxoption_list$fh
  }
  # Increase width for timeseries
  if (vroption_list$xg_str == "vd") {
    fw <- 10
  }
  
  ggplot2::ggsave(p_c,
                  filename = png_fname,
                  path     = fxoption_list$png_archive,
                  width    = fw,
                  height   = fh,
                  units    = fxoption_list$fig_units,
                  dpi      = fxoption_list$fig_dpi,
                  device   = 'png')
  
}

#================================================#
# GET START/END DATES FOR FOLDER AND TITLE
#================================================#

get_sedate <- function(fsd,
                       fed,
                       cy_start,
                       cy_end){
  
  if (is.character(cy_start)) {
    cy_start <- harpIO::str_datetime_to_datetime(cy_start)
    cy_end   <- harpIO::str_datetime_to_datetime(cy_end)
  }

  if ((!is.na(fsd)) & (!is.na(fed))) {
    sdate      <- fsd
    edate      <- fed
    tsdate     <- harpCore::as_YMDh(cy_start)
    tedate     <- harpCore::as_YMDh(cy_end)
  } else {
    sdate      <- harpCore::as_YMDh(cy_start)
    edate      <- harpCore::as_YMDh(cy_end)
    tsdate     <- sdate
    tedate     <- edate
  }

return(list("sdate"  = sdate,
            "edate"  = edate,
            "tsdate" = tsdate,
            "tedate" = tedate))

}

#================================================#
# CHECK THAT PNG ARCHIVE POINTS TO SDATE/EDATE
#================================================#

check_png_sedate <- function(sdate,
                             edate,
                             png_archive){
  
  d_end <- paste0(sdate,"-",edate)
  if (basename(png_archive) != d_end) {
    png_archive <- file.path(png_archive,d_end)
  }
  if (!dir.exists(png_archive)) {
    dir.create(file.path(png_archive),showWarnings = TRUE,recursive = FALSE)
  }
  
  return(png_archive)
  
}

#================================================#
# RETURN SOME INFORMATION FROM THE VERIF OBJECT
#================================================#

fn_check_verif <- function(df,
                           verif,
                           dfnames){
  
  # Check if "station_group" exists
  station_group_var = "station_group"
  if (station_group_var %in% dfnames) {
    stations <- unique(df[[station_group_var]])
  } else {
    stations <- c("All")
    verif <- verif %>% harpPoint::mutate_list("{station_group_var}" := "All") 
    # Note that {} and := are used to assign a variable to a column
  }
  
  # The same for fcst_cycle
  if ("fcst_cycle" %in% dfnames) {
    cycles <- unique(df[["fcst_cycle"]])
  } else {
    cycles <- c("All")
    verif  <- verif %>% harpPoint::mutate_list(fcst_cycle = "All")
  }
  
  # And check what leadtimes exist
  if ("lead_time" %in% dfnames) {
    leadtimes <- unique(df[["lead_time"]])
  } else {
    leadtimes <- NA_character_
  }
  
  # And check what validhours exist
  if ("valid_hour" %in% dfnames) {
    validhours <- unique(df[["valid_hour"]])
  } else {
    validhours <- NA_character_
  }
  return(list("verif"             = verif,
              "cycles"            = cycles,
              "stations"          = stations,
              "leadtimes"         = leadtimes,
              "validhours"        = validhours,
              "station_group_var" = station_group_var))
}

#================================================#
# HELPER FUNCTION FOR SIMPLE PLOTS
#================================================#

fn_plot_point <- function(verif,
                          title_str,
                          subtitle_str,
                          fxoption_list,
                          vroption_list,
                          vlt="NA",
                          vth="NA"){
  
  # Read options
  c_typ        <- vroption_list$c_typ
  c_ftyp       <- vroption_list$c_ftyp
  score        <- vroption_list$score
  xgroup       <- vroption_list$xgroup
  cycle        <- vroption_list$cycle
  station      <- vroption_list$station
  xg_str       <- vroption_list$xg_str
  line_size    <- fxoption_list$line_size
  stroke_size  <- fxoption_list$stroke_size
  ptheme_l     <- fxoption_list$ptheme_l
  mcolors      <- fxoption_list$mcolors
  line_styles  <- fxoption_list$line_styles
  par_unit     <- fxoption_list$par_unit
  ens_spec     <- fxoption_list$ens_spec
  png_archive  <- fxoption_list$png_archive
  sdate        <- fxoption_list$sdate
  edate        <- fxoption_list$edate
  fw           <- fxoption_list$fw
  fh           <- fxoption_list$fh
  param        <- fxoption_list$param
  num_models   <- fxoption_list$num_models
  point_size   <- fxoption_list$point_size
  fig_units    <- fxoption_list$fig_units
  fig_dpi      <- fxoption_list$fig_dpi
  png_projname <- fxoption_list$png_projname
  score_sep    <- fxoption_list$score_sep
  comp_val     <- fxoption_list$comp_val
  thr_brks     <- fxoption_list$thr_brks
  
  if (all(grepl("_scores",names(verif)))) {
    df <- verif[[paste0(c_ftyp,"_",c_typ,"_scores")]]
  } else {
    df <- verif
  }
  
  # x/y labels
  xl <- gsub("_"," ",stringr::str_to_title(xgroup))
  yl <- par_unit             
  if (xgroup == "threshold_val") {
    if (comp_val == "ge") {
      cvstr <- ">= Threshold"
    } else if (comp_val == "gt") {
      cvstr <- "> Threshold"
    } else if (comp_val == "le") {
      cvstr <- "<= Threshold"
    } else if (comp_val == "lt") {
      cvstr <- "< Threshold"
    } else if (comp_val == "eq") {
      cvstr <- "= Threshold"
    } else if (comp_val == "between"){
      cvstr <- "Classes (between)"
    } else if (comp_val == "outside") {
      cvstr <- "Classes (outside)"
    } else {
      stop("Comp_val abort!")
    }
    xl <- paste0(cvstr," (",par_unit,")")
    yl <- ""
  } else if (xgroup == "mids") {
    xl <- par_unit
    yl <- "Frequency"
    if ("freq_bias" %in% names(df)) {
      mcolors <- mcolors[names(mcolors) != "OBS"]
      yl <- ""
    }
  }
  
  # A check on the type of xgroup (in case of character type for 
  # lead_time and validhour)
  if ((xgroup %in% names(df)) & (is.character(df[[xgroup]])) &
      ((xgroup == "lead_time") || (xgroup == "valid_hour"))) {
    df[[xgroup]] <- as.numeric(df[[xgroup]])
  }
  
  # Split score into its individual parts
  all_scores   <- strsplit(score,score_sep)[[1]]
  num_scores   <- length(all_scores)
  # Remove "mbr" from the title
  title_scores <- gsub("mbr","",all_scores) 
  # Remove underscore from score names
  title_scores <- gsub("_"," ",stringr::str_to_title(title_scores)) 
  
  # Plot title
  ptitle <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  
  # Change point size if we are looking at valid_dttm
  if (xgroup == "valid_dttm") {
    stroke_size <- 0
    point_size  <- 0
    xl          <- "Valid date"
  }
  
  # Plot according to number of scores 
  # (and handle some special scores for plot_point_verif)
  if (all(all_scores %in% ens_spec)) {
    # Create the colour table for plot_point_verif
    models_tmp     <- names(mcolors)
    colours_tmp    <- unname(mcolors)
    # Colour table
    ctab           <- data.frame(fcst_model = models_tmp,colour = colours_tmp)
    colnames(ctab) <- c("fcst_model","colour")
    nlr            <- 1
    if (num_models > 2) {
      nlr          <- 2
    }
    if (all_scores == "rank_histogram") {
      all_scores_plot <- c("rank_histogram","normalized_rank_histogram")
    } else {
      all_scores_plot <- all_scores
    }
    for (cur_score in all_scores_plot) {
      if (length(str_split(subtitle_str,"\n")[[1]]) > 1){
        caption_str <- str_split(subtitle_str,"\n")[[1]][2]
      } else {
        caption_str <- "none"
      }
      plot_point_verif(verif,
                       {{cur_score}},
                       rank_is_relative = "TRUE",
                       rank_hist_type   = "bar",
                       colour_by        = "fcst_model",
                       colour_table     = ctab,
                       plot_title       = ptitle,
                       plot_subtitle    = str_split(subtitle_str,"\n")[[1]][1],
                       legend_position  = "top",
                       plot_caption     = caption_str,
                       num_legend_rows  = nlr,
                       base_size        = 8)
      
      # Save 
      png_fname <- fn_png_name(c_ftyp,
                               cur_score,
                               xg_str,
                               param,
                               cycle,
                               sdate,
                               edate,
                               station,
                               vlt,
                               vth,
                               projectname = png_projname)
      # Can get a lot of messages for reliability etc
      suppressMessages(
        ggplot2::ggsave(filename = png_fname,
                        path     = png_archive,
                        width    = fw,
                        height   = fh,
                        units    = fig_units,
                        dpi      = fig_dpi,
                        device   = 'png')
      )
    }
    
    p_out <- "NA"
    return(p_out)
    
  } else if (any(grepl("mbr",all_scores))) {
    
    # If looking at valid_dttm, then use only one column
    if (xgroup == "valid_dttm") {
      ncols = 1
    } else {
      ncols = min(num_models,2)
    }
    c_score <- gsub("mbr","",all_scores)
    
    # If OBS exists as a fcst_model, then add this in
    df_ctrl <- df %>% dplyr::filter(member == "mbr000")
    
    # This deals with the case of dailyvar/timeseries
    if ("OBS" %in% unique(df[["member"]])) {
      df_obs  <- df %>% dplyr::filter(member == "OBS") 
      df_mean <- df %>% dplyr::filter(member == "mean")
      df      <- df %>% dplyr::filter(!(member %in% c("mean","spread","OBS")))
      
      p_out <- ggplot2::ggplot() +
        ggplot2::geom_path(data      = df,
                           aes(x     = get(xgroup),
                               y     = get(c_score),
                               group = interaction(fcst_model,member)),
                           alpha     = 0.5,
                           linewidth = line_size,
                           color     = "gray") +
        ggplot2::geom_path(data      = df_ctrl,
                           aes(x     = get(xgroup),
                               y     = get(c_score),
                               color = "mbr000"),
                           linewidth = line_size) +
        ggplot2::geom_path(data      = df_mean,
                           aes(x     = get(xgroup),
                               y     = get(c_score),
                               color = "Mean"),
                           linewidth = line_size) +
        ggplot2::geom_path(data      = df_obs,
                           aes(x     = get(xgroup),
                               y     = get(c_score),
                               color = "OBS"),
                           linewidth = line_size) +
        ggplot2::facet_wrap(vars(fcst_model),
                            ncol = ncols) +
        ggplot2::labs(x        = xl,
                      y        = yl,
                      title    = ptitle,
                      subtitle = subtitle_str,
                      color    = "") + 
        ptheme_l
      
    } else {
      
      p_out <- ggplot2::ggplot() +
        ggplot2::geom_path(data      = df,
                           aes(x     = get(xgroup),
                               y     = get(c_score),
                               group = interaction(fcst_model,member)),
                           alpha     = 0.5,
                           linewidth = line_size,
                           color     = "gray") +
        ggplot2::geom_path(data      = df_ctrl,
                           aes(x     = get(xgroup),
                               y     = get(c_score)),
                           color     = "red",
                           linewidth = line_size) +
        ggplot2::facet_wrap(vars(fcst_model),
                            ncol = ncols) +
        ggplot2::labs(x        = xl,
                      y        = yl,
                      title    = ptitle,
                      subtitle = subtitle_str) + 
        ptheme_l
      
    }
      
    # Add zero line if relevant
    cp_ylim <- ggplot2::layer_scales(p_out)$y$range$range
    if ((cp_ylim[1] < 0) && (cp_ylim[2] > 0)) {
      p_out <- p_out + ggplot2::geom_hline(yintercept = 0,
                                           linewidth  = line_size,
                                           color      = "black",
                                           linetype   = "dashed")
    }

    if (xgroup == "lead_time") {
      cp_xlim <- ggplot2::layer_scales(p_out)$x$range$range
      if ((cp_xlim[2] - cp_xlim[1]) < 12) {
        lt_sep <- 1
      } else {
        lt_sep <- 6
      }
      p_out <- p_out + ggplot2::scale_x_continuous(breaks = seq(0,720,lt_sep))
    }
    return(p_out)
    
  } else {
    
    if (num_scores == 1) {
      # Remove Inf/-Inf values which may appear for ens skill scores
      df    <- df[!is.infinite(df[[all_scores]]),]
      p_out <- df %>% 
        ggplot2::ggplot(aes(x     = get(xgroup),
                            color = forcats::fct_inorder(fcst_model))) +
        ggplot2::geom_line(aes(y     = get(all_scores)),
                           linewidth = line_size) +
        ggplot2::geom_point(aes(y  = get(all_scores)),
                            stroke = stroke_size,
                            size   = point_size)
      tsns  <- 8
    } else if (num_scores == 2) {
      # Remove Inf/-Inf values which may appear for ens skill scores
      df    <- df[!is.infinite(df[[all_scores[1]]]),]
      df    <- df[!is.infinite(df[[all_scores[2]]]),]
      p_out <- df %>% 
        ggplot2::ggplot(aes(x     = get(xgroup),
                            color = forcats::fct_inorder(fcst_model))) +
        ggplot2::geom_line(aes(y        = get(all_scores[1]),
                               linetype = title_scores[1]),
                           linewidth    = line_size) +
        ggplot2::geom_point(aes(y  = get(all_scores[1])),
                            stroke = stroke_size,
                            size   = point_size) +
        ggplot2::geom_line(aes(y        = get(all_scores[2]),
                               linetype = title_scores[2]),
                           linewidth    = line_size) +
        ggplot2::geom_point(aes(y = get(all_scores[2])),
                            stroke = stroke_size,
                            size   = point_size)
      tsns  <- 6
    } else {
      stop("Number of scores=",num_scores," is not considered in plot_point")
    }
    
    p_out <- p_out + 
      ggplot2::labs(x        = xl,
                    y        = yl,
                    color    = "",
                    linetype = "",
                    title    = ptitle,
                    subtitle = subtitle_str) +
      ptheme_l + 
      ggplot2::scale_color_manual(values = mcolors)
    
    cp_ylim <- layer_scales(p_out)$y$range$range
    if ((cp_ylim[1] < 0) && (cp_ylim[2] > 0)) {
      p_out <- p_out + ggplot2::geom_hline(yintercept = 0,
                                          linewidth  = line_size,
                                          color      = "black",
                                          linetype   = "dashed")
    }
    
    if ((xgroup == "mids") & ("freq_bias" %in% names(df))) {
      p_out <- p_out + ggplot2::geom_hline(yintercept = 1,
                                          linewidth  = line_size,
                                          color      = "black",
                                          linetype   = "dashed")
    }
    
    # Change to log axis for precip threshold scores, and add breaks
    if (xgroup == "threshold_val") {
      if (grepl("AccPcp",param,fixed = TRUE)) {
        p_out <- p_out + ggplot2::scale_x_continuous(trans = "log10",
                                                     breaks = thr_brks,
                                                     minor_breaks = NULL)
      } else {
        p_out <- p_out + ggplot2::scale_x_continuous(breaks = thr_brks,
                                                     minor_breaks = NULL)
      }
      p_out <- p_out + ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45,vjust = 0.75))
    }
    
    # Change the breaks for lead_time/validhour plots
    if (xgroup == "lead_time") {
      cp_xlim <- layer_scales(p_out)$x$range$range
      if ((cp_xlim[2] - cp_xlim[1]) < 12) {
        lt_sep <- 1
      } else {
        lt_sep <- 6
      }
      p_out <- p_out + ggplot2::scale_x_continuous(breaks = seq(0,720,lt_sep))
    } else if (xgroup == "valid_hour") {
      p_out <- p_out + ggplot2::scale_x_continuous(breaks = seq(0,21,3),
                                                   minor_breaks = NULL)
    } else if (xgroup == "mids") {
      if (vroption_list$log_ind) {
        p_out <- p_out + ggplot2::scale_x_continuous(
          trans        = "pseudo_log",
          breaks       = vroption_list$p_breaks,
          minor_breaks = NULL)
      } else {
        p_out <- p_out + ggplot2::scale_x_continuous(
          breaks       = vroption_list$p_breaks,
          minor_breaks = NULL)
      }
      p_out <- p_out + ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45,vjust = 0.75))
    }

    # Change the legend to multiple rows if required
    if ((num_models == 3)) {
      p_out <- p_out + ggplot2::theme(
        legend.text = ggplot2::element_text(size = tsns))
    } else if (num_models >= 4) {
      p_out <- p_out + ggplot2::guides(
        color = ggplot2::guide_legend(nrow = 2,byrow = TRUE))
      p_out <- p_out + ggplot2::theme(
        legend.text = ggplot2::element_text(size = tsns))
    }
    
    return(p_out)
  }
}

#================================================#
# HELPER FUNCTION FOR PLOTTING THE NUMBER OF CASES
#================================================#

fn_plot_numcases <- function(verif,
                             fxoption_list,
                             vroption_list){
  
  # Read options
  c_typ       <- vroption_list$c_typ
  c_ftyp      <- vroption_list$c_ftyp
  xgroup      <- vroption_list$xgroup
  line_size   <- fxoption_list$line_size
  stroke_size <- fxoption_list$stroke_size
  point_size  <- fxoption_list$point_size
  ptheme_nc   <- fxoption_list$ptheme_nc
  mcolors     <- fxoption_list$mcolors
  param       <- fxoption_list$param
  thr_brks    <- fxoption_list$thr_brks
  
  if (all(grepl("_scores",names(verif)))) {
    df <- verif[[paste0(c_ftyp,"_",c_typ,"_scores")]]
  } else {
    df <- verif
  }
  
  # A check on the type of xgroup (in case of character type for
  # lead_time and validhour)
  if ((xgroup %in% names(df)) & (is.character(df[[xgroup]])) &
      ((xgroup == "lead_time") || (xgroup == "valid_hour"))) {
    df[[xgroup]] <- as.numeric(df[[xgroup]])
  }
  
  # Reduce point size for timeseries plots
  if (xgroup == "valid_dttm") {
    point_size <- 0
  }
  
  if (c_typ == "summary") {
    ncy    <- "num_cases"
    ylabel <- "Num. cases"
  } else if (c_typ == "threshold") {
    if (c_ftyp == "det") {
      ncy  <- "num_cases_for_threshold_observed"
    } else if (c_ftyp == "ens") {
      ncy  <- "num_cases_observed"
    }
    ylabel <- "Num. cases obs."
  }
  
  if (xgroup == "mids") {
    ncy       <- "obs_count"
    ylabel    <- "Num. cases obs."
    if ("freq_bias" %in% names(df)) {
      mcolors <- mcolors[names(mcolors) != "OBS"]
    }
  }
  
  p_out <- df %>%
    ggplot2::ggplot(aes(x     = get(xgroup),
                        color = forcats::fct_inorder(fcst_model))) +
    ggplot2::geom_line(aes(y     = get(ncy)),
                       linewidth = line_size) +
    ggplot2::geom_point(aes(y  = get(ncy)),
                        stroke = stroke_size,
                        size   = point_size) +
    ggplot2::labs(x     = " ",
                  y     = ylabel,
                  color = " ",
                  shape = " ") +
    ptheme_nc +
    ggplot2::scale_color_manual(values = mcolors)
  
  # Change axis transformation if applicable
  p_ylim <- ggplot2::layer_scales(p_out)$y$range$range
  if ((p_ylim[2] - p_ylim[1]) > 1000) {
    c_breaks <- round(pracma::logseq(max(1,p_ylim[1]),p_ylim[2],3),0)
    p_out    <- p_out + ggplot2::scale_y_continuous(
      trans  = "pseudo_log",
      breaks = c_breaks,
      labels = as.character(c_breaks),
      minor_breaks = FALSE)
  } else {
    c_breaks <- unique(round(pracma::linspace(p_ylim[1],p_ylim[2],3),0))
    p_out    <- p_out + ggplot2::scale_y_continuous(
      breaks = c_breaks,
      labels = as.character(c_breaks),
      minor_breaks = FALSE)
  }
  
  # Change to log axis for precip threshold scores, and add breaks
  if (xgroup == "threshold_val") {
    if (grepl("AccPcp",param,fixed = TRUE)) {
      p_out <- p_out + ggplot2::scale_x_continuous(trans = "log10",
                                                   breaks = thr_brks,
                                                   minor_breaks = NULL)
    } else {
      p_out <- p_out + ggplot2::scale_x_continuous(breaks = thr_brks,
                                                   minor_breaks = NULL)
    }
    p_out <- p_out + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45,vjust = 0.75))
  }

  # Change the breaks for lead_time/validhour plots
  if (xgroup == "lead_time") {
    cp_xlim <- layer_scales(p_out)$x$range$range
    if ((cp_xlim[2] - cp_xlim[1]) < 12) {
      lt_sep <- 1
    } else {
      lt_sep <- 6
    }
    p_out <- p_out + ggplot2::scale_x_continuous(breaks = seq(0,720,lt_sep))
  } else if (xgroup == "valid_hour") {
    p_out <- p_out + ggplot2::scale_x_continuous(breaks = seq(0,21,3),
                                                 minor_breaks = NULL)
  } else if (xgroup == "mids") {
    if (vroption_list$log_ind) {
      p_out <- p_out + ggplot2::scale_x_continuous(
        trans        = "pseudo_log",
        breaks       = vroption_list$p_breaks,
        minor_breaks = NULL)
    } else {
      p_out <- p_out + 
        ggplot2::scale_x_continuous(breaks       = vroption_list$p_breaks,
                                    minor_breaks = NULL)
    }
    
    p_out <- p_out + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45,vjust = 0.75))
  }
  
  return(p_out)
}

#================================================#
# HELPER FUNCTION FOR PLOTTING MAPS FROM A
# VERIF OBJECT
#================================================#

fn_plot_map <- function(verif,
                        title_str,
                        subtitle_str,
                        fxoption_list,
                        vroption_list){
  
  # Read options
  c_typ      <- vroption_list$c_typ
  c_ftyp     <- vroption_list$c_ftyp
  score      <- vroption_list$score
  num_models <- fxoption_list$num_models
  par_unit   <- fxoption_list$par_unit
  station    <- vroption_list$station
  score_sep  <- fxoption_list$score_sep
  param      <- fxoption_list$param
  map_cbar_d <- fxoption_list$map_cbar_d
  df         <- verif[[paste0(c_ftyp,"_",c_typ,"_scores")]]
  
  # Split sscore into its individual parts
  all_scores   <- strsplit(score,score_sep)[[1]]
  num_scores   <- length(all_scores)
  # Remove "mbr" from the title
  title_scores <- gsub("mbr","",all_scores)
  # Remove underscore from score names
  title_scores <- gsub("_"," ",stringr::str_to_title(title_scores)) 
  
  # Plot title
  ptitle <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  
  # In the map plotting it is assumed that the SID lat/lons are included in 
  # the verification object. Otherwise, nothing can be done
  if ((all(c("lat","lon") %in% names(df))) & (num_scores == 1)) {
    
    # Colorbar options
    c_min   <- min(df[[score]],na.rm = TRUE)
    c_max   <- max(df[[score]],na.rm = TRUE)
    min_lon <- min(df[["lon"]])
    max_lon <- max(df[["lon"]])
    min_lat <- min(df[["lat"]])
    max_lat <- max(df[["lat"]])
   
    # If there is only one station, then skip!
    if ( (min_lon == max_lon) & (min_lat == max_lat)) {
	   warning("Skipping map plotting as only one SID found")
	   return(NA_character_)
    }
    
    p_map <- df %>% ggplot2::ggplot(aes(lon,lat,
                                        fill = get(score),
                                        size = abs(get(score))))
    
    # Get the cmap and brks
    cbar_opts <- fn_get_map_cbar(map_cbar_d,c_min,c_max,score,param,par_unit)
    
    p_map <- p_map + 
      ggplot2::geom_polygon(data        = ggplot2::map_data("world"),
                            mapping     = aes(long, lat, group = group),
                            fill        = "grey100",
                            colour      = "black",
                            inherit.aes = FALSE) +  
      ggplot2::geom_point(colour = 'grey40',pch = 21) + 
      ggplot2::coord_sf(xlim = c(min_lon-0.2,max_lon+0.2),
                        ylim = c(min_lat-0.2,max_lat+0.2)) +
      ggplot2::theme(
            panel.background          = ggplot2::element_rect(fill = "grey95"),
            panel.grid                = ggplot2::element_blank(),
            axis.text                 = ggplot2::element_blank(),
            axis.ticks                = ggplot2::element_blank(),
            axis.title                = ggplot2::element_blank(),
            plot.title                = ggplot2::element_text(size = 12),
            plot.title.position       = "plot",
            legend.text               = ggplot2::element_text(size = 8),
            plot.subtitle             = ggplot2::element_text(size = 8), 
            legend.title              = ggplot2::element_text(size = 10),
            legend.position           = "right", 
            legend.key.height         = unit(1.5,"cm"),
            strip.background          = ggplot2::element_rect(fill = "white"),
            strip.text                = ggplot2::element_text(size = 12)) +
      ggplot2::facet_wrap(
        vars(fcst_model),
        ncol = min(num_models,3)) +
      ggplot2::scale_size_continuous(range = c(0.1, 3)) + # Controls point size
      ggplot2::labs(title    = ptitle,
                    subtitle = subtitle_str,
                    fill     = "",
                    size     = "") +
      ggplot2::guides(size = "none") # Remove size label from legend
    
    if (map_cbar_d) {
      p_map <- p_map + ggplot2::binned_scale(
        name = paste0("(",par_unit,")"),
        aesthetics = "fill",
        palette = function(x) cbar_opts$cmap,
        breaks = cbar_opts$brks,
        limits = c(min(cbar_opts$brks),max(cbar_opts$brks)),
        labels = function(x) round(x,2),
        oob = scales::squish,
        guide = "colorsteps"
      ) 
    } else {
      p_map <- p_map + cbar_opts$cmap
    }
    
    return(p_map)
  } else {
    warning("Cannot plot maps as lat/lon missing or multiple scores specified")
    return(NA_character_)
  }
  
}

#================================================#
# DEFINE THE COLOURBAR OPTIONS USED IN MAP PLOTTING
#================================================#

fn_get_map_cbar <- function(map_cbar_d,c_min,c_max,score,param,par_unit){
  
  if (map_cbar_d){
    
  # First define the breaks based on the parameter
  # Largely following those in Monitor
  if (param %in% c("Pmsl")) {
    if (score %in% c("bias","mean_bias")) {
      brks <- seq(-5,-0.5,0.5)
      brks <- c(brks,-0.25,0.25,-rev(brks))
    } else {
      brks <- seq(0,5,0.5)
    }
  } else if (param %in% c("T2m","Td2m","Q2m","Tmax","Tmin")) {
    if (score %in% c("bias","mean_bias")) {
      brks <- seq(-6,-1,1)
      brks <- c(brks,-0.5,0.5,-rev(brks))
    } else {
      brks <- seq(0,10,1)
    }
  } else if (param %in% c("S10m","Gmax","Smax","G10m")) {
    if (score %in% c("bias","mean_bias")) {
      brks <- seq(-10,-1,1)
      brks <- c(brks,-0.5,0.5,-rev(brks))
    } else {
      brks <- seq(0,15,1)
    }
  } else if (param %in% c("D10m")){
    if (score %in% c("bias","mean_bias")){
      brks <- seq(-180,-30,30)
      brks <- c(brks,-15,15,-rev(brks))
    } else {
      brks <- seq(0,180,30)
    }
  } else if (param %in% c("RH2m")){
    if (score %in% c("bias","mean_bias")) {
      brks <- seq(-25,-5,5)
      brks <- c(brks,-2.5,2.5,-rev(brks))
    } else {
      brks <- seq(0,100,10)
    }
  } else if (param %in% c("vis","Cbase")){
    if (score %in% c("bias","mean_bias")) {
      brks <- c(-40000,-30000,-20000,-10000,-5000,-2500,-1000)
      brks <- c(brks,-rev(brks))
    } else {
      brks <- c(0,2500,5000,10000,15000,20000,25000,30000,40000,50000)
    }
  } else if (param %in% c("CCtot","CClow","CCmed","CChigh","N75")){
    if (score %in% c("bias","mean_bias")) {
      brks <- seq(-8,-2,2)
      brks <- c(brks,-1,1,-rev(brks))
    } else {
      brks <- seq(0,8,1)
    }
  } else if (grepl("Pcp",param,fixed=T)) {
    if (score %in% c("bias","mean_bias")) {
      brks <- seq(-20,-2.5,2.5)
      brks <- c(brks,-1,1,-rev(brks))
    } else {
      brks <- c(seq(0,10,2),12.5,15,17.5,20,25,30,35)
    }
  } else {
    stop("Need to add this param in cmap defs!")
  }
  
  # Then get the cmap
  if (score %in% c("bias","mean_bias")) {
    scico_pal <- "vik"
    scico_dir <- 1
  } else {
    scico_pal <-"lipari"
    scico_dir <- -1
  }
  cmap <- scico::scico(length(brks)-1,
                       palette   = scico_pal,
                       direction = scico_dir)
  
  # Then filter the cmap to just the colours which cover the range cmin-cmax
  bl <- brks[brks<=c_min]
  if (length(bl) > 1){
    bl_ind <- which(brks == tail(bl,1))
    bl_val <- NULL
  } else { 
    bl_ind <- 1
    bl_val <- round(c_min,1)
  }
  bu <- brks[brks>=c_max]
  if (length(bu) > 1){
    bu_ind <- which(brks == bu[1])
    bu_val <- NULL
  } else {
    bu_ind <- length(brks)
    bu_val <- round(c_max,1)
  }
  
  cmap_out <- cmap[bl_ind:(bu_ind-1)] 
  brks_out <- brks[bl_ind:bu_ind]
  
  # If we are outside the limits defined by brks, add in the extremes
  if (!is.null(bl_val) & (score %in% c("bias","mean_bias"))) {
    brks_out <- c(bl_val,brks_out)
    cmap_out <- c("darkorchid3",cmap_out)
  }
  if (!is.null(bu_val)){
    brks_out <- c(brks_out,bu_val)
    if (score %in% c("bias","mean_bias")) {
      up_col <- "hotpink2"
    } else {
      up_col <- "darkorchid3"
    }
    cmap_out <- c(cmap_out,up_col)
  }
  
  } else {
    
  c_min   <- c_min - 0.1
  c_max   <- c_max + 0.1
  c_min   <- round(c_min,1)
  c_max   <- round(c_max,1)
  
  if (grepl("bias",score)) {
    cmap_out <- ggplot2::scale_fill_gradient2(
      paste0("(",par_unit,")"),
      guide    = ggplot2::guide_colourbar(title.position = "top"),
      low      = "blue4",
      mid      = "white",
      high     = "red4",
      n.breaks = 5,
      limits   = c(c_min,c_max))
  } else {
    cmap_out <- ggplot2::scale_fill_viridis_c(
      paste0("(",par_unit,")"),
      option    = "A",
      direction = -1,
      guide     = ggplot2::guide_colourbar(title.position = "top"),
      limits    = c(c_min,c_max))
  }
  
  brks_out <- NULL
    
  }
  
  return(list("cmap" = cmap_out,
              "brks" = brks_out))
  
}

#================================================#
# PLOTTING UA PROFILES
#================================================#

fn_plot_profile <- function(verif,
                            title_str,
                            subtitle_str,
                            plot_num_cases,
                            fxoption_list,
                            vroption_list){
  
  # Read options
  c_typ       <- vroption_list$c_typ
  c_ftyp      <- vroption_list$c_ftyp
  score       <- vroption_list$score
  xgroup      <- vroption_list$xgroup
  cycle       <- vroption_list$cycle
  station     <- vroption_list$station
  xg_str      <- vroption_list$xg_str
  line_size   <- fxoption_list$line_size
  stroke_size <- fxoption_list$stroke_size
  ptheme_l    <- fxoption_list$ptheme_l
  mcolors     <- fxoption_list$mcolors
  line_styles <- fxoption_list$line_styles
  par_unit    <- fxoption_list$par_unit
  png_archive <- fxoption_list$png_archive
  sdate       <- fxoption_list$sdate
  edate       <- fxoption_list$edate
  ptheme_nc   <- fxoption_list$ptheme_nc 
  fw          <- fxoption_list$fw
  fh          <- fxoption_list$fh
  param       <- fxoption_list$param
  num_models  <- fxoption_list$num_models
  point_size  <- fxoption_list$point_size
  score_sep   <- fxoption_list$score_sep
  df          <- verif[[paste0(c_ftyp,"_",c_typ,"_scores")]]
  
  # Split sscore into its individual parts
  all_scores   <- strsplit(score,score_sep)[[1]]
  num_scores   <- length(all_scores)
  # Remove "mbr" from the title
  title_scores <- gsub("mbr","",all_scores) 
  # Remove underscore from score names
  title_scores <- gsub("_"," ",stringr::str_to_title(title_scores)) 
  
  # Plot title
  ptitle <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  
  # Note: No member plotting considered here 
  if (num_scores == 1) {
    p_out <- df %>% 
      ggplot2::ggplot(aes(y     = p,
                          color = forcats::fct_inorder(fcst_model))) +
      ggplot2::geom_path(aes(x    = get(all_scores)),
                        linewidth = line_size) +
      ggplot2::geom_point(aes(x  = get(all_scores)),
                          stroke = stroke_size,
                          size   = point_size)
  } else if (num_scores == 2) {
    p_out <- df %>% 
      ggplot2::ggplot(aes(y     = p,
                          color = forcats::fct_inorder(fcst_model))) +
      ggplot2::geom_path(aes(x         = get(all_scores[1]),
                             linetype  = title_scores[1]),
                         linewidth     = line_size) +
      ggplot2::geom_point(aes(x  = get(all_scores[1])),
                          stroke = stroke_size,
                          size   = point_size) +
      ggplot2::geom_path(aes(x         = get(all_scores[2]),
                             linetype  = title_scores[2]),
                         linewidth     = line_size) +
      ggplot2::geom_point(aes(x  = get(all_scores[2])),
                          stroke = stroke_size,
                          size   = point_size)
  } else {
    stop("Number of scores=",num_scores," is not considered in plot_profile")
  }
  
  if (max(df$p) < 100) {
    p_label <- "Channel"
  } else {
    p_label <- "p (hPa)"
  }
  p_out <- p_out +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(y        = p_label,
                  x        = par_unit,
                  color    = "",
                  linetype = "") + 
    ptheme_l +
    ggplot2::scale_color_manual(values = mcolors)
  
  # Add zero line if relevant
  cp_xlim <- ggplot2::layer_scales(p_out)$x$range$range
  if ((cp_xlim[1] < 0) && (cp_xlim[2] > 0)) {
    p_out <- p_out + ggplot2::geom_vline(xintercept = 0,
                                         linewidth  = line_size,
                                         color      = "black",
                                         linetype   = "dashed")
  }
  
  # Change the legend to multiple rows if required
  lh <- 1
  bh <- 9
  if (num_models > 2) {
    p_out <- p_out + ggplot2::guides(color = ggplot2::guide_legend(nrow = 2,
                                                                   byrow = TRUE))
    lh <- 1.5
    bh <- 8.5
  }
  
  # Combine plot and numcases grobs side-by-side. To do this, we want a common 
  # legend over the plot, which is obtained by usingfn_prof_legend
  mylegend <- fn_prof_legend(p_out)
  
  if (plot_num_cases) {
    ncy    <- "num_cases"
    ylabel <- "Num. cases"
    p_nc  <- df %>% 
      ggplot2::ggplot(aes(y     = p,
                          color = forcats::fct_inorder(fcst_model))) +
      ggplot2::geom_path(aes(x     = get(ncy)),
                         linewidth = line_size) +
      ggplot2::geom_point(aes(x  = get(ncy)),
                          stroke = stroke_size,
                          size   = point_size) +
      ggplot2::labs(y     = " ",
                    x     = ylabel,
                    color = " ",
                    shape = " ") +
      ptheme_nc +
      ggplot2::scale_color_manual(values = mcolors) +
      ggplot2::scale_y_reverse() + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0.75))

    p_xlim   <- ggplot2::layer_scales(p_nc)$x$range$range
    c_breaks <- unique(round(pracma::linspace(p_xlim[1],p_xlim[2],2),0))
    p_nc     <- p_nc + ggplot2::scale_x_continuous(breaks = c_breaks,
                                                   labels = as.character(c_breaks))
    p_tot    <- gridExtra::grid.arrange(
                  mylegend,
                  gridExtra::arrangeGrob(p_out + theme(legend.position = "none"),
                                         p_nc + theme(legend.position = "none"),
                                         ncol    = 2,
                                         nrow    = 1,
                                         widths  = c(4,1.0),
                                         heights = c(4)),
                  top = grid::textGrob(paste0(ptitle," \n",subtitle_str),
                                       x     = 0.1,
                                       hjust = 0,
                                       gp    = grid::gpar(fontsize = 8)),
                  nrow    = 2,
                  heights = c(lh,bh))
  } else {
    
    p_tot <- gridExtra::grid.arrange(
                  mylegend,
                  gridExtra::arrangeGrob(p_out + theme(legend.position = "none"),
                                         ncol    = 1, 
                                         nrow    = 1,
                                         widths  = c(4),
                                         heights = c(4)),
                  top = textGrob(paste0(ptitle," \n",subtitle_str),
                               x     = 0.1,
                               hjust = 0,
                               gp    = grid::gpar(fontsize = 8)),
                  nrow    = 2,
                  heights = c(lh,bh))
  }
  
  return(p_tot)
  
}

#================================================#
# LEGEND FOR UA PROFILES
#================================================#

fn_prof_legend <- function(a.gplot){
  tmp    <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
  leg    <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#================================================#
# HIGHLIGHT WHICH STATIONS HAVE LARGE RMSE
#================================================#

fn_sid_issue_table <- function(df,
                               fxoption_list,
                               vroption_list,
                               vlt="NA",
                               vth="NA"){
  
  # Read options
  c_ftyp      <- vroption_list$c_ftyp
  score       <- vroption_list$score;
  cycle       <- vroption_list$cycle;
  station     <- vroption_list$station;
  xg_str      <- vroption_list$xg_str
  png_archive <- fxoption_list$png_archive;
  sdate       <- fxoption_list$sdate;
  edate       <- fxoption_list$edate;
  param       <- fxoption_list$param
  
  mnames               <- unique(df[[1]][["fcst_model"]])
  num_stat             <- unique(df[[1]][["SID"]])
  stns_lrmse           <- df[[1]] %>% 
    dplyr::filter(fcst_model == mnames[1]) %>%
    dplyr::arrange(-rmse)
  ns_val               <- min(50,num_stat) # Info for 50 stations
  stns_lrmse           <- stns_lrmse[1:ns_val,] %>% 
    dplyr::select("fcst_model",
                  "valid_hour",
                  "fcst_cycle",
                  "SID",
                  "lat",
                  "lon",
                  "rmse",
                  "num_cases")
  stns_lrmse[["rmse"]] <- round(stns_lrmse[["rmse"]],2)
  
  # Get corresponding png name
  png_fname <- fn_png_name(c_ftyp,
                           score,
                           xg_str,
                           param,
                           cycle,
                           sdate,
                           edate,
                           station,
                           vlt,
                           vth)
  tab_fname <- paste0("SIDtable-",png_fname)
  tab_fname <- gsub(".png",".csv",tab_fname)
  
  # Save as a table
  write.table(stns_lrmse,
              file      = file.path(png_archive,tab_fname),
              sep       = ",",
              row.names = FALSE)
}

#================================================#
# A WRAPPER FUNCTION FOR CREATING ALL 
# AUXILIARY PLOTS
#================================================#
fn_aux <- function(fc,
                   title_str,
                   subtitle_str,
                   fxoption_list,
                   vroption_list,
                   rolling_verif){
  
  if (grepl("Mbr000",title_str)) {
    cprefix <- "ctrl"
  } else {
    cprefix <- ""
  }
  
  if (!rolling_verif) {
    
    # Daily Var 
    group_vars           <- c("fcst_model","valid_hour")
    vroption_list$xgroup <- "valid_hour"
    vroption_list$score  <- paste0(cprefix,"dailyvar")
    vroption_list$xg_str <- "vh"
    fn_dvar_ts(fc,
               group_vars,
               title_str,
               subtitle_str,
               fxoption_list,
               vroption_list)
    
    # Frequency distribution 
    vroption_list$xgroup <- "NA"
    vroption_list$score  <- paste0(cprefix,"freqdist")
    vroption_list$xg_str <- "NA";
    fn_freqdist(fc,
                title_str,
                subtitle_str,
                fxoption_list,
                vroption_list)
    
    # Alternative freqhist method for freq dist plots
    vroption_list$xgroup <- "mids"
    vroption_list$xg_str <- "cls"
    fn_freqhist(fc,
                title_str,
                subtitle_str,
                fxoption_list,
                vroption_list)
  }
  
  # Timeseries 
  group_vars          <- c("fcst_model","valid_dttm")
  vroption_list$xgroup <- "valid_dttm"
  vroption_list$score  <- paste0(cprefix,"timeseries")
  vroption_list$xg_str <- "vd"
  fn_dvar_ts(fc,
             group_vars,
             title_str,
             subtitle_str,
             fxoption_list,
             vroption_list)
  
  # Scatter plots
  vroption_list$xgroup <- "NA"
  vroption_list$score  <- paste0(cprefix,"scatterplot")
  vroption_list$xg_str <- "NA";
  fn_scatterplot(fc,
                 title_str,
                 subtitle_str,
                 fxoption_list,
                 vroption_list)
  
}


#================================================#
# COMPUTE THE DAILY VARIATION/TIMESERIES
#================================================#

fn_dvar_ts <- function(fc,
                       group_vars,
                       title_str,
                       subtitle_str,
                       fxoption_list,
                       vroption_list){
  
  # Read options
  score      <- vroption_list$score
  station    <- vroption_list$station
  score_orig <- score
  if (grepl("mbr",score)) {
    score <- gsub("mbr","",score)
  }
  
  if (grepl("ctrl",score)) {
    score               <- gsub("ctrl","",score)
    vroption_list$score <- score
  }
  
  dv <- fc %>% 
    dplyr::group_by_at(group_vars) %>%
    dplyr::summarise(!!score  := mean(fcst),
                     mean_obs  = mean(OBS),
                     num_cases = n())
  
  # Replace mean_obs by "OBS" in fcst_model (or member)
  if (grepl("mbr",score_orig)) {
    dv_tmp <- dplyr::filter(dv,member == unique(dv[["member"]][1]))
    dv_tmp <- dplyr::mutate(dv_tmp,member = "OBS","{score}" := mean_obs)
  } else {
    dv_tmp <- dplyr::filter(dv,fcst_model == unique(dv[["fcst_model"]])[1])
    dv_tmp <- dplyr::mutate(dv_tmp,fcst_model = "OBS","{score}" := mean_obs)
  }
  dv <- dplyr::bind_rows(dv,dv_tmp)
  dv <- dplyr::select(dv,-mean_obs)
  
  # Call plotting
  p_c <- fn_plot_point(dv,
                       title_str,
                       subtitle_str,
                       fxoption_list,
                       vroption_list)
  
  if (fxoption_list$plot_num_cases & (!grepl("mbr",score_orig))) {
    p_nc  <- fn_plot_numcases(dv,
                              fxoption_list,
                              vroption_list)
    p_c   <- fn_nc_combine(p_c,p_nc)
  }
  
  fn_save_png(p_c           = p_c,
              fxoption_list = fxoption_list,
              vroption_list = vroption_list,
              fcst_type     = fxoption_list$c_ftyp,
              score         = score_orig)
  
}

#================================================#
# FREQUENCY DISTRIBUTION
#================================================#

fn_freqdist <- function(fc,
                        title_str,
                        subtitle_str,
                        fxoption_list,
                        vroption_list){
  
  # Remove "mbr/CTRL/_" from the title
  title_scores <- gsub("mbr","",vroption_list$score) 
  title_scores <- gsub("ctrl","",vroption_list$score) 
  title_scores <- gsub("_"," ",stringr::str_to_title(title_scores)) 
  ptitle       <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  param        <- fxoption_list$param
  num_models   <- fxoption_list$num_models
  
  # Add in filter for precipitation to only plot values > threshold
  if (grepl("AccPcp",param,fixed = TRUE)) {
    pcp_thr <- 0.1
    fc <- fc %>% dplyr::filter(fcst > pcp_thr,OBS > pcp_thr)
  }
  
  if (nrow(fc) == 0 ) {
    warning("No data for ",param," freqdist plot, skipping!")
  } else {
    if (grepl("mbr",vroption_list$score)) {
      
      fc_obs  <- fc %>% dplyr::filter(member == unique(fc[["member"]][1]))
      fc_obs  <- fc_obs %>% dplyr::mutate(member = "OBS",fcst = OBS)
      fc_mean <- fc %>% dplyr::filter(member == "mean")
      fc_ctrl <- fc %>% dplyr::filter(member == "mbr000")
      fc      <- fc %>% dplyr::filter(!(member %in% c("mean","spread","OBS")))
      
      p_out <- ggplot2::ggplot() +
        ggplot2::geom_density(data      = fc,
                              aes(x     = fcst,
                                  group = interaction(fcst_model,member)),
                              color     = "gray",
                              linewidth = fxoption_list$line_size,
                              adjust    = fxoption_list$fd_adjust,
                              alpha     = 0.5) +
        ggplot2::geom_density(data      = fc_ctrl,
                              aes(x     = fcst,
                                  group = fcst_model,
                                  color = "mbr000"),
                              linewidth = fxoption_list$line_size,
                              adjust    = fxoption_list$fd_adjust,
                              alpha     = 0.5) +
        ggplot2::geom_density(data      = fc_mean,
                              aes(x     = fcst,
                                  group = fcst_model,
                                  color = "mean"),
                              linewidth = fxoption_list$line_size,
                              adjust    = fxoption_list$fd_adjust,
                              alpha     = 0.5) +
        ggplot2::geom_density(data      = fc_obs,
                              aes(x     = fcst,
                                  group = fcst_model,
                                  color = "OBS"),
                              linewidth = fxoption_list$line_size,
                              adjust    = fxoption_list$fd_adjust,
                              alpha     = 0.5) +
        ggplot2::facet_wrap(vars(fcst_model),
                            ncol = min(fxoption_list$num_models,2)) +
        ggplot2::labs(x        = fxoption_list$par_unit,
                      y        = "Density",
                      color    = "",
                      title    = ptitle,
                      subtitle = subtitle_str) +
        fxoption_list$ptheme_l
      
    } else {
      
      fc_obs <- fc %>% dplyr::filter(
        fcst_model == unique(fc[["fcst_model"]][1]))
      
      p_out <- ggplot2::ggplot() +
        ggplot2::geom_density(data      = fc,
                              aes(x     = fcst,
                                  color = forcats::fct_inorder(fcst_model)),
                              linewidth = fxoption_list$line_size,
                              adjust    = fxoption_list$fd_adjust) +
        ggplot2::geom_density(data      = fc_obs,
                              aes(x     = OBS,
                                  color = "OBS"),
                              linewidth = fxoption_list$line_size,
                              adjust    = fxoption_list$fd_adjust) +
        ggplot2::labs(x        = fxoption_list$par_unit,
                      y        = "Density",
                      color    = "",
                      title    = ptitle,
                      subtitle = subtitle_str) +
        fxoption_list$ptheme_l + 
        ggplot2::scale_color_manual(values = fxoption_list$mcolors)
      
    }
    
    cp_xlim <- ggplot2::layer_scales(p_out)$x$range$range
    if ((cp_xlim[1] < 0) & (cp_xlim[2] > 0) &
        (!grepl("AccPcp",fxoption_list$param,fixed = TRUE))) {
      p_out <- p_out + ggplot2::geom_vline(xintercept = 0,
                                           linewidth  = fxoption_list$line_size,
                                           color      = "black",
                                           linetype   = "dashed")
    }  
    
    # Change to log axis for precip
    if (grepl("AccPcp",param,fixed = TRUE)) {
      p_out <- p_out + ggplot2::scale_x_continuous(trans = "log10")
    }
    
    # Change the legend to multiple rows if required
    if ((num_models == 3)) {
      p_out <- p_out + ggplot2::theme(
        legend.text = ggplot2::element_text(size = 8))
    } else if (num_models >= 4) {
      p_out <- p_out + ggplot2::guides(
        color = guide_legend(nrow = 2,byrow = TRUE))
      p_out <- p_out + ggplot2::theme(
        legend.text = ggplot2::element_text(size = 8))
    }
    
    fn_save_png(p_c           = p_out,
                fxoption_list = fxoption_list,
                vroption_list = vroption_list,
                fcst_type     = fxoption_list$c_ftyp,
                score         = vroption_list$score)
    
  }
  
}

#================================================#
# FREQUENCY HIST/BIAS USING CLASSES
#================================================#

fn_freqhist <- function(fc,
                        title_str,
                        subtitle_str,
                        fxoption_list,
                        vroption_list){
  
  title_scores <- gsub("mbr","",vroption_list$score) 
  title_scores <- gsub("ctrl","",vroption_list$score) 
  title_scores <- gsub("_"," ",str_to_title(title_scores)) 
  ptitle       <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  param        <- fxoption_list$param;
  if (grepl("Mbr000",title_str)) {
    cprefix <- "ctrl"
  } else {
    cprefix <- ""
  }
  
  # Define the breaks to be used in histogram plotting
  plot_ind <- TRUE
  log_ind  <- FALSE
  if ((param %in% c("T2m","Td2m")) || (grepl("T2m",param,fixed=TRUE))) {
    p_breaks <- seq(-30,30,2.5)
  } else if (param == "Q2m") {
    p_breaks <- seq(0.5,10,0.5)
  } else if (param  == "RH2m") {
    p_breaks <- seq(25,100,5)
  } else if ((param %in% c("S10m","Gmax")) || (grepl("S10m",param,fixed=TRUE))) {
    p_breaks <- c(2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,30,35,40)
  } else if (param == "Cbase") {
    p_breaks <- c(0,100,500,1000,1500,2000,3000,5000,7000,10000,15000,20000)
    log_ind <- TRUE
  } else if (param %in% c("AccPcp1h","AccPcp3h","AccPcp6h",
                          "AccPcp12h","AccPcp24h")) {
    p_breaks <- c(0.1,0.25,0.5,1,2.5,5,7.5,10,12.5,15,17.5,20,25,30,40,50,75,100,125,150)
    log_ind <- TRUE
  } else if (param == "vis") {
    p_breaks <- c(0,1000,2000,3000,4000,5000,7500,10000,15000,
                  20000,25000,30000,40000)
    log_ind <- TRUE
  } else if (param %in% c("CClow","CCmed","CChigh","CCtot")) {
    p_breaks <- seq(-0.5,8.5,1)
  } else {
    warning("Do not produce freq hist plots for ",param)
    plot_ind <- FALSE
    p_breaks <- "NA"
  }
  
  vroption_list$p_breaks <- p_breaks
  vroption_list$log_ind  <- log_ind
  
  if (plot_ind) {
    
    if (grepl("mbr",vroption_list$score)) {
      
      warning("Freq hist plot not implemented for ensemble scores yet")
      
    } else {
      
      # Get obs counts based on p_breaks
      fc_obs <- fc %>% dplyr::filter(
        fcst_model == unique(fc[["fcst_model"]][1]))
      o_hist <- ggplot2::ggplot() +
        ggplot2::geom_histogram(data   = fc_obs,
                                aes(x  = OBS),
                                breaks = p_breaks,
                                closed = "right")
      o_hist <- ggplot2::layer_data(o_hist)
      o_hist <- o_hist %>% dplyr::filter(count > 0)
      
      # Convert to a suitable tibble
      df_ob            <- NULL
      df_ob$mids       <- o_hist$x
      df_ob$freqhist   <- o_hist$count
      df_ob$obs_count  <- o_hist$count # For consistency with df_fh below
      df_ob$fcst_model <- "OBS"
      df_ob            <- tibble::as_tibble(df_ob)
      
      # Now do the same for the forecast data. Loop over models and get freq
      df_fh <- NULL
      for (mn in unique(fc[["fcst_model"]])) {
        fhp <- fc %>% dplyr::filter(fcst_model == mn) %>%
          ggplot2::ggplot() + 
          ggplot2::geom_histogram(aes(x  = fcst),
                                  breaks = p_breaks,
                                  closed = "right")
        f_hist            <- ggplot2::layer_data(fhp)
        f_hist            <- f_hist %>% dplyr::filter(x %in% df_ob$mids)
        df_tmp            <- NULL
        df_tmp$mids       <- f_hist$x
        df_tmp$freqhist   <- f_hist$count
        df_tmp$fcst_model <- mn; 
        df_tmp$obs_count  <- df_ob$obs_count
        df_tmp$freq_bias  <- df_tmp$freqhist/df_tmp$obs_count
        df_tmp            <- tibble::as_tibble(df_tmp)
        df_fh             <- dplyr::bind_rows(df_fh,df_tmp)
      }
      
      df_fb <- df_fh
      df_fh <- df_fh %>% dplyr::select(-freq_bias)
      df_fh <- dplyr::bind_rows(df_fh,df_ob)
      
    }
    
    if (nrow(df_fh) > 0) {
      
      # Plot histogram
      vroption_list$score <- "freqhist"
      p_c                 <- fn_plot_point(df_fh,
                                           title_str,
                                           subtitle_str,
                                           fxoption_list,
                                           vroption_list)
      score_ps            <- paste0(cprefix,"freqhist")
      
      fn_save_png(p_c           = p_c,
                  fxoption_list = fxoption_list,
                  vroption_list = vroption_list,
                  fcst_type     = fxoption_list$c_ftyp,
                  score         = score_ps)
      
      # Plot associated freq_bias
      vroption_list$score <- "freq_bias"
      p_c                 <- fn_plot_point(df_fb,
                                           title_str,
                                           subtitle_str,
                                           fxoption_list,
                                           vroption_list)
      p_nc                <- fn_plot_numcases(df_fb,
                                              fxoption_list,
                                              vroption_list)
      p_c                 <- fn_nc_combine(p_c,p_nc,h1=3.75,h2=1.25)
      score_ps            <- paste0(cprefix,"freq_bias")
      
      fn_save_png(p_c           = p_c,
                  fxoption_list = fxoption_list,
                  vroption_list = vroption_list,
                  fcst_type     = fxoption_list$c_ftyp,
                  score         = score_ps)
      
    }
  } # plot_ind
  
}

#================================================#
# SCATTERPLOT
# TODO: CAN MOVE OVER TO HARP'S PLOT_SCATTER?
#================================================#

fn_scatterplot <- function(fc,
                           title_str,
                           subtitle_str,
                           fxoption_list,
                           vroption_list){
  
  title_scores <- gsub("mbr","",vroption_list$score)
  title_scores <- gsub("ctrl","",vroption_list$score) 
  title_scores <- gsub("_"," ",str_to_title(title_scores))
  ptitle       <- paste0(paste0(title_scores,collapse = ", ")," : ",title_str)
  
  if ((length(unique(fc$OBS)) > 1) & (length(unique(fc$fcst)) > 1)) {
    
    p_scat    <- fc %>% ggplot2::ggplot(aes(x = OBS,
                                            y = fcst)) +
      ggplot2::geom_hex(bins = fxoption_list$scat_bins) +
      ggplot2::facet_wrap(vars((fcst_model)),
                          ncol = min(fxoption_list$num_models,3))
    max_count <- max(ggplot2::ggplot_build(p_scat)$data[[1]]$count)
    br1       <- unique(round(logseq(1,max_count,5),0))
    
    oto_col <- "black"
    if (fxoption_list$cmap_hex == "paired") {
      p_scat <- p_scat + ggplot2::scale_fill_gradientn(
        " ",
        colors = fxoption_list$cpal_hex,
        labels = br1,
        breaks = br1,
        trans  = "log")
    } else if (fxoption_list$cmap_hex %in% scico::scico_palette_names()) {
      p_scat <- p_scat + scico::scale_fill_scico(
        " ",
        palette = fxoption_list$cmap_hex,
        direction = -1,
        labels = br1,
        breaks = br1,
        trans  = "log")
      oto_col <- "red"
    } else {
      p_scat <- p_scat + ggplot2::scale_fill_viridis_c(
        " ",
        option = fxoption_list$cpal_hex,
        breaks = br1,
        labels = br1,
        trans  = "log")
    }
    
    p_scat    <- p_scat +
      ggplot2::geom_abline(intercept = 0,
                           slope     = 1,
                           color     = oto_col,
                           linewidth = 0.375) +
      ggplot2::labs(x        = paste0("OBS (",fxoption_list$par_unit,")"),
                    y        = paste0("Forecast (",fxoption_list$par_unit,")"),
                    title    = ptitle,
                    subtitle = subtitle_str) +
      fxoption_list$ptheme_l +
      ggplot2::theme(legend.key.width = unit(1.5,"cm"))
    
    fn_save_png(p_c           = p_scat,
                fxoption_list = fxoption_list,
                vroption_list = vroption_list,
                fcst_type     = fxoption_list$c_ftyp,
                score         = vroption_list$score)
  
  } else {
    warning("Fringe case in scatter where all OBS/Forecast values are the same")
  }
  
}

#================================================#
# HELPER FUNCTION TO GET LEADTIMES USED IN PROFILE
# PLOTS
#================================================#

fn_get_lts_used <- function(vh,cyc,allcycles,alllts){
  
  allcycles <- setdiff(allcycles,"All")
  lts_avail <- setdiff(alllts,"All")
  lts_used  <- c()
  
  if (vh == "All") {
    vh = 100
    vh_add <- TRUE
  } else {
    vh_add <- FALSE
    vh = as.integer(vh)
  }
  
  if (cyc == "All") {
    cyc_vec <- allcycles
  } else {
    cyc_vec <- cyc
  }
    
  for (cc in cyc_vec) {
    for (lt in lts_avail) {
      vv <- as.integer(cc) + as.integer(lt)
      if (((vv %% 24) == vh) || (vh_add)) {
        if (is.null(lts_used)) {
          lts_used <- as.integer(lt)
        } else {
          lts_used <- c(lts_used,as.integer(lt))
        }
      }
    }
  }
  lts_used <- sort(unique(lts_used))
  
  if (is.null(lts_used)) {
    lts_used <- ""
  } else {
    if (length(lts_used) > 5) {
      lt_used_fig <- c(lts_used[1],
                       lts_used[2],
                       "... ",
                       lts_used[length(lts_used)-1],
                       lts_used[length(lts_used)])
      lt_used_fig <- paste0(lt_used_fig,collapse = ", ")
    } else {
      lt_used_fig <- paste0(lts_used,collapse = ", ")
    }
    lts_used <- paste("+",lt_used_fig)
  }
  return(lts_used)
  
}

#================================================#
# HELPER FUNCTION FOR DEFINING FIXED LINE COLOURS
#================================================#

fn_gen_model_colors <- function(in_names,
                                cmap,
                                withobs=FALSE){
  
  # Some fixed model names which the verification scripts know about
  # If "trubetskoy" is used, up to 20 colours are available, with
  # less for RColorBrewer
  model_names  <- c("c1",
                    "c2",
                    "c3",
                    "c4",
                    "c5",
                    "c6",
                    "c7",
                    "c8",
                    "c9",
                    "c10",
                    "c11",
                    "c12",
                    "c13",
                    "c14",
                    "c15",
                    "c16",
                    "c17",
                    "c18",
                    "c19",
                    "c20")
  
  # If all of the input names are known, use a fixed list.
  # Otherwise, use length(in_names) colours
  if (all(in_names %in% model_names)) {
    pal_length <- length(model_names)
    pal_names  <- model_names
  } else {
    pal_length <- length(in_names)
    pal_names  <- in_names
  }
  
  # Assuming cmap is an Rcolorbrewer type
  bi <- rownames(RColorBrewer::brewer.pal.info)
  if (cmap %in% bi) {
    if (RColorBrewer::brewer.pal.info[bi == cmap,]$maxcolors >= pal_length) {
      model_colors <- RColorBrewer::brewer.pal(max(pal_length,3),cmap)
    }
  # Use pals package
  } else if ((cmap %in% c("trubetskoy")) & (pal_length <= 20)) {
    # Drop the last two colours (white,black)
    tmap         <- unname(get(cmap)())[1:20]
    model_colors <- tmap[1:pal_length]
  # If not these, then go to the default
  } else {
    warning(cmap," does not have enough colours/is not in RColorBrewer")
    cat("Using R's defualt line colors")
    model_colors <- scales::hue_pal()(pal_length)
  }
  
  model_colors <- model_colors[1:length(pal_names)]
  if (withobs) {
    model_colors <- c(model_colors,'#4F4F4F')
    pal_names    <- c(pal_names,"OBS")
  }
  names(model_colors) <- pal_names
  
  # Filter to just in_names
  model_colors <- model_colors[names(model_colors) %in% c(in_names,"OBS")]
  
  return(model_colors)
}
