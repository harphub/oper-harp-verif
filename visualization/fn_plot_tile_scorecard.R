

#================================================#
# ALTERNATIVE SCORECARDS PLOTTING
#
# ASSUMES INPUT DATA IN "PROPER" ORDER
#================================================#

fn_plot_tile_scorecard <- function(sc_input,
                                   scores_to_plot,
                                   fcstmodel,
                                   refmodel,
                                   png_archive,
                                   significance = 0.95,
                                   png_projname = NA_character_,
                                   leadtimes = "All",
                                   fsd = NA_character_,
                                   fed = NA_character_){

  #================================================#
  # SOME INITIAL CHECKS AND
  # MANIPULATE THE DATA INTO A SUITABLE FORMAT 
  #================================================#
  
  num_cycles  <- attr(sc_input[[1]][[1]],"num_cycles")
  dttm_avail  <- attributes(sc_input[[1]][[1]])$dttm
  if (is.list(dttm_avail)) {
    dttm_avail <- dttm_avail %>% unlist() %>% unique()
  }
  dttm_avail <- dttm_avail[order(dttm_avail)]
  
  # Get start and end dates
  d_out        <- get_sedate(fsd,
                             fed,
                             dttm_avail[1],
                             tail(dttm_avail,1))
  start_date   <- d_out$sdate
  end_date     <- d_out$edate
  tstart_date  <- d_out$tsdate
  tend_date    <- d_out$tedate
  
  # Check png_archive for s/edate
  png_archive <- check_png_sedate(start_date,end_date,png_archive)
  
  # Define some figure widths/heights
  sc_fh     <- 8
  sc_fw     <- 12
  fig_units <- "in"
  fig_dpi   <- 200

  domains       <- unique(names(sc_input))
  scores_tables <- names(sc_input[[1]][[1]])
  if (all(grepl("det_",scores_tables))) {
    fcst_type   <- "det"
  } else if (all(grepl("ens_",scores_tables))) {
    fcst_type   <- "ens"
  } else {
    stop("Input does not look like a harpPoint verification object, aborting")
  }

  df_all <- NULL
  for (dc in domains) {
    
    q      <- sc_input[[dc]]
    params <- names(q)
      
    for (pc in params) {
      
      qq <- q[[pc]]
      if (fcst_type == "ens") {
        qqq <- qq[["ens_summary_scores"]]
      } else if (fcst_type == "det") {
        qqq <- qq[["det_summary_scores"]]
      }
      qqq$domain       <- dc
      qqq$parameter    <- pc
      qqq$num_stations <- attr(qq,"num_stations")
      qqq$num_cycles   <- attr(qq,"num_cycles")
      
      df_all <- dplyr::bind_rows(df_all,qqq)
      
    }
  }
  
  # Filter leadtimes used in plotting
  if (leadtimes[1] != "All") {
    df_all <- df_all %>% dplyr::filter(lead_time %in% leadtimes)
  }

  # Make some additions
  df_all <- df_all %>% dplyr::mutate(
    difference_mean_round = signif(difference_mean,2),
    percent_diff_mean     = 100*((fcst_score_mean - ref_score_mean)/(ref_score_mean)))
  df_all <- df_all %>% dplyr::mutate(
    percent_diff_mean     = signif(percent_diff_mean,2))
  df_all <- df_all %>% dplyr::mutate(
    sig = dplyr::case_when(
      percent_better >= significance       ~ "Better",
      percent_better <= (1 - significance) ~ "Worse",
      TRUE                                 ~ "Neutral"
    )
  )
  df_all$signed_percent_diff_mean                          <- 
    df_all$percent_diff_mean
  df_all$signed_percent_diff_mean[df_all$sig == "Better"]  <-
    abs(df_all$signed_percent_diff_mean[df_all$sig == "Better"])
  df_all$signed_percent_diff_mean[df_all$sig == "Worse"]   <-
    -abs(df_all$signed_percent_diff_mean[df_all$sig == "Worse"])
  df_all$signed_percent_diff_mean[df_all$sig == "Neutral"] <- NA
  df_all$diff_text                                         <- 
    df_all$difference_mean_round
  df_all$diff_text[df_all$sig == "Neutral"]                <- NA

  #================================================#
  # LOOP OVER DOMAINS
  #================================================#
  
  # A function for labeling the facet titles
  label_facet <- function(scores_in){
    labels <- list()
    for (score_in in scores_in) {
      lab <- switch(score_in,
                    "rmse" = "RMSE",
                    "crps" = "CRPS",
                    "mean_bias" = "Mean Bias",
                    "fair_crps" = "Fair CRPS",
                    "spread" = "Spread",
                    stringr::str_to_title(score_in))
      labels[score_in] <- lab
    }
    return(labels)
  }
  
  for (dc in domains) {
    
    data_cur <- df_all %>% dplyr::filter(
      domain     == dc,
      fcst_model == fcstmodel,
      ref_model  == refmodel)
    
    sc_title1 <- paste0("Models: ",fcstmodel," vs ",
                      refmodel," (reference)")
    sc_title2 <- paste0("Station selection: ",dc,", Period: ",
      format(harpIO::str_datetime_to_datetime(tstart_date),"%Y-%m-%d-%H")," - ",
      format(harpIO::str_datetime_to_datetime(tend_date),"%Y-%m-%d-%H"),
      " (",num_cycles," cycles), Significance level: ",100*significance,"%")
    
    qwe_all <- list()
    for (ii in seq(1,length(scores_to_plot))) {
      qwe  <- data_cur %>% dplyr::filter(score == scores_to_plot[ii])
      cmin <- max(min(tidyr::drop_na(qwe)[["signed_percent_diff_mean"]]) - 0.1,-100)
      cmax <- min(max(tidyr::drop_na(qwe)[["signed_percent_diff_mean"]]) + 0.1,100)

      if (scores_to_plot[ii] %in% c("bias","mean_bias")) {
        # Only indicate better/worse with actual diff as text. Percentage
        # diff not really useful for bias.
        qwe$signed_percent_diff_mean[qwe$sig == "Better"] <- "Better"
        qwe$signed_percent_diff_mean[qwe$sig == "Worse"]  <- "Worse"
        gg <- ggplot2::ggplot(qwe,
                              aes(x     = lead_time,
                                  y     = (parameter),
                                  fill  = signed_percent_diff_mean,
                                  label = diff_text)) +
          ggplot2::geom_tile() +
          ggplot2::geom_text() +
          ggplot2::scale_fill_manual(
            "",
            values   = c("Better" = "#6c91c6","Worse" = "#e6897f","NA" = "white"),
            na.value = "white",
            labels   = formatC(c("","",""),width = 6)) +
          ggplot2::facet_wrap(~score,
                              labeller = ggplot2::as_labeller(label_facet)) +
          ggplot2::theme_bw() +
          ggplot2::ylim(rev(unique(qwe$parameter))) +
          ggplot2::scale_x_continuous(breaks = unique(qwe$lead_time),
                                      expand = c(0,0)) +
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         axis.title.y     = ggplot2::element_blank(),
                         axis.title.x     = ggplot2::element_blank(),
                         axis.text        = ggplot2::element_text(size = 10),
                         strip.text       = ggplot2::element_text(size = 10),
                         legend.key.width = grid::unit(0.5,"cm"),
                         legend.text      = ggplot2::element_text(size = 10),
                         legend.position  = "right")
      } else {
        gg <- ggplot2::ggplot(qwe,
                              aes(x     = lead_time,
                                  y     = (parameter),
                                  fill  = signed_percent_diff_mean,
                                  label = diff_text)) +
          ggplot2::geom_tile() +
          ggplot2::geom_text() +
          ggplot2::scale_fill_gradient2(
            "%",
            guide   = ggplot2::guide_colourbar(title.position = "top"),
            labels  = function(x) formatC(x,width = 3),
            low     = "#cd4a44",
            mid     = "white",
            high    = "#2970b3",
            oob     = scales::squish,
            n.breaks = 5,
            na.value = "white",
            limits   = c(cmin,cmax)) +
          ggplot2::facet_wrap(~score,
                              labeller = ggplot2::as_labeller(label_facet)) +
          ggplot2::theme_bw() +
          ggplot2::ylim(rev(unique(qwe$parameter))) +
          ggplot2::scale_x_continuous(breaks = unique(qwe$lead_time),
                                      expand = c(0,0)) +
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         axis.title.y     = ggplot2::element_blank(),
                         axis.title.x     = ggplot2::element_blank(),
                         axis.text        = ggplot2::element_text(size = 10),
                         strip.text       = ggplot2::element_text(size = 10),
                         legend.text      = ggplot2::element_text(size   = 10,
                                                         family = "mono"),
                         legend.key.width = grid::unit(0.5,"cm"),
                         legend.position  = "right")
      }
      
      qwe_all[[ii]] <- gg
    }
        
    p_scrd <- qwe_all %>% 
      gridExtra::arrangeGrob(grobs = .,nrow = length(scores_to_plot)) %>% 
      gridExtra::grid.arrange(
        bottom = "Lead time [h]",
        top    = grid::textGrob(paste0(sc_title1," \n",sc_title2),
                                x     = 0.05,
                                hjust = 0))
    
    # Save
    png_fname <- fn_png_name(fcst_type,
                             "altscard",
                             "lt",
                             "All",
                             "All",
                             start_date,
                             end_date,
                             dc,
                             vlt = fcstmodel,
                             vth = refmodel,
                             projectname = png_projname)
    ggsave(p_scrd,
           filename = png_fname,
           path     = png_archive,
           width    = sc_fw,
           height   = sc_fh,
           units    = fig_units,
           dpi      = fig_dpi,
           device   = 'png',
           bg       = "white")
  }

}

