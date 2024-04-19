
#================================================#
# CONSTRUCT SCORECARDS AND SCORE DIFF+SIGNIF PLOTS
#================================================#

fn_scorecard_signif <- function(sc_input,
                                     scores_to_plot,
                                     sc_params,
                                     new_model,
                                     ref_model,
                                     png_archive,
                                     plot_signif = TRUE,
                                     verif_path = NA_character_,
                                     png_projname = NA_character_,
                                     fsd = NA_character_,
                                     fed = NA_character_){
  
  #================================================#
  # SOME INITIAL CHECKS AND
  # MANIPULATE THE DATA INTO A SUITABLE FORMAT 
  #================================================#
  
  # Is the input a file path to an rds file?
  if (!is.list(sc_input)) {
    
    cat("Signif score plotting: Reading scorecard rds file input\n")
    sc_data <- readRDS(sc_input)
    
  } else {
    
    # Reorganise some of the data if it is not in the "correct" format
    if ("sc_data" %in% names(sc_input[[1]])) {
      
      cat("Signif score plotting: Reorganise data and save for future use\n")
      
      sc_data_domain <- list()
      
      for (pc in names(sc_input)) {
        sc_c <- sc_input[[pc]][["sc_data"]]
        if (!is.null(sc_c)) {
          for (dc in names(sc_c)) {
            sc_cc <- sc_c[[dc]]
            # Need to handle pressure level data carefully
            if ("p" %in% names(sc_cc[[1]])) {
              all_available_l <- unique(sc_cc[[1]][["p"]])
              for (c_pl in all_available_l) {
                new_UA_p <- paste0(pc,c_pl)
                c_df     <- sc_cc %>% harpPoint::filter_list(p == c_pl) 
                sc_data_domain[[dc]][[new_UA_p]] <- c_df %>%
                  harpPoint::select_list(-p) # Remove pressure entry from data 
                attributes(sc_data_domain[[dc]][[new_UA_p]])$parameter <- new_UA_p
              }
            } else {
              sc_data_domain[[dc]][[pc]] <- sc_cc
            } # if UA variable
          } # loop over domains
        } # is.null(sc_c)
      } # loop over parameters
      
      # Finally, reorder the data according to the order in scorecards$parameters
      for (dc in names(sc_data_domain)) {
        target_order         <- base::intersect(sc_params,
                                                names(sc_data_domain[[dc]]))
        ov                   <- base::match(target_order,
                                            names(sc_data_domain[[dc]]))
        sc_data_domain[[dc]] <- sc_data_domain[[dc]][ov]
      }
      
      dttm_avail   <- attributes(sc_data_domain[[1]][[1]])$dttm
      if (is.list(dttm_avail)) {
        dttm_avail <- dttm_avail %>% unlist() %>% unique()
      }
      dttm_avail   <- dttm_avail[order(dttm_avail)]
      
      # Get start and end dates
      d_out        <- get_sedate(fsd,
                                 fed,
                                 dttm_avail[1],
                                 tail(dttm_avail,1))
      start_date   <- d_out$sdate
      end_date     <- d_out$edate
   
      fcst_models  <- sort(unique(sc_data_domain[[1]][[1]][[1]][["fcst_model"]]))
      
      # Save the data for future use
      # This filename is liable to be overwritten!
      sc_data_fname     <- paste("harpScData",
                                 start_date,
                                 end_date,
                                 paste0(fcst_models,collapse = "-"),
                                 sep = "-")
      sc_data_fname     <- paste0(sc_data_fname,".rds")
      saveRDS(sc_data_domain,file = file.path(verif_path,sc_data_fname))
      sc_data_fname_out <- file.path(verif_path,sc_data_fname)
      sc_data           <- sc_data_domain
      
    } else {
      
      cat("Signif score plotting: Assuming input data is in correct format\n")
      sc_data           <- sc_input
      sc_data_fname_out <- NA
      
    }
    
  } # is.list(sc_input)
  
  dttm_avail <- attributes(sc_data[[1]][[1]])$dttm
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
  num_cycles   <- attr(sc_data[[1]][[1]],"num_cycles")
  fcst_models  <- sort(unique(sc_data[[1]][[1]][[1]][["fcst_model"]]))

  if (all(grepl("det_",names(sc_data[[1]][[1]])))) {
    fcst_type <- "det"
  } else if (all(grepl("ens_",names(sc_data[[1]][[1]])))) {
    fcst_type <- "ens"
  } else {
    stop("Input does not have the correct scorecard data format, aborting")
  }
  
  # Check png_archive for s/edate
  png_archive <- check_png_sedate(start_date,end_date,png_archive)
  
  #================================================#
  # SCORECARD AND SIGNIFICANT DIFFERENCE PLOTTING
  #================================================#
  
  # Define some figure widths/heights
  fw        <- 7
  fh        <- 4.5
  fig_units <- "in"
  fig_dpi   <- 200
  
  # Choosing the appropriate figure height and width for scorecards is a 
  # little awkward (depends on number of params+scores)
  sc_fh <- 8
  sc_fw <- 12
  
  for (dc in names(sc_data)) {
    
    sc_current <- sc_data[[dc]]
    sc_df      <- harpPoint::bind_point_verif(sc_current)
    
    #================================================#
    # SCORECARD 
    #================================================#
    
    p_scrd <- harpVis::plot_scorecard(
      sc_df,
      fcst_model = new_model,
      ref_model  = ref_model,
      scores     = scores_to_plot
    )
    
    # Add a title
    pt <- paste0("Station selection: ",dc,", Period: ",
          format(harpIO::str_datetime_to_datetime(tstart_date),"%Y-%m-%d-%H"),
          " - ",
          format(harpIO::str_datetime_to_datetime(tend_date),"%Y-%m-%d-%H"),
          " (",num_cycles," cycles)")
    
    p_scrd <- p_scrd + ggplot2::ggtitle(pt) +
      ggplot2::theme(text = ggplot2::element_text(size = 10))
    
    # Save
    png_fname <- fn_png_name(fcst_type,
                             "scard",
                             "lt",
                             "All",
                             "All",
                             start_date,
                             end_date,
                             dc,
                             vlt = new_model,
                             vth = ref_model,
                             projectname = png_projname)
    
    ggplot2::ggsave(p_scrd,
           filename = png_fname,
           path     = png_archive,
           width    = sc_fw,
           height   = sc_fh,
           units    = fig_units,
           dpi      = fig_dpi,
           device   = 'png',
           bg       = "white")
    
    #================================================#
    # DIFFS
    #================================================#
    
    if (plot_signif) {
      
      # Loop over available params and scores
      for (param_c in names(sc_current)) {
        
        p_current <- sc_current[[param_c]]
        
        for (score_c in scores_to_plot) {
          
          # Assuming 95% confidence interval 
          p_diff <- fn_plot_signif_diff(p_current,
                                        fcst_type,
                                        score_c,
                                        new_model,
                                        ref_model,
                                        significance = 0.95,
                                        conf = "ribbon",
                                        domain = dc,
                                        num_cycles = num_cycles)
          
          png_fname <- fn_png_name(fcst_type,
                                   paste0("sdiff",score_c),
                                   "lt",
                                   param_c,
                                   "All",
                                   start_date,
                                   end_date,
                                   dc,
                                   vlt = new_model,
                                   vth = ref_model,
                                   projectname = png_projname)
          
          ggplot2::ggsave(p_diff,
                 filename = png_fname,
                 path     = png_archive,
                 width    = fw,
                 height   = fh,
                 units    = fig_units,
                 dpi      = fig_dpi,
                 device   = 'png')
          
        } # Score loop
      } # Param loop
    } # Plot signig
    
  } # Loop over domain
  
  # Return the name of the saved scorecards rds file
  return(sc_data_fname_out)
  
} # Fn
