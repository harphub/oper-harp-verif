#================================================#
# COMMON HELPER FUNCTIONS 
#================================================#

#================================================#
# PACKAGES
#================================================#

suppressPackageStartupMessages({
  library(harp)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(RSQLite)
  library(lubridate)
})

#================================================#
# SILENT STOP WHEN RUNNING USING Rscript
# WHEN IN RSTUDIO, DEFAULT BACK TO STOP TO AVOID
# TERMINATING THE SESSION
#================================================#

silent_stop <- function(errmsg = "Stopping") {
  
  if (!interactive()) {
    cat(paste0(errmsg,"\n"))
    quit(save = "no", status = 0)
  } else {
    stop(errmsg)
  }
  
}

#================================================#
# CHECK IF START AND END DATE ARE OKAY
#================================================#

check_sedate <- function(start_date,end_date) {
  
  sdate <- NULL
  edate <- NULL
  
  if (nchar(start_date) == 10) {
    sdate <- start_date
  }
  if (nchar(end_date) == 10) {
    edate <- end_date
  }
  if (nchar(start_date) == 8) {
    # i.e. YYYYMMDD format - assume the first cycle of the day
    sdate <- paste0(start_date,"00")
  }
  if (nchar(end_date) == 8) {
    # i.e. YYYYMMDD format - assume the last possible cycle of the day
    edate <- paste0(end_date,"23")
  }
  if (nchar(start_date) == 6) {
    # i.e. YYYYMM format - assume the first day and cycle of the month
    sdate <- paste0(start_date,"0100")
  }
  if (nchar(end_date) == 6) {
    # i.e. YYYYMM format - assume the last day and possible cycle of the month
    lastday <- lubridate::days_in_month(harpCore::as_dttm(paste0(end_date,"01"))) %>%
      unname()
    edate <- paste0(end_date,lastday,"23")
  }
  
  if ((!is.null(sdate)) & (!is.null(edate))) {
    cat("Looking at date range",sdate,"-",edate,"\n")
  } else {
    stop("The input start/end_date should be 6, 8, or 10 characters!")
  }
  
  return(list("start_date" = sdate,
              "end_date"   = edate))
}

#================================================#
# CHECK IF CONFIG FILE OPTIONS EXIST
#================================================#

check_config_input <- function(config,x,y,default="None") {
  
  val <- config[[x]][[y]]
  
  if (is.null(val)) {
    cat("Could not find",x,":",y,"in the config\n")
    if (default == "None") {
      stop("No default value set for ",y,", aborting")
    } else {
      cat("Setting",y,"=",default[[1]],"\n")
      val <- default
    }
  }
  
  return(val)
  
}

#================================================#
# CHECK IF DIRECTORIES EXIST
#================================================#

check_dirs_exist <- function(dir_vec) {
  
  for (cdir in dir_vec) {
    if (!dir.exists(cdir)) {
      stop(cdir," does not exist")
    }
  }
  
}

#================================================#
# CREATE A NAMED LIST ASSOCIATING fcst_model WITH A
# INPUT LIST (E.G. MEMBERS/LAGS FOR EPS EXPERIMENTS)
#================================================#

get_named_list <- function(input_str,
                           fcst_model,
                           str_descrip){
  
  input_str <- as.list(input_str)
  if (length(input_str) == 1) {
    cat("Using the same verif:",str_descrip,"value for each forecast model\n")
    input_str <- rep(input_str,length(fcst_model))
  } else if (length(input_str) == length(fcst_model)) {
    cat("Assuming that verif:",str_descrip,"follows the same order as verif:fcst_model\n")
  } else {
    stop("verif:input is neither of length 1 or the same length as fcst_model, aborting.")
  }
  out <- input_str
  for (ii in seq(1,length(input_str),1)) {
    if (!is.null(input_str[[ii]])) {
      # Deal with case where lags is specified as "0h" etc, not in c() format
      if (any(input_str[[ii]] %in% paste0(seq(0,23),"h"))) {
        out[[ii]] <- input_str[[ii]]
      } else {
        out[[ii]] <- eval(parse(text = input_str[[ii]]))
      }
    }
  }
  out_list = stats::setNames(out,fcst_model)
  return(out_list)
}

#================================================#
# CONVERT A HARMONIE ALLSYNOP LIST INTO SOMETHING
# USEABLE FOR harp
#================================================#

conv_allsynop <- function(input_list){
  
  synop  <- utils::read.table(input_list,
                              header    = FALSE,
                              sep       = "",
                              fill      = TRUE,
                              col.names = paste0("V",seq_len(20)))
  
  # Reorganise synop into something that looks like a harp station list
  synop_n     <- synop %>% dplyr::select(-V1,-V2,-V3,-V4)
  synop_n     <- tidyr::unite(synop_n,"name",V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,
                              V15,V16,V17,V18,V19,V20,sep = " ",na.rm = TRUE) %>% 
                              dplyr::mutate("name" = stringr::str_trim(name))
  synop_c     <- synop %>% dplyr::select(V1,V2,V3,V4)
  colnames(synop_c) <- c("SID","lat","lon","elev")
  synop_c     <- synop_c %>% dplyr::mutate(name = synop_n$name)
  synop_list  <- tibble::as_tibble(synop_c)
  rm(synop,synop_n,synop_c)
  return(synop_list)
}


#================================================#
# MODIFY THE STATION ELEVATION IN THE STATIONS FILE
# TO THE ACTUAL ELEVATION FROM THE OBSERVATIONS
#================================================#

modify_station_elev <- function(stations_rf,
                                obs_path,
                                start_date,
                                output_diag = FALSE) {
  
  # Read in the obstable
  yyyy <- substr(start_date,0,4)
  sql_file <- file.path(obs_path,paste0("OBSTABLE_",yyyy,".sqlite"))
  
  if (!file.exists(sql_file)) {
    
    cat("Could not find OBSTABLE for correcting station elev, skipping\n")
    return(stations_rf)
    
  } else {
    
    con        <- DBI::dbConnect(drv    = RSQLite::SQLite(),
                                 dbname = sql_file)
    sql_synop  <- DBI::dbGetQuery(conn      = con,
                                  statement = "SELECT SID,elev FROM SYNOP") %>%
      as_tibble() %>% distinct() %>% filter(elev != -99) %>%
      group_by(SID) %>% summarise(obs_elev = mean(elev))
    DBI::dbDisconnect(con)
    
    # Join and update elev with OBSTABLE elev if it exists
    onames      <- colnames(stations_rf)
    stations_rf <- left_join(stations_rf,sql_synop,by = "SID") %>%
      mutate(obs_elev_filtered = case_when(
        is.na(obs_elev) ~ elev,
        .default = obs_elev),
        asl_elev = elev) %>% 
      mutate(elev_diff = abs(asl_elev - obs_elev_filtered)) %>%
      mutate(elev = obs_elev_filtered)
    
    if (output_diag) {
      out_name <- "asl_corrected_elev.rds"
      cat("Saving",out_name,"with updated station elev info\n")
      saveRDS(stations_rf, file = here("pre_processing/",out_name))
    }
    
    stations_rf <- stations_rf %>% select(all_of(onames))
    return(stations_rf)
  }
  
}

#================================================#
# CALL THE VERIFICATION FUNCTION FOR DIFFERENT
# GROUPINGS
#================================================#

fn_run_verif_groups <- function(fcst = "",
                                prm_name = "",
                                vertical_coordinate = "",
                                fcst_type = "",
                                num_ref_members = "",
                                grps_param = "",
                                grps_threshold = "",
                                grps_SID = "",
                                all_station_groups = "",
                                thresholds_param = "",
                                create_png = "",
                                force_valid_thr = "",
                                t_s_str = "",
                                run_sid = T){
  
  # Initialise output
  verif        <- NULL
  verif_tpplot <- NULL
  verif_sid    <- NULL
  
  #================================================#
  # FIRST SET VERIF OPTIONS 
  #================================================#
  
  # Generate verification options
  if (fcst_type == "eps") {
    verif_fn              <- "ens_verify"
    verif_options_list_nm <- list(parameter       = {{prm_name}},
                                  num_ref_members = num_ref_members,
                                  verify_members  = FALSE,
                                  rank_hist       = FALSE,
                                  crps            = FALSE,
                                  crps_decomp     = FALSE,
                                  hexbin          = FALSE)
    verif_options_list    <- list(parameter       = {{prm_name}},
                                  num_ref_members = num_ref_members,
                                  verify_members  = TRUE,
                                  hexbin          = FALSE)
  } else if (fcst_type == "det") {
    verif_fn              <- "det_verify"
    verif_options_list    <- list(parameter = {{prm_name}},
                                  hexbin    = FALSE)
    verif_options_list_nm <- verif_options_list
  }
  
  # Get units
  par_unit <- unique(fcst[[1]][["units"]])
  
  #================================================#
  # RUN STANDARD VERIFICATION
  #================================================#
  
  num_fcst_cycles <- length(unique(fcst[[1]][["fcst_cycle"]]))
  if (num_fcst_cycles == 1) {
    warning("Only one fcst_cycle exits, removing it as a group")
    grps_param     <- lapply(grps_param,function(x) x[x != "fcst_cycle"])
    grps_param     <- grps_param[lapply(grps_param,length) > 0]
    grps_param     <- unique(grps_param)
    grps_threshold <- list("station_group")
  }
  
  cat("Running standard verification...\n")
  # Do not compute threshold score for valid_hour/dttm unless explicitly requested
  if (is.null(thresholds_param)) {
    # Threshold scores not considered in this case
    grps_1 <- grps_param
  } else {
    if (force_valid_thr) {
      cat("Using all groups (including valid_dttm/hour) for threshold scores\n")
      grps_1 <- grps_param
    } else {
      cat("Only grouping over lead_time for threshold scores\n")
      grps_1 <- grps_param[grepl("lead_time",grps_param)]
    }
  }
  verif <- do.call(
    get(verif_fn),
    c(list(.fcst      = fcst,
           thresholds = thresholds_param,
           groupings  = grps_1),
      verif_options_list)
  ) %>% fn_verif_rename(.,par_unit)
  
  # Then compute summary scores for valid_dttm/hour
  # Only required if:
  # 1) threshold scores are activated
  # 2) valid_dttm/hour have not been used above i.e. force_valid_thr=F
  if ((!is.null(thresholds_param)) & (!force_valid_thr)) {
    # First store the threshold type in verif
    vttype <- typeof(verif[[2]]$threshold[[1]])
    
    verif_others <- do.call(
      get(verif_fn),
      c(list(.fcst      = fcst,
             thresholds = NULL,
             groupings  = grps_param[!grepl("lead_time",grps_param)]),
        verif_options_list)
    ) %>% fn_verif_rename(.,par_unit)
    verif <- bind_point_verif(verif,verif_others) %>%
      select_list(-parameter)
    # Add in valid_hour and valid_dttm = All 
    verif[[2]] <- verif[[2]] %>% mutate(valid_hour = "All",
                                        valid_dttm = "All")
    
    # After binding, it may be necesssary to switch "threshold" back to it's
    # original type (e.g. changed from dbl to "chr")
    nvttype <- typeof(verif[[2]]$threshold[[1]])
    if (nvttype != vttype){
      cat("After bpverif, threshold type changed from",vttype,"to",nvttype,"\n")
      if (vttype == "double") {
        verif[[2]] <- verif[[2]] %>% mutate(threshold  = as.double(threshold))
      } else if (vttype == "character") {
        verif[[2]] <- verif[[2]] %>% mutate(threshold  = as.character(threshold))
      } else if (vttype == "integer") {
        verif[[2]] <- verif[[2]] %>% mutate(threshold  = as.integer(threshold))
      } else {
        stop("Why are we here?")
      }
    }
    
    # par_unit missing after binding
    attr(verif,"par_unit") <- par_unit
  }
  
  # Save object to one used for plotting and manipulate as required
  verif_toplot <- verif
  if (!is.null(verif_toplot[[t_s_str]])) {
    verif_toplot[[t_s_str]][["lead_time"]] <- 
      as.character(verif_toplot[[t_s_str]][["lead_time"]])
  }
  
  # Add all lead times used as an attribute
  attr(verif_toplot,"all_lts_avail") <- as.character(sort(unique(fcst[[1]]$lead_time)))
  
  #================================================#
  # RUN SID AND ALL THRESHOLD VERIFICATION FOR
  # SURFACE PARAMS
  #================================================#
  
  if (is.na(vertical_coordinate) & (create_png)) {
    
    #================================================#
    # SID/MAP SCORES
    #================================================#
    
    if (run_sid){
      
      cat("Running verification over individual stations...\n")
      # Reduce the number of valid_hours as computing map scores is intense
      # Generally looks at valid hours 00-21:3, but only do this filtering 
      # if all of these hours exist in the data
      avail_vhours <- unique(fcst[[1]][["valid_hour"]]) %>% as.integer()
      run_average  <- F
      if (all(seq(0,21,3) %in% avail_vhours)) {
        cat("Using valid hours 0-21:3 when computing map scores\n")
        fcst_sid_tmp <- fcst %>% 
          harpPoint::filter_list(valid_hour %in% sprintf("%02d",seq(0,21,3)))
        run_average  <- T
        grps_SID     <- list(c("SID","valid_hour")) # Remove the group over SID only
      } else {
        # In this case just use whatever is available
        cat("Using valid hours",avail_vhours,"when computing maps scores\n")
        fcst_sid_tmp <- fcst
      }
      # Check if we should remove valid_hour as a group if only one is present
      num_valid_hours <- length(unique(fcst_sid_tmp[[1]][["valid_hour"]]))
      if (num_valid_hours == 1) {
        warning("Only one valid hour exists, removing it as a group")
        grps_SID <- list("SID")
      }
      
      # Compute average scores using the entire dataset if fcst_sid_tmp != fcst_sid
      if (run_average) {
        verif_sid_average <- do.call(
          get(verif_fn),
          c(list(.fcst      = fcst,
                 thresholds = NULL,
                 groupings  = list("SID")),
            verif_options_list_nm)
        ) %>% fn_verif_rename(.,par_unit)
        # Need to add in valid_hour afterwards
        verif_sid_average[[1]][["valid_hour"]] <- "All"
      }
      verif_sid <- do.call(
        get(verif_fn),
        c(list(.fcst      = fcst_sid_tmp,
               thresholds = NULL,
               groupings  = grps_SID),
          verif_options_list_nm)
      ) %>% fn_verif_rename(.,par_unit)
      if (run_average) {
        verif_sid[[1]] <- bind(verif_sid_average[[1]],verif_sid[[1]])
      }
      # Add all lead times used as an attribute 
      attr(verif_sid,"all_lts_avail") <- as.character(sort(unique(fcst_sid_tmp[[1]]$lead_time)))
      rm(fcst_sid_tmp)
      
      # Need to add lat/lon to the SIDs
      verif_sid <- fn_sid_latlon(verif_sid,fcst)
      
      # Join station_groups to verif object
      for (vn in names(verif_sid)) {
        verif_sid[[vn]] <- dplyr::inner_join(verif_sid[[vn]],
                                             all_station_groups,
                                             by = "SID",
                                             relationship = "many-to-many")
      }
      
    } else {
      
      cat("Skipping verification over individual stations...\n")
      
    }
    
    #================================================#
    # THRESHOLD SCORES OVER ALL LEAD TIMES
    #================================================#
    
    # Compute the standard threshold scores over all lead_times and add
    if (!(is.null(thresholds_param))) {
      cat("Running all threshold verification...\n")
      verif_alllt <- do.call(
        get(verif_fn),
        c(list(.fcst      = fcst,
               thresholds = thresholds_param,
               groupings  = grps_threshold),
          verif_options_list_nm)
      ) %>% fn_verif_rename(.,par_unit)
      
      if (!is.null(verif_alllt[[t_s_str]])) {
        verif_alllt[[t_s_str]] <- dplyr::mutate(verif_alllt[[t_s_str]],
                                                lead_time  = "All",
                                                valid_hour = "All",
                                                valid_dttm = "All") %>%
          dplyr::select(names(verif_toplot[[t_s_str]]))
        cat("Adding threshold scores over all lead_times\n")
        verif_toplot[[t_s_str]] <- dplyr::bind_rows(verif_toplot[[t_s_str]],
                                                    verif_alllt[[t_s_str]])
      }
    } # threshold flag
    
    # Filter threshold scores to cases where we have a significant number of cases
    min_thr_cases <- 20
    if (!is.null(verif_toplot[[t_s_str]])) {
      if (fcst_type == "det") {
        obs_col_name <- "num_cases_for_threshold_observed"
      } else if (fcst_type == "eps") {
        obs_col_name <- "num_cases_observed"
      } else {
        stop("How did this happen?")
      }
      if (min(verif_toplot[[t_s_str]][[obs_col_name]]) < min_thr_cases) {
        message(paste0("The min number of observed cases in the threshold scores = ",
                       min(verif_toplot[[t_s_str]][[obs_col_name]]),
                       ", skipping filtering!"))
      } else {
        verif_toplot[[t_s_str]] <- verif_toplot[[t_s_str]] %>% 
          dplyr::filter(get(obs_col_name) >= min_thr_cases)
      }
    }
    
  } # is.na(vertical_coordinate)
  
  return(list("verif"        = verif,
              "verif_toplot" = verif_toplot,
              "verif_sid"    = verif_sid))
  
}

#================================================#
# RENAMING OF VERIFICATION GROUPS E.G. THOSE WITH
# NA OR "00;12", ETC
#================================================#

fn_verif_rename <- function(df,par_unit){
  
  for (ii in seq(1,length(df),1)) {
    for (var_rename in c("fcst_cycle","station_group","valid_hour","valid_dttm","lead_time")) {
      if (var_rename %in% names(df[[ii]])) {
        
        v_avail  <- unique(df[[ii]][[var_rename]])
        v_rename <- v_avail[grep(";",v_avail)]
        
        # Rename any columns with ";" to "All"
        if (length(v_rename) > 0) {
          df[[ii]] <- dplyr::mutate(df[[ii]],
                                    "{var_rename}" := harpCore::psub(get(var_rename),
                                                      {{v_rename}},
                                                      "All"))
        }
        
        # Rename any NAs introduced by NULL grouping to "All"
        if (sum(is.na(df[[ii]][[var_rename]])) > 0) {
          stop("CHECK!")
          df[[ii]][is.na(df[[ii]][[var_rename]]),var_rename] <- "All" 
        }
        
      }
    }
  }
  
  attributes(df)$par_unit <- par_unit
  
  return(df)
}

#================================================#
# SPLIT THRESHOLD INTO VALS AND COMPARATOR
# (TO ACCOMMODATE HARP VERSIONS 0.2 - 0.3)
#================================================#

fn_split_thr <- function(vo){
  
  if (any(grepl("threshold",names(vo),fixed=T))) {
    
    vttype <- typeof(vo[[2]]$threshold[[1]])
    # If character, then check if we have to split 
    if (vttype == "character") {
      
      # Use numbers only to see if comparator is in there
      comp_exists <- numbers_only(vo[[2]]$threshold[[1]])
      if (!comp_exists) {
        
        # Str split by "_"
        len_split <- length(str_split(vo[[2]]$threshold,"_")[[1]])
        if (len_split == 2) {
          comp_val  <- unique(unlist(lapply(str_split(vo[[2]]$threshold,"_"),"[",1)))
          thr_vals  <- as.double(unlist(lapply(str_split(vo[[2]]$threshold,"_"),"[",2)))
          thr_brks  <- unique(thr_vals)
        } else if (len_split == 4) {
          comp_vals1 <- unique(unlist(lapply(str_split(vo[[2]]$threshold,"_"),"[",1)))
          thr_vals1  <- as.double(unlist(lapply(str_split(vo[[2]]$threshold,"_"),"[",2)))
          comp_vals2 <- unique(unlist(lapply(str_split(vo[[2]]$threshold,"_"),"[",3)))
          thr_vals2  <- as.double(unlist(lapply(str_split(vo[[2]]$threshold,"_"),"[",4)))
          # Use midpoint as thr_vals
          thr_vals   <- (thr_vals1 + thr_vals2)/2
          thr_brks   <- unique(thr_vals1,thr_vals2)
          if (comp_vals1 == "ge") {
            comp_val <- "between"
          } else if (comp_vals1 == "le") {
            comp_val <- "outside"
          } else {
            stop("Threshold split issue")
          }
        } else {
          stop("Threshold split issue")
        }
        
        vo[[2]]$threshold_val <- thr_vals
        
      } else {
        # No comparator, so assumed gt (which was the default before "comparator" 
        # argument was introduced). Can convert straight to double
        vo[[2]]  <- vo[[2]] %>% mutate(threshold_val = as.double(threshold))
        comp_val <- "gt"
        thr_brks <- unique(vo[[2]]$threshold_val)
      }
    } else {
      # No comparator, so assumed gt (which was the default before "comparator" 
      # argument was introduced).
      vo[[2]] <- vo[[2]] %>% mutate(threshold_val = threshold)
      comp_val <- "gt"
      thr_brks <- unique(vo[[2]]$threshold_val)
    }
    
    # Finally, add attributes
    attr(vo,"thr_brks") <- sort(thr_brks)
    attr(vo,"comp_val") <- comp_val
    
  }
  
  return(vo)
  
}

#================================================#
# GET LAT/LONS FROM HARP FCST OBJECT
#================================================#

get_latlon <- function(df,
                       fc,
                       ind,
                       print_warning = TRUE){
  
  sid_toget <- df[[ind]][["SID"]]
  lat_sids  <- NULL
  lon_sids  <- NULL
  fc_tmp    <- fc[[1]] 
  
  for (sid_i in sid_toget) {
    
    lat_v <- unique(fc_tmp$lat[fc_tmp$SID == sid_i])
    lon_v <- unique(fc_tmp$lon[fc_tmp$SID == sid_i]) 
    
    # NAs can sometimes occur at random leadtimes for Pcp - vfld2sql processing?
    lat_v <- lat_v[!is.na(lat_v)]
    lon_v <- lon_v[!is.na(lon_v)]
    
    if ((length(lat_v) == 0) || (length(lon_v) == 0)) {
      if (print_warning) {
        cat("Warning: Station",sid_i,
            "has no associated lat/lon value. Assigning NA\n")
      }
      lat_v <- NA
      lon_v <- NA
    }
    
    if ((length(lat_v) > 1) || (length(lon_v) > 1)) {
      
      if (print_warning) {
        cat("Warning: Why multiple lat/lons for SID=",sid_i,
            "? Will use workaround here. For reference, values are:\n")
        cat("Lat:",paste0(lat_v,collapse = ","),
            "and Lon:",paste0(lon_v,collapse = ","),"\n")
      }
      
      lat_v <- lat_v[1]
      lon_v <- lon_v[1]
     
    }
    
    lat_sids <- c(lat_sids,lat_v)
    lon_sids <- c(lon_sids,lon_v)
  }
  
  rm(fc_tmp)
  return(list("lat_sids" = lat_sids,
              "lon_sids" = lon_sids))
}

#================================================#
# ADD LAT/LON VALUES TO HARP VERIF OBJECT FOR SIDS
#================================================#

fn_sid_latlon <- function(df,
                          fc,
                          print_warning = TRUE){
  
  ll1         <- get_latlon(df,fc,1,print_warning)
  df[[1]]$lat <- ll1$lat_sids
  df[[1]]$lon <- ll1$lon_sids
  
  # Do the same for deterministic scores in the case of an ensemble 
  if (length(df) > 2) {
    if (nrow(df[[3]]) > 0) {
      ll3         <- get_latlon(df,fc,3,print_warning)
      df[[3]]$lat <- ll3$lat_sids
      df[[3]]$lon <- ll3$lon_sids
    }
  }
  
  return(df)
}

#================================================#
# FILTER THE NUM CASES IN VERIF OBJECT
#================================================#

# Filter verif object dataframe
filter_verif <- function(verif_o,
                         fts,
                         cf,
                         force_vh = FALSE){
  
  # Only filter cases for lead_time, not valid_dttm or valid_hour
  verif_out <- verif_o
  verif_f   <- verif_o %>% 
    harpPoint::filter_list(dplyr::case_when(
      lead_time != "All" ~ num_cases >= cf,
      TRUE ~ num_cases >= 1)
    )
  
  # Force valid_hour filtering
  if (force_vh) {
    max_vh_cases <- verif_f[[paste0(fts,"_summary_scores")]] %>%
      filter(lead_time == "All")
    max_vh_cases <- max(max_vh_cases$num_cases)
    verif_f  <- verif_f %>%
      harpPoint::filter_list(dplyr::case_when(
        lead_time == "All" ~ num_cases >= round(max_vh_cases/4),
        TRUE ~ num_cases >= 1)
      )
  }
    
  # Check the filtered object
  qwe <- verif_f[[paste0(fts,"_summary_scores")]] %>% 
    dplyr::filter(lead_time != "All") 
  
  if (nrow(qwe) == 0) {
    cat("Filtering using a minimum number of cases of",cf,"is overly aggressive!\n")
    cat("Using the entire dataframe instead of the filtered dataframe\n")
  } else {
    verif_out <- verif_f
  }
  
  return(verif_out)
}

#================================================#
# QUERY THE SQLITE TABLES TO ASSESS WHICH MODEL
# HAS THE FEWEST NUMBER OF STATIONS
#
# THIS IS USEFUL FOR REDUCING THE DATA VOLUME
# INITIALLY READ IN
#================================================#

create_station_filter <- function(dttm,
                                  fcst_model,
                                  param,
                                  fctable_dir,
                                  station_list_dir){

  YYYY <- substr(dttm,0,4)
  MM   <- substr(dttm,5,6)
  HH   <- substr(dttm,9,10)
  if (grepl("AccPcp",param,fixed = TRUE)) {
    param <- "Pcp"
  }
  
  # Read the standard list of synop sids from Harmonie allsynop.list (pointing 
  # to a pre-generated filtered version here). This list is used for comparison 
  # with the SIDs in the FCTABLE and helps to avoid potential issues where 
  # additional stations have been added into local vfld files. This will not 
  # filter out local stations, it just compares synop only as a proxy for "size"
  allsynop_sids        <- file.path(station_list_dir,"allsynop_sids.list") 
  synop_list           <- utils::read.table(allsynop_sids,header = FALSE) %>%
                          tibble::as_tibble()
  colnames(synop_list) <- "SID"
  
  # Loop over all models
  num_sids_running <- 1e20
  for (fm in fcst_model) {
    
    # Read the FCTABLE for the input parameter (assuming the default directory 
    # structure for FCTABLE).
    dbase <- file.path(fctable_dir,
                       fm,
                       YYYY,
                       MM,
                       paste0("FCTABLE_",param,"_",YYYY,MM,"_",HH,".sqlite"))
    
    # Query the database and get the number of stations
    if (file.exists(dbase)) {
      cat("Create station filter: Reading FCTABLE for",fm,"\n")
      db         <- DBI::dbConnect(RSQLite::SQLite(), dbase)
      num_sids   <- tbl(db, "FC") %>% 
                    dplyr::pull(SID) %>%
                    unique() %>%
                    as.double()
      # Compare to default synop list
      num_sids   <- base::intersect(num_sids,synop_list$SID) %>%
                    length() 
      DBI::dbDisconnect(db)
      
      if (num_sids < num_sids_running) {
        num_sids_running <- num_sids
        model_domain_min <- fm
      }
      
    } else {
      warning("An error occured in create_station_filter due to a missing table")
      model_domain_min <- NA_character_
      break
    }
  }
  if (!is.na(model_domain_min)) {
    cat("Model",model_domain_min,"has the fewest number of stations\n")
  }
  return(model_domain_min)
}

#================================================#
# READ POINT FORECAST WRAPPED IN TRYCATCH
#================================================#

try_rpforecast <- function(start_date,
                           end_date,
                           by_step,
                           fcst_model,
                           fcst_type,
                           param,
                           lead_time,
                           members,
                           lags,
                           fcst_path,
                           stations,
                           vc){
  
  # With harp v0.3+ it's best to use merge lags directly when reading. Confirmed
  # that this works fine for surface and UA variables.
  if (as.numeric(substr(packageVersion("harpIO"),1,3)) >= 0.3) {
    ml <- T
  } else {
    ml <- F
  }
  
  fcst <- tryCatch(
    {
      harpIO::read_point_forecast(
        dttm                = harpCore::seq_dttm(start_date,
                                                 end_date,
                                                 by = by_step),
        fcst_model          = fcst_model,
        fcst_type           = fcst_type,
        parameter           = param,
        lead_time           = lead_time,
        members             = members,
        lags                = lags,
        file_path           = fcst_path,
        stations            = stations,
        get_lat_and_lon     = TRUE, # Useful when lat/lon missing from vobs files
        merge_lags          = ml,
        vertical_coordinate = vc
      )
    },
    error = function(cond){
      cat("An error was detected during the FCTABLE reading for",param,"\n")
      cat("This is probably due to a missing parameter in a FCTABLE\n")
      cat("Here is the original message:\n")
      message(conditionMessage(cond))
      return(NULL)
    },
    finally = {
      cat("Finished FCTABLE reading\n")
    }
  )
  if (is.null(fcst)) {
    return(fcst)
  }

  # Add a check to test if read_forecast returned a named list
  # (where the names are the forecast models)
  # Also deals with converting to a harp list for a single forecast model
  if (!is.null(fcst)) {
    
    # If fcst has at least one of fcst_model, then we are fine. 
    # If not, then only a single model was found
    if (!(any(names(fcst) %in% fcst_model))) {
      
      avail_model <- unique(fcst$fcst_model)
      if (length(avail_model) > 1) {
        stop("Investigation required!")
      }
      fcst        <- harpCore::as_harp_list(placeholder = fcst)
      names(fcst) <- avail_model
    }
    
  }

  # Add a check on the number of rows (e.g. FCTABLE could exist, but it may not
  # include the leadtimes requested)
  if (!is.null(fcst)) {
    nrow_rolling <- 0
    for (ii in names(fcst)) {
      if (is.data.frame(fcst[[ii]])) {
        nrow_rolling <- max(nrow_rolling,nrow(fcst[[ii]]))
      } else {
        nrow_rolling <- max(nrow_rolling,length(fcst[[ii]]))
      }
    }
    if (nrow_rolling == 0){
      cat("Did not find any forecast data after reading the FCTABLEs\n")
      cat("This may be due to missing leadtime data in the sqlite files\n")
      cat("Parameter",param,"will be skipped\n")
      fcst <- NULL
      return(fcst)
    }
  }
  
  # In harp0.3 lead_time includes units. Check if lead_time is a character
  # and extract numeric if required.
  lt_units <- NULL
  for (ii in names(fcst)) {
    # Make sure all models have the same units and type (could this happen?)
    if (is.character(fcst[[ii]][["lead_time"]])) {
      clt_units <- unique(regmatches(fcst[[ii]][["lead_time"]],
                                     regexpr("[a-z]*$",fcst[[ii]][["lead_time"]])))
      if (is.null(lt_units)) {
        lt_units <- clt_units
      } else {
        lt_units <- c(lt_units,clt_units)
      }
    }
  }
  # If lt_units is null, nothing is required (assumed to be hourly). If not null,
  # check that it has the correct length and is unique
  if (is.null(lt_units)) {
    lt_unit_attr <- "h"
  } else {
    if (length(lt_units) != length(names(fcst))) {
      stop("Looks like models have different lead time types, aborting.")
    }
    if (length(unique(lt_units)) != 1) {
      stop("Model have different lead time units, aborting.")
    }
    lt_unit_attr <- unique(lt_units)
    # Using extract_numeric can be slow
    for (cm in names(fcst)) {
      fcst[[cm]][["lead_time"]] <- as.numeric(gsub(unique(lt_units),"",fcst[[cm]][["lead_time"]],fixed = T))
    }
    #fcst <- fcst %>% harpPoint::mutate_list(lead_time = harpCore::extract_numeric(lead_time))
  }

  # Add lt_untis as an attribute
  attributes(fcst)$lt_unit <- lt_unit_attr
  attributes(fcst)$ml      <- ml
  
  # Convert SID column from integer to double to protect against integer64
  fcst <- fcst %>% mutate_list(SID = as.double(SID))
  
  # Finally, check if lat/lon = NA and replace if so (something to do with
  # vfld processing of Pcp, not clear why)
  if (grepl("Pcp",param,fixed=T)) {
    for (cm in names(fcst)) {
      df <- fcst[[cm]]
      llna <- filter(df, is.na(lat) | is.na(lon))
      if (nrow(llna) > 0) {
        cat("NA lat/lon found after read_point_forecast - replace these with correct values.\n")
        vsids <- df %>% select(SID,lat,lon) %>% group_by(SID) %>%
          summarise(lat = min(lat,na.rm = T),lon = min(lon,na.rm = T))
        if (nrow(vsids) != length(unique(df$SID))) {
          stop("Lat/lon are missing for some stations, abort!")
        }
        df <- df %>% select(-lat,-lon)
        df <- inner_join(df,vsids,relationship = "many-to-many",by = "SID")
        fcst[[cm]] <- df
      }
    }
  }

  return(fcst)

}

#================================================#
# EXPLICIT LAGGING FOR EPS EXPERIMENTS
#================================================#

try_epslag <- function(fcst,
                       fcst_model,
                       parent_cycles,
                       vc){
  
  for (jj in fcst_model) {
    
    # Get available vertical levels
    vl_loop <- switch(
      vc,
      "pressure" = unique(fcst[[jj]][["p"]]),
      "height"   = unique(fcst[[jj]][["z"]]),
      c(1e20) # Dummy for surface variables
    )
    
    lagged_model <- NULL
    for (vl in vl_loop) {
      
      # Filter to vl
      fcst_vl <- switch(
        vc,
        "pressure" = fcst %>% harpPoint::filter_list(p == vl),
        "height"   = fcst %>% harpPoint::filter_list(z == vl),
        fcst
      )
      
      # Add in a tryCatch for cases where no model data exists
      aa <- tryCatch(
        {
          harpPoint::lag_forecast(
            fcst_vl,
            fcst_model    = jj,
            parent_cycles = parent_cycles
          )
        },
        error = function(cond){
          cat("An error was detected during the lagging for model",jj,"\n")
          cat("This is probably due to a missing parameter in a FCTABLE\n")
          cat("Here is the original message:\n")
          message(conditionMessage(cond))
          return(NULL)
        },
        finally = {
          if (vl == 1e20) {
            cat("Finished lagging for model",jj,"\n")
          } else {
            cat("Finished lagging for model",jj,"at vertical level",vl,"\n")
          }
        }
      )
      
      if (is.null(aa)) {
        cat("The lagging failed in try_epslag\n")
        return(NULL)
      }
    
      # Some cleaning here to remove lat/lon_lag* etc
      all_c_names    <- names(aa[[jj]])
      lat_lag_remove <- all_c_names[grepl("lat_lag",all_c_names)]
      lon_lag_remove <- all_c_names[grepl("lon_lag",all_c_names)]
      fcm_lag_remove <- all_c_names[grepl("fcst_model_lag",all_c_names)]
      aa[[jj]]       <- aa[[jj]] %>%
                        dplyr::select(-tidyselect::all_of(lat_lag_remove))
      aa[[jj]]       <- aa[[jj]] %>%
                        dplyr::select(-tidyselect::all_of(lon_lag_remove))
      aa[[jj]]       <- aa[[jj]] %>%
                        dplyr::select(-tidyselect::all_of(fcm_lag_remove))
      
      # Remove vertical lags
      if (!is.na(vc)) {
        vc_lag_remove <- switch(
          vc,
          "pressure" = all_c_names[grepl("p_lag",all_c_names)],
          "height"   = all_c_names[grepl("z_lag",all_c_names)]
        )
        aa[[jj]] <- aa[[jj]] %>%
                    dplyr::select(-tidyselect::all_of(vc_lag_remove))
      }
      
      # Lagged_model is reset before the vl loop
      if (is.null(lagged_model)) {
        lagged_model <- aa
      } else {
        lagged_model[[jj]] <- dplyr::bind_rows(lagged_model[[jj]],aa[[jj]])
      }
      
    } # vl_loop
    
    # Replace model by lagged version
    fcst[[jj]] <- lagged_model[[jj]]
    
  } # model loop
  
  return(fcst)
  
}

#================================================#
# FILTER FORECASTS TO A MAXIMUM VALUE
#================================================#

fcst_fctmax_filter <- function(fcst,
                               fcst_type,
                               prm_info,
                               vc){
  
  if (!is.null(prm_info$fctmax_val)) {
    
    nrb  <- nrow(fcst[[1]])
    
    if (fcst_type == "det") {
      
      fcst <- fcst %>% harpPoint::filter_list(
        fcst <= prm_info$fctmax_val
      )
      
    } else if (fcst_type == "eps") {
      
      # Explicit filtering over members - omitting any forecast where a 
      # member exceeds the max value.
      # Something better with across(contains("_mbr"))/if_any could be done, 
      # but is a little awkward
      
      for (cmod in names(fcst)) {
        cn  <- names(fcst[[cmod]])
        cmm <- cn[grepl("_mbr",cn,fixed = TRUE)]
        for (mc in cmm) {
          fcst[[cmod]] <- fcst[[cmod]] %>% dplyr::filter(
            get(mc) <= prm_info$fctmax_val)
        }
      }
    }
    
    # You need to run common cases again after this filtering
    fcst <- switch(
      vc,
      "pressure" = harpCore::common_cases(fcst, p),
      "height"   = harpCore::common_cases(fcst, z),
      harpCore::common_cases(fcst)
    )
    
    nra <- nrow(fcst[[1]])
    nrd <- nrb - nra
    cat("Removed",nrd,"rows with forecasts >",prm_info$fctmax_val,"\n")
    
    
  }
  
  
  return(fcst)

}

#================================================#
# READ POINT OBS WRAPPED IN TRYCATCH
#================================================#

try_rpobs <- function(fcst,
                      param,
                      param_info,
                      obs_path,
                      vc){
  
  obs <- tryCatch(
    {
      harpIO::read_point_obs(
        dttm                = harpCore::unique_valid_dttm(fcst),
        parameter           = param,
        obs_path            = obs_path,
        stations            = harpCore::unique_stations(fcst),
        min_allowed         = param_info$obsmin_val,
        max_allowed         = param_info$obsmax_val,
        vertical_coordinate = vc
      )
    },
    error = function(cond){
      cat("An error was detected during the obs read for",param,"\n")
      cat("This is probably due to a missing parameter in the OBSTABLE\n")
      cat("Here is the original message:\n")
      message(conditionMessage(cond))
      return(NULL)
    },
    finally = {
      cat("Finished obs reading\n")
    }
  )
  
  return(obs)
  
}

#================================================#
# PERFORM SOME QUALITY CHECKS ON THE FCST OBJECT
# I.E. check_obs_against_fcst AND DISCARD
# VERY INFREQUENT STATIONS
#================================================#

fcst_qc <- function(fcst,
                    param,
                    param_info,
                    vc,
                    num_days){
  
  # Check that data exists for all models
  min_rows <- nrow(fcst[[1]])
  for (ii in names(fcst)) {
    min_rows <- min(min_rows,nrow(fcst[[ii]]))
  }
  if (min_rows == 0) {
    cat("No data is available after joining to the obs for at least one model\n")
    return(NULL)
  }

  if (!is.null(param_info$error_sd)) {
    
    if (!is.na(vc)) {
    cat("error_sd =",param_info$error_sd,"for UA variable",param,
        "but check_obs_against_fcst does not work properly for UA - skipping!\n")
    } else {
    
    cat("Run check_obs_against_fcst\n")
    
    # Note: Need to be careful with lat, lon, elev, and model_elevation
    # in the fcst object, since check_obs_against_fcst does a join_model
    # based on common values of these variables! A slight difference in lat lon
    # values may be present in the vfld files (due to a different synop.list),
    # which will then cause issues in check_obs_against_fcst.
    
    if ("model_elevation" %in% names(fcst[[1]])) {
      fcst <- fcst %>% harpPoint::select_list(-model_elevation)
    }
    
    # Do a loop over all models and ensure the same lat, lon, elev
    # Note that each model is ordered in the same way after common_cases, so
    # it is safe to do this (hopefully)
    if (all(c("lat","lon","elev") %in% names(fcst[[1]]))) {
      for (ii in seq(1,length(fcst))) {
        fcst[[ii]][["lat"]]  <- fcst[[1]][["lat"]]
        fcst[[ii]][["lon"]]  <- fcst[[1]][["lon"]]
        fcst[[ii]][["elev"]] <- fcst[[1]][["elev"]]
      }
    }
    
    fcst_coaf <- tryCatch(
       {
         check_obs_against_fcst(fcst,
                                {{param}},
                                 num_sd_allowed = param_info$error_sd)
       },
       error = function(cond){
         cat("An error was detected during check_obs_against_fcst for",param,"\n")
         cat("Here is the original message:\n")
         message(conditionMessage(cond))
         return(NULL)
       }
    )
    if (!is.null(fcst_coaf)) {
      fcst <- fcst_coaf
    } else {
      cat("check_obs_against_fcst did not run for ",param,"\n")
    }
    } # vc check
  
  } else {
    cat("error_sd is not set in the parameter list, skipping check_obs_against_fcst\n")
  }
  
  # Remove stations that only occur very infrequently (surface variables only)
  if (is.na(vc)) {
    station_count    <- fcst[[1]] %>% dplyr::count(SID)
    # Using the 1% quantile of station frequency
    min_num_stations <- stats::quantile(station_count$n,
                                        probs = .01,
                                        names = FALSE)
    min_num_stations <- max(min_num_stations,num_days)
    # If the number of stations available is quite small (e.g. <10) then do not remove
    # any stations
    if (nrow(station_count) < 10) {
      min_num_stations <- 0
      cat("Only found",nrow(station_count),
          "stations in the dataset - skipping the 'infrequent' station check\n")
    }
    
    sids_filter      <- station_count$SID[station_count$n >= min_num_stations]
    sids_removed     <- station_count$SID[station_count$n < min_num_stations]
    fcst             <- harpPoint::filter_list(fcst,SID %in% sids_filter)

    if (length(sids_removed) > 0) {
      cat("Only using stations which have >=",min_num_stations,"reports\n")
      cat("This has removed the following SIDS:\n")
      print(sids_removed)
    } else {
      cat("No 'infrequent' stations removed\n")
    }
  }
  
  # Finally, call the slapdash forecast-obs difference check if included in
  # param_info
  if (!is.null(param_info$fcob_diff_sd)) {
    fcst <- fc_ob_diff_check(fcst,param,vc,num_sd = param_info$fcob_diff_sd)
  }
  
  
  return(fcst)
  
}

#================================================#
# SCORECARD GENERATION
#================================================#

gen_sc <- function(sc_data,
                   fcst,
                   param,
                   domain,
                   CONFIG,
                   fcst_type,
                   vc,
                   all_UA_vars,
                   num_ref_members){
  
  if (fcst_type == "eps") {
    verif_fn <- "ens_verify"
  } else if (fcst_type == "det") {
    verif_fn <- "det_verify"
  } else {
    stop("Why did this happen?")
  }

  if (!is.na(vc)) {
    
    # Change bootstrap grouping
    boot_groups <- c("lead_time","p")
    
    # Check if scorecard$parmaeters contains this UA var (remove pressure level)
    sc_params <- unique(gsub('[0-9]+','',CONFIG$scorecards$parameters))
    sc_params <- base::intersect(all_UA_vars,sc_params)
    sc_params <- unique(c(CONFIG$scorecards$parameters,sc_params))
    
    # If the current UA parameter is in sc_params, then filter the data to the 
    # levels requested by scorecards$parameters
    if (param %in% sc_params) {
      
      # What are the available pressure levels in the data
      pl_avail <- unique(fcst[[1]][["p"]])
      all_ppl  <- paste0(param,pl_avail)
      pl_in_sc <- base::intersect(CONFIG$scorecards$parameters,all_ppl)
      
      if (length(pl_in_sc) == 0) { 
        stop("Scorecard: No corresponding pressure level can be found in data")
      } else {
        pl_in_sc <- as.numeric(gsub(param,'',pl_in_sc))
        # Filter the data to just the pressure levels desired for the scorecard
        fcst     <- filter_list(fcst,p %in% pl_in_sc)
      }
    }
    
  } else {
    boot_groups <- "lead_time"
    sc_params   <- CONFIG$scorecards$parameters
  }
  
  # Use the filtered fcst data
  # Check if the domain and parameter are chosen by the user
  if ((domain %in% CONFIG$scorecards$domains) & (param %in% sc_params)) {
    
    # Add some attributes
    if (is.null(attributes(sc_data))) {
      attributes(sc_data)$numboot <- CONFIG$scorecards$numboot
      attributes(sc_data)$pool_by <- CONFIG$scorecards$pooled_by
    }
    
    # Use 3 hourly data for scorecards
    max_lt <- max(unique(fcst[[1]][["lead_time"]]))
    fcst_sc_tmp <- fcst %>% 
      harpPoint::filter_list(lead_time %in% seq(0,max_lt,3))
    
    if ((verif_fn == "ens_verify") & (!is.na(num_ref_members))) {
      sc_df <- harpPoint::bootstrap_verify(
        fcst_sc_tmp,
        ens_verify,
        {{param}},
        n               = CONFIG$scorecards$numboot,
        pool_by         = CONFIG$scorecards$pooled_by,
        groupings       = boot_groups,
        parallel        = CONFIG$scorecards$parallel,
        num_cores       = CONFIG$scorecards$num_cores,
        num_ref_members = num_ref_members
      )
    } else {
      sc_df <- harpPoint::bootstrap_verify(
        fcst_sc_tmp,
        get(verif_fn),
        {{param}},
        n         = CONFIG$scorecards$numboot,
        pool_by   = CONFIG$scorecards$pooled_by,
        groupings = boot_groups,
        parallel  = CONFIG$scorecards$parallel,
        num_cores = CONFIG$scorecards$num_cores
      )
    }
    rm(fcst_sc_tmp)
    
    # Add in number of cycles considered as an attribute to sc_df
    num_fc <- length(unique(fcst[[1]][["fcst_dttm"]]))
    attributes(sc_df)$num_cycles <- num_fc
    
    # Save to list
    sc_data[[domain]] <- sc_df
  }
  
  return(sc_data)
  
}

#================================================#
# EXPERIMENTAL!
# A SIMPLE WAY OF REMOVING CASES WHERE THE DIFFERENCE 
# BETWEEN FCST OBS IS GREATER THAN X STANDARD DEVIATIONS 
# FROM THE MEAN DIFFERENCE
# NOT ADVISED TO USE WIDELY, BUT MAY BE HANDY FOR UA
# WHERE check_obs_against_fcst DOES NOT WORK. USE
# A LARGE num_sd E.G. 20 TO FOCUS ON OBVIOUS ERRORS
#================================================#

fc_ob_diff_check <- function(fcst,
                             param,
                             vc,
                             num_sd = NULL) {
  
  if (!is.null(num_sd)) {
    if (num_sd > 0) {
      # Compute the difference between forecast and obs and
      # check how far each difference is away from the mean difference.
      # As this is essentially a check on the observations, using the first
      # model should be sufficient
      
      # Forecast column to use will be either "fcst" or the first member
      if ("fcst" %in% names(fcst[[1]])) {
        fc_col <- "fcst"
      } else {
        avail_mems <- names(fcst[[1]])[grepl("_mbr0",names(fcst[[1]]),fixed=T)]
        if (length(avail_mems) == 0) {
          message("Could not find a matching forecast column in fc_ob_diff_check - skipping!")
          return(fcst)
        } else {
          fc_col <- avail_mems[1]
        }
      }
      tdf <- fcst[[1]] %>% dplyr::mutate(diff = (get(fc_col) - get(param))) %>%
        dplyr::mutate(nsda = abs((diff - mean(diff))/sd(diff)))
      
      if (max(tdf$nsda) <= num_sd) {
        cat("Max fcst-obs diff is below sd threshold of",num_sd,"\n")
      } else {
        tdf_remove <- tdf %>% dplyr::filter(nsda > num_sd)
        if (nrow(tdf_remove) == nrow(fcst[[1]]) ) {
          cat("You have removed the entire dataset with fcob_diff_sd = ",num_sd,"- skipping this check!\n")
          return(fcst)
        } else {
          fcst[[1]]  <- anti_join(fcst[[1]],tdf_remove)
          # Run common_cases to remove the same obs from other models
          fcst <- switch(
            vc,
            "pressure" = harpCore::common_cases(fcst, p),
            "height"   = harpCore::common_cases(fcst, z),
            harpCore::common_cases(fcst)
          )
          cat("Removed",nrow(tdf_remove),"cases where fcst-obs diff >",num_sd,"sd from mean diff\n")
          attr(fcst,"fcob_diff_removed") <- tdf_remove
        }
      }
    }
  }
  
  return(fcst)
  
}
