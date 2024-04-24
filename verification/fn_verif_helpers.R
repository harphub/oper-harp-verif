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
})

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
      if (any(input_str[[ii]] %in% paste0(seq(0,6),"h"))) {
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
# RENAMING OF VERIFICATION GROUPS E.G. THOSE WITH
# NA OR "00;12", ETC
#================================================#

fn_verif_rename <- function(df,par_unit){
  
  for (ii in seq(1,length(df),1)) {
    for (var_rename in c("fcst_cycle","station_group","valid_hour","lead_time")) {
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
# GET LAT/LONS FROM HARP FCST OBJECT
#================================================#

get_latlon <- function(df,
                       fc,
                       ind){
  
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
      cat("Warning: Station",sid_i,
          "has no associated lat/lon value. Assigning NA\n")
      lat_v <- NA
      lon_v <- NA
    }
    
    if ((length(lat_v) > 1) || (length(lon_v) > 1)) {
      
      cat("Warning: Why multiple lat/lons for SID=",sid_i,
          "? Will use workaround here. For reference, values are:\n")
      cat("Lat:",paste0(lat_v,collapse = ","),
          "and Lon:",paste0(lon_v,collapse = ","),"\n")
      
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
                          fc){
  
  ll1         <- get_latlon(df,fc,1)
  df[[1]]$lat <- ll1$lat_sids
  df[[1]]$lon <- ll1$lon_sids
  
  # Do the same for deterministic scores in the case of an ensemble 
  if (length(df) > 2) {
    if (nrow(df[[3]]) > 0) {
      ll3         <- get_latlon(df,fc,3)
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
                         cf){
  
  # Only filter cases for lead_time, not valid_dttm or valid_hour
  verif_out <- verif_o
  verif_f   <- verif_o %>% 
    harpPoint::filter_list(dplyr::case_when(
      lead_time != "All" ~ num_cases >= cf,
      TRUE ~ num_cases >= 1)
    )
  
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
                                  fcst_models,
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
                    unique()
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
        merge_lags          = FALSE, # Explicitly do the lagging
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
  
  if (!is.null(fcst)) {
    
    # In the case of only one forecast model, convert fcst to a harp_list
    if (length(fcst_model) == 1) {
      fcst        <- harpCore::as_harp_list(placeholder = fcst)
      names(fcst) <- fcst_model
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
    
    cat("Only using stations which have >=",min_num_stations,"reports\n")
    
    sids_filter      <- station_count$SID[station_count$n >= min_num_stations]
    fcst             <- harpPoint::filter_list(fcst,SID %in% sids_filter)
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
                   verif_fn,
                   vc,
                   all_UA_vars,
                   num_ref_members){

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
