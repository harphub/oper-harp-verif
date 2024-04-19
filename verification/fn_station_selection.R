#================================================#
# DEFINE STATION IDS AND GENERATE LIST FOR DIFF
# SUB-DOMAINS. THE SCRIPT WORKS IN TWO PARTS:
#
# 1) USER-SPCIFIED SID LISTS ARE DEFINED E.G.
# THOSE FROM MONITOR, STATIONS TO ALWAYS REMOVE
#
# 2) GENERATE STATIONLISTS FROM LAT/LON BOXES.
# THIS REQUIRES AN INPUT OBSTABLE AND HARMONIE'S
# allsynop/temp.list.STATIONS ARE OMITTED IF THE 
# SID HAS MULTIPLE LAT/LON VALUES IN THE OBSTABLE,
# OR IF THERE IS A LARGE DIFF BETWEEN LAT/LON
# IN OBSTABLE VERSUS allsynop/temp.list
#================================================#

suppressPackageStartupMessages({
  library(harp)
  library(here)
  library(dplyr)
  library(tidyr)
  library(RSQLite)
})

fn_station_selection <- function(domain_choice,
                                 param = "T2m",
                                 generate_domains = FALSE,
                                 domains_to_gen = "All",
                                 plot_domains = FALSE,
                                 sl_dir = here::here("verification"),
                                 sql_file = "/path/to/OBSTABLE",
                                 png_path = here::here("verification"),
                                 domain_file = "sid_lists.rds",
                                 multlatlon_rmv = TRUE){
  
  #=================================================#
  # STATIONS TO REMOVE
  #=================================================#
  
  # Create an empty list which will hold all stations lists of interest
  all_station_lists    <- list()
  
  # Define a list of stations which are always removed from the verification
  stations_always_rmv  <- stats::setNames(tibble::as_tibble(
                           c(-1)),
                           "SID") 
  
  # Include additional filtering based on the parameter
  if (param == "S10m") {
    # Add in filtering of French stations and high-altitude stations in the UK
    stations_param_rmv <- c(3039,3065,3072,3148,3227,3410,6009,6012,
                            seq(7001,7998,1))
  } else if (param == "T2m") {
    stations_param_rmv <- c(26422,26436,26544,26429,26324,26348,26424,26416,
                            26503,26446,26335,26346,26447,26339,26318,26313,
                            26435,26403,26238,26406,26314,26229,26326)
  } else if (param == "Pmsl") {
    stations_param_rmv <- c(1304,1411)
  } else if (param == "Gmax") {
    stations_param_rmv <- c(2488,6730,11819,13461)
  } else if (param == "Td2m") {
    stations_param_rmv <- c(2325,6032,6068,6183)
  } else {
    stations_param_rmv <- NULL
  }
  stations_param_rmv <- stats::setNames(tibble::as_tibble(stations_param_rmv),
                                        "SID")
  
  # All stations to be removed for this parameter
  stations_to_rmv    <- unique(dplyr::bind_rows(stations_always_rmv,
                                                stations_param_rmv))
  
  #=================================================#
  # USER-DEFINED STATIONLISTS (AS IN MONITOR)
  # ADD SPECIFIC LISTS HERE AS REQUIRED
  #=================================================#
  
  # Denmark lists
  DKlist    <- stats::setNames(tibble::as_tibble(
               c(06030,06041,06043,06049,06052,06058,06060,06070,06072,06073,
                 06074,06079,06080,06081,06096,06102,06104,06110,06116,06118,
                 06119,06120,06123,06126,06135,06138,06141,06149,06151,06154,
                 06156,06165,06168,06170,06180,06181,06190,06193)),
               "SID")
  # Add this to all_stations_list
  all_station_lists[["DKlist"]] <- DKlist
              
  DKland    <- stats::setNames(tibble::as_tibble(
               c(06030,06049,06060,06072,06102,06104,06110,06116,06120,06126,
                 06135,06141,06156,06170)),
               "SID")
  all_station_lists[["DKland"]] <- DKland
  
  DKcoast   <- stats::setNames(tibble::as_tibble(
               c(06041,06043,06052,06058,06073,06079,06081,06096,06119,06123,
                 06126,06138,06149,06151,06165,06193)),
               "SID")
  all_station_lists[["DKcoast"]] <- DKcoast

  DKupdated <- stats::setNames(tibble::as_tibble(
               c(6019,6030,6031,6032,6041,6049,6052,6055,6056,6058,6060,6063,
                 6065,6068,6070,6072,6073,6074,6079,6080,6081,6082,6093,6096,
                 6102,6104,6108,6109,6110,6119,6120,6123,6124,6126,6135,6136,
                 6138,6141,6147,6149,6151,6154,6156,6159,6168,6169,6170,6174,
                 6180,6181,6183,6188,6030,6041,6043,6049,6052,6058,6060,6070,
                 6072,6073,6074,6079,6080,6081,6096,6102,6104,6110,6116,6118,
                 6119,6120,6123,6126,6135,6138,6141,6149,6151,6154,6156,6165,
                 6168,6170,6180,6181,6190,6193)),
               "SID")
  all_station_lists[["DKupdated"]] <- DKupdated
  
  # Greenland
  Greenland <- stats::setNames(tibble::as_tibble(
               c(4208,4214,4220,4242,4250,4253,4260,4266,4272,4285,
                 4320,4351,4373)),
               "SID")
  all_station_lists[["Greenland"]] <- Greenland

  # NorthSea (sea stations only)
  NorthSea <- stats::setNames(tibble::as_tibble(
              c(1400,6205,6239,6201,6211,6252,6204,6212,6203,6207,
                10004,10007,10015,10124,6285,6208)),
              "SID")
  all_station_lists[["NorthSea"]] <- NorthSea

  #=================================================#
  # GENERALLY NO NEED TO EDIT BEYOND HERE
  #
  # GENERATE STATIONLISTS BY READING THE SYNOP AND
  # TEMP LIST FILES AND COMPARE TO INPUT SQL FILE
  # EXTRACTING STATIONS IN LAT/LON BOXES
  # THEN SAVE THE AVAILABLE STATIONLISTS 
  # NB: TYPICALLY ONLY NEED TO GENERATE THIS ONCE,
  # THEN JUST READ THE FILE
  #=================================================#
  
  # Filename which contains all the stationlists
  fname_domains <- domain_file
  
  if (generate_domains) {
    
    #=================================================#
    # GET STATIONS IN SYNOP/TEMP FILES
    #=================================================#
    
    # Use Harmonie's synop.list and temp.list for SID, lat, and lon
    # This is due to the fact that the lat/lon for some stations are missing in
    # the vobs files (a known issue at METIE, bufr2vobs related)
    allsynop    <- file.path(sl_dir,"allsynop.list") 
    alltemp     <- file.path(sl_dir,"alltemp.list")
    # Include additional columns at the end and extract first three columns (SID, lat, lon) (V1,V2,V3)
    synop       <- read.table(allsynop,
                              header    = FALSE,
                              sep       = "",
                              fill      = TRUE,
                              col.names = paste0("V",seq_len(20))) %>%
                   dplyr::select(V1,V2,V3,V4)
    temp        <- read.table(alltemp,
                              header    = FALSE,
                              sep       = "",
                              fill      = TRUE,
                              col.names = paste0("V",seq_len(20))) %>%
                   dplyr::select(V1,V2,V3)
    # Find temp IDS which are not in synop 
    # Need to also mutate lat and lon in the temp.list
    temp_extra  <- temp[!(temp$V1 %in% synop$V1),] 
    temp_extra  <- temp_extra %>% dplyr::mutate(V2 = V2/1000, 
                                                V3 = ifelse(V3 > 180000,
                                                            ((V3/1000) - 360),
                                                            V3/1000),
                                                V4 = NA)
    allstations <- dplyr::bind_rows(synop,temp_extra)
    
    # This contains all possible stations in Harmonie vflds
    colnames(allstations) <- c("SID","lat","lon","elev")
    allstations           <- tibble::as_tibble(allstations) %>% 
                             dplyr::arrange(SID) 
    
    #=================================================#
    # GET STATIONS IN THE OBSTABLE
    #=================================================#
    
    # Now look at the stations in the current OBSTABLE 
    con        <- DBI::dbConnect(drv    = RSQLite::SQLite(),
                                 dbname = sql_file)
    sql_synop  <- DBI::dbGetQuery(conn      = con,
                                  statement = "SELECT * FROM SYNOP")
    sql_temp   <- tryCatch(
      {
        DBI::dbGetQuery(conn      = con,
                        statement = "SELECT * FROM TEMP") 
      },
        error = function(cond){
          cat("An error was detected when looking for TEMP in",sql_file,"\n")
          cat("This is probably due to a SYNOP-only OBSTABLE\n")
          cat("Here is the original message:\n")
          message(conditionMessage(cond))
          return(sql_synop)
      }
    )
    DBI::dbDisconnect(con)
    # Find temp stations which are not in synop
    sql_temp_extra <- sql_temp[!(sql_temp$SID %in% sql_synop$SID),] 
    sql_full       <- dplyr::bind_rows(sql_synop,sql_temp_extra)
    
    # Save SID, lat, lon info from SQL file
    sql_stations     <- NULL
    sql_stations$SID <- sort(unique(sql_full$SID))
    
    #=================================================#
    # LOOP OVER SQL STATIONS AND PERFORM QC CHECKS
    #=================================================#
    
    lat_vals         <- NULL
    lon_vals         <- NULL
    ind_vals         <- NULL
    
    # Tolerance for lat/lon diffs
    tol_val <- 0.2
    
    # Loop over available SIDs and do some filtering etc.
    for (sid in sql_stations$SID) {
      
      lat_v <- unique(sql_full[sql_full$SID == sid,][["lat"]])
      lon_v <- unique(sql_full[sql_full$SID == sid,][["lon"]])
      ind_v <- 0
      
      # Lat/lon values for a small number of SIDs in OBSTABLE can have different
      # lat lon values
      if ((length(lat_v) > 1) || (length(lon_v) > 1)) {
        #print(paste0("Warning: Why multiple lat/lons for SID=",sid,"? For reference, values are"))
        #print(paste0("Lat: ",paste0(lat_v,collapse = ",")," and Lon:",paste0(lon_v,collapse = ",")))
        if (length(lat_v) > 1) {
          lat_diffs <- diff(lat_v)
        } else {
          lat_diffs <- 0
        }
        if (length(lon_v) > 1) {
          lon_diffs <- diff(lon_v)
        } else {
          lon_diffs <- 0
        }
        if (max(lat_diffs) < tol_val & max(lon_diffs) < tol_val) {
          # Use workaround and keep SID 
          lat_v <- lat_v[1]
          lon_v <- lon_v[1]
        } else {
          cat("Filtering: Will remove station",sid,"from the stationlist\n")
          lat_v <- lat_v[1]
          lon_v <- lon_v[1]
          ind_v <- 1
        }
      }
      
      # Compare lat lon in OBSTABLE to the synop/temp.list values
      lat_ref <- allstations$lat[allstations$SID == sid]
      lon_ref <- allstations$lon[allstations$SID == sid]
      # Some stations in OBSTABLE have missing lat/lon
      if ((lat_v != -99) & (lon_v != -99) & 
         (length(lat_ref) > 0) & (length(lat_ref) > 0)) { 
        
        if ((abs(lat_v - lat_ref) > tol_val) || 
            (abs(lon_v - lon_ref) > tol_val)) {
          #print(paste0("Difference between lat/lon values in synop/temp.list and the OBSTABLE for SID = ",sid))
          #print(paste0("Lat: ",paste(lat_v,lat_ref,sep = ",")," and Lon:",paste(lon_v,lon_ref,sep = ",")))
          cat("Filtering: Will remove station",sid,"from the stationlist\n")
          ind_v <- 2
        }
      }
      
      lat_vals <- c(lat_vals,lat_v)
      lon_vals <- c(lon_vals,lon_v)
      ind_vals <- c(ind_vals,ind_v)
    }
    
    sql_stations$lat <- lat_vals
    sql_stations$lon <- lon_vals
    sql_stations$ind <- ind_vals
    sql_stations     <- tibble::as_tibble(sql_stations)
    
    # Now filter to common SIDs in OBSTABLE and allstations
    allstations      <- allstations %>%
                        dplyr::filter(SID %in% sql_stations$SID)

    # Remove stations if indicated 
    if (multlatlon_rmv) {
      mll_rmv_obstable <- sql_stations %>% 
                          dplyr::filter(ind > 0) %>%
                          dplyr::arrange(SID)
      mll_rmv_vfldlist <- allstations %>%
                          dplyr::filter(SID %in% mll_rmv_obstable$SID)
      allstations      <- allstations %>% 
                          dplyr::filter(!(SID %in% mll_rmv_obstable$SID))
    }
    
    # Finally, modify the elevation column for plotting purposes
    allstations$elevmap                           <- NA
    allstations$elevmap[allstations$elev < 10]    <- "1: <10m"
    allstations$elevmap[allstations$elev >= 10]   <- "2: 10-100m"
    allstations$elevmap[allstations$elev >= 100]  <- "3: 100-250m"
    allstations$elevmap[allstations$elev >= 250]  <- "4: 250-1000m"
    allstations$elevmap[allstations$elev >= 1000] <- "5: >1000m"
    
    #=================================================#
    # LOOP OVER DOMAINS AND GENERATE STATION LISTS FROM
    # LAT LON BOXES
    #=================================================#
    
    # Check if this filename already exists. If it does, then load the
    # current domainlists for comparison with the newly generated lists
    if (file.exists(here::here("verification",fname_domains))) {
      cat(fname_domains,"already exists. Saving new data to a tmp file\n")
      all_current_domains <- readRDS(file = here::here("verification",
                                                       fname_domains))
      fname_domains_save  <- "tmp_sid_lists.rds"
    } else {
      cat("Create",fname_domains,"\n")
      fname_domains_save  <- fname_domains
    }
    
    # Reserved stationlist names which are used to specify lat/lon boxes
    # (see below). Do not use these names elsewhere!
    # Note: you can use "domain=DISPLAY_ALL_STATIONS" to plot all stations 
    # in the OBSTABLE (i.e. no lat/lon filtering)
    if (length(domains_to_gen) == 1) {
      if (domains_to_gen == "All") {
        domains_to_gen <- c("DINI","IE_EN","IS","NL","NL_OP","DK","IRL","SCD",
                            "FR","NNS","DE","Alps")
      }
      cat("Generating all reserved domains\n")
    } else {
      cat("Generating domains",paste0(domains_to_gen,collapse = ","),"\n")
    }
    
    # Object to hold all generated stationlists
    all_gen_domains <- NULL
    
    for (domain in domains_to_gen) {
      
      if (domain == "IE_EN") {  #Ireland+UK bounding box
        slat <- 50.0
        wlon <- -11.0
        nlat <- 60.0
        elon <- 2.0
      } else if (domain == "IS") { #Iceland bounding box
        slat <- 62.0
        wlon <- -25.0
        nlat <- 67.0
        elon <- -12.0 
      } else if (domain == "NL") { #Netherlands bounding box
        slat <- 51.0
        wlon <- 1.5
        nlat <- 54.5
        elon <- 9.0
      } else if (domain == "NL_OP") { #Netherlands bounding box used at KNMI
        slat <- 49.0
        wlon <- 0.0
        nlat <- 55.0
        elon <- 11.0
      } else if (domain == "DK") {
        slat <- 54.0
        wlon <- 8.0
        nlat <- 58.0
        elon <- 13.0
      #} else if (domain == "DK_EXT"){
      #  slat <- 53.0
      #  wlon <- 1.0
      #  nlat <- 60.0
      #  elon <- 14.0
      } else if (domain == "DINI") { 
        # Place some additional DINI bounds 
        # These are just dummy arugments 
        slat <- 0 
        nlat <- 90
        wlon <- -50
        elon <- 50
      } else if (domain == "IRL") {
        slat <- 51
        wlon <- -11
        nlat <- 55.5
        elon <- -6
      } else if (domain == "SCD") { # Scandinavia bounding box
        slat <- 55
        wlon <- 4.5 
        nlat <- 70
        elon <- 32
      } else if (domain == "FR") {
        slat <- 44
        wlon <- -5
        nlat <- 50
        elon <- 8
      } else if (domain == "NNS") {
        slat <- 55
        wlon <- -1.5
        nlat <- 61
        elon <- 9
      } else if (domain == "DE") {
        slat <- 47.27
        wlon <- 5.86
        nlat <- 55.10
        elon <- 15.04
      } else if (domain == "Alps") {
        slat <- 43.5
        wlon <- 6.1
        nlat <- 48.2
        elon <- 17.3
      }
      
      if (domain != "DISPLAY_ALL_STATIONS") {
        df <- filter_stations(allstations,domain,slat,nlat,wlon,elon)
      } else {
        df <- allstations
      }
      
      # Check if this domain already exists. If so, only add new SIDS
      if (file.exists(here::here("verification",fname_domains))) {
        cdf <- all_current_domains[[domain]]
        if (!is.null(cdf)) {
          new_df <- df %>% dplyr::filter(!(SID %in% unique(cdf$SID)))
          df     <- dplyr::bind_rows(cdf,new_df)
        }
      }
      
      # Plot the maps if desired
      if (plot_domains) {
        p1        <- station_map(df,domain)
        png_fname <- paste0("sample-",domain,"-stationmap.png")
        ggplot2::ggsave(p1,
                        filename = png_fname,
                        path     = png_path,
                        width    = 10,
                        height   = 4.5,
                        units    = "in",
                        dpi      = 200)
      }
      
      # Save to generated domain list
      all_gen_domains[[domain]] <- df
      
    } # End of domain loop
    
    #=================================================#
    # OUTPUT SID DATA
    #=================================================#
    
    # If the domains file already exists, output the original domains that 
    # have not been generated here
    if (file.exists(here::here("verification",fname_domains))) {
      orig_minus_domains <- all_current_domains[!(names(all_current_domains) %in%
                                                  domains_to_gen)]
      all_gen_domains    <- c(orig_minus_domains,all_gen_domains)
      
    }
    
    # Add in some additional lists for reference
    if (multlatlon_rmv) {
      if (file.exists(here::here("verification",fname_domains))) {
        new_mll_ob <- mll_rmv_obstable %>% 
          dplyr::filter(!(SID %in% all_current_domains[["mll_rmv_obstable"]]$SID))
        mll_rmv_obstable <- dplyr::bind_rows(all_current_domains[["mll_rmv_obstable"]],
                                             new_mll_ob)
        new_mll_vf <- mll_rmv_vfldlist %>%
          dplyr::filter(!(SID %in% all_current_domains[["mll_rmv_vfldlist"]]$SID))
        mll_rmv_vfldlist <- dplyr::bind_rows(all_current_domains[["mll_rmv_vfldlist"]],
                                             new_mll_vf)
      }
      all_gen_domains[["mll_rmv_obstable"]] <- mll_rmv_obstable
      all_gen_domains[["mll_rmv_vfldlist"]] <- mll_rmv_vfldlist
    }
    
    # Save the output list
    saveRDS(all_gen_domains,file = here::here("verification",fname_domains_save))
    
  } else {
    
    # Read stationlist from an existing file
    cat("Read existing SID lists in",fname_domains,"\n")
    all_gen_domains <- readRDS(file = here::here("verification",fname_domains))
    
  }
  
  # Now pick out the relevant stationlist for the chosen domains and filter
  if (any(names(all_station_lists) %in% names(all_gen_domains))) {
    
    warning("A user-specified stationlist name already exists in 
            the pre-defined domain-generated stationlists")
    stop("Aborting due to fn_station_selection error!")
    
  } else {
    all_station_lists  <- c(all_station_lists,all_gen_domains)
    stationlist_out    <- NULL
    if (all(domain_choice == "all_domains")) {
      domain_choice <- names(all_station_lists)
    }
    for (dc in domain_choice) {
      if (dc %in% names(all_station_lists)) {
        stationlist_choice <- all_station_lists[[dc]] 
        # Filter
        if (!is.null(stationlist_choice)) {
          stationlist_choice <- stationlist_choice %>%
                                dplyr::filter(!(SID %in% stations_to_rmv$SID))
        }
        stationlist_out[[dc]] <- stationlist_choice
        
        if (plot_domains) {
          if ("elevmap" %in% names(stationlist_choice)) {
            p1        <- station_map(stationlist_choice,dc)
            png_fname <- paste0("sample-",dc,"-stationmap.png")
            ggplot2::ggsave(p1,
                            filename = png_fname,
                            path     = png_path,
                            width    = 10,
                            height   = 4.5,
                            units    = "in",
                            dpi      = 200)
          }
        }
      } else {
        warning("Domain ",dc," not recognised in station selection!\n")
      }
    }
    # Add in the stations_to_rmv (used when looking at "All" stations)
    stationlist_out[["stations_to_rmv"]] <- stations_to_rmv
  }

  return(stationlist_out)
}

#=================================================#
# LAT LON FILTER
#=================================================#
filter_stations <- function(df,
                            domain,
                            slat,
                            nlat,
                            wlon,
                            elon){
  
  df_out <- df %>% 
    dplyr::filter(between(lat,slat,nlat)) %>%
    dplyr::filter(between(lon,wlon,elon))
  
  return(df_out)
}

#=================================================#
# FOR PLOTTING THE DOMAINS
#=================================================#
station_map <- function(df,domain){
  
  min_lon      <- min(df[["lon"]])
  max_lon      <- max(df[["lon"]])
  min_lat      <- min(df[["lat"]])
  max_lat      <- max(df[["lat"]])
  num_stations <- length(df$SID)
  
  # Take out stations which have NA in their elevation
  df    <- tidyr::drop_na(df)
  p_map <- df %>% ggplot2::ggplot(aes(lon,lat,fill = elevmap),size = 3)
  p_map <- p_map + 
    ggplot2::geom_polygon(data        = ggplot2::map_data("world"),
                          mapping     = aes(long, lat, group = group),
                          fill        = "grey100",
                          colour      = "black",
                          inherit.aes = FALSE) +  
    ggplot2::geom_point(colour = 'grey40',pch = 21) + 
    ggplot2::coord_map(projection = "lambert",
                       lat0       = 63,
                       lat1       = 63,
                       xlim       = c(min_lon,max_lon),
                       ylim       = c(min_lat,max_lat)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey95"),
                   panel.grid       = ggplot2::element_blank(),
                   axis.text        = ggplot2::element_blank(),
                   axis.ticks       = ggplot2::element_blank(),
                   axis.title       = ggplot2::element_blank(),
                   plot.title       = ggplot2::element_text(size = 14),
                   legend.text      = ggplot2::element_text(size = 10),
                   plot.subtitle    = ggplot2::element_text(size = 12),
                   legend.title     = ggplot2::element_text(size = 12),
                   legend.position  = "right", 
                   strip.background = ggplot2::element_rect(fill = "white"),
                   strip.text       = ggplot2::element_text(size = 14)) +
    ggplot2::scale_fill_brewer("Elevation",palette = "YlOrBr") +
    ggplot2::labs(title = paste0(domain,": ",num_stations," stations")) +
    ggplot2::guides(size = "none") # Remove size label from legend
  
  return(p_map)
  
}
