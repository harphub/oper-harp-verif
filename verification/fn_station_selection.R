#================================================#
# DEFINE STATION IDS AND GENERATE LIST FOR DIFF
# SUB-DOMAINS. THE SCRIPT WORKS IN TWO PARTS:
#
# 1) USER-SPCIFIED SID LISTS ARE DEFINED E.G.
# THOSE FROM MONITOR, STATIONS TO ALWAYS REMOVE
#
# 2) GENERATE STATIONLISTS FROM LAT/LON BOXES, 
# SYNOP RANGES, OR POLYGON FILES.
#
# GENERATION OF STATIONLISTS CAN BE DONE IN TWO WAYS:
# 
# 1) STATIC METHOD (NO LONGER RECOMMENDED): THIS 
# REQUIRES AN INPUT OBSTABLE AND HARMONIE'S
# allsynop/temp.list. LISTS OF SIDS ARE SAVED TO
# A STATIC FILE (e.g. sid_lists.rds) WHICH NEEDS TO 
# BE MANUALLY UPDATED.
#
# 2) DYNAMIC METHOD (NEW): GENERATE LISTS FROM THE
# AVAILABLE FORECAST/OBS DATA DURING VERIFICATION.
#
# SEVERAL QC CHECKS CAN BE CARRIED OUT FOR EACH METHOD:
# 1) STATIONS ARE OMITTED IF THE SID HAS MULTIPLE LAT/LON
# VALUES IN THE OBSTABLE, OR
# 2) IF THERE IS A LARGE DIFF BETWEEN LAT/LON
# IN OBSTABLE VERSUS allsynop/temp.list (OR FCTABLE)
#================================================#

suppressPackageStartupMessages({
  library(harp)
  library(here)
  library(dplyr)
  library(tidyr)
  library(RSQLite)
})

if (!exists("sf_available")) {
  if ("sf" %in% rownames(installed.packages())) {
    library(sf)
    sf_available <- TRUE
  } else {
    warning("sf package not found - filtering via poly files will not work and will be skipped!\n")
    sf_available <- FALSE
  }
} else {
  cat("sf_available inherited\n")
}

fn_station_selection <- function(domain_choice = "All_Domains",
                                 param = "T2m",
                                 generate_domains = FALSE,
                                 domains_to_gen = "All_Domains",
                                 plot_domains = FALSE,
                                 fcst_object = NULL,
                                 obs_object  = NULL,
                                 sl_dir = here::here("verification"),
                                 poly_dir = here::here("verification/poly_files"),
                                 sql_file = "/path/to/OBSTABLE",
                                 png_path = here::here("verification"),
                                 domain_file = "sid_lists.rds",
                                 multlatlon_rmv = TRUE,
                                 return_defined_lists = FALSE){
  
  #=================================================#
  # OPTION CHECKS
  #=================================================#
  
  if (generate_domains) {
    if ((!is.null(fcst_object)) || (!is.null(obs_object))){
      stop("Use generate_domains to create static SID lists!")
    } else {
      cat("Assuming static generation!\n")
    }
  }
  
  dynamic_gen <- FALSE
  if ((!is.null(fcst_object)) || (!is.null(obs_object))){
    if ((!is.null(fcst_object)) & (!is.null(obs_object))){
      cat("Dynamically generating SID lists during the verification\n")
      if (length(domains_to_gen) == 1) {
        if (domains_to_gen == "All_Domains") {
          stop("Why generate all domains during verification?")
        }
      } 
      if (generate_domains){
        stop("Cannot run dynamic and static!\n")
      }
    } else {
      stop("You need to supply the obs and fcst object for dynamic generation!")
    }
    dynamic_gen <- TRUE
  }
  
  #=================================================#
  # STATIONS TO REMOVE
  #=================================================#
  
  # Create an empty list which will hold all stations lists of interest
  all_station_lists    <- list()
  
  # Define a list of stations which are always removed from the verification
  stations_always_rmv  <- stats::setNames(tibble::as_tibble(
                           c(-1,9997485)),
                           "SID") 
  
  # Include additional filtering based on the parameter
  if (param == "S10m") {
    # Add in filtering of French stations, poor-quality Irish and high-altitude UK stations
    stations_param_rmv <- c(3039,3065,3072,3148,3227,3410,
                            3971,3979,
                            6009,6012,
                            seq(7001,7998,1))
  } else if (param == "T2m") {
    stations_param_rmv <- c(26422,26436,26544,26429,26324,26348,26424,26416,
                            26503,26446,26335,26346,26447,26339,26318,26313,
                            26435,26403,26238,26406,26314,26229,26326)
  } else if (param == "Pmsl") {
    stations_param_rmv <- c(1304,1411,10004,4160,4166)
  } else if (param == "Gmax") {
    stations_param_rmv <- c(2488,6730,11819,13461)
  } else if (param == "Td2m") {
    stations_param_rmv <- c(2325,6032,6068,6183,4976)
  } else if (param == "RH2m") {
    stations_param_rmv <- c(4976,9991985,9996085)
  } else {
    stations_param_rmv <- NULL
  }
  stations_param_rmv <- stats::setNames(tibble::as_tibble(stations_param_rmv),
                                        "SID")
  
  # All stations to be removed for this parameter
  stations_to_rmv    <- unique(dplyr::bind_rows(stations_always_rmv,
                                                stations_param_rmv))
  
  #=================================================#
  # RESERVED STATIONLIST NAMES WHICH ARE USED TO 
  # SPECIFY LAT/LON BOXES, WMO SID RANGES, OR POLY FILES
  # DO NOT USE THESE NAMES ELSEWHERE!
  #=================================================#
  
  # Note the vector concatenated with "Synop" (i.e. synop_reserved_names)
  # is used to generate lists based on SID ranges. 
  # latlon_reserved_names are used to generate lists defined by lat/lon
  # bounding boxes, while poly_reserved_names looks for .poly files
  latlon_reserved_names <- c("DINI","IE_EN","IS","NL","NL_OP","DK","IRL","SCD",
                             "FR","DE","Alps")
  synop_reserved_names  <- paste0(c("France","Germany","Ireland","Norway",
                                    "Sweden","Finland","Iceland","Greenland",
                                    "Denmark","Netherlands","Spain",
                                    "Switzerland","UnitedKingdom"),
                                 "Synop")
  poly_reserved_names   <- c("Baltex","Baltic_sea","Gotaland","Latvia_Lithuania",
                             "Norrland","Svealand","Sweden")
  
  all_reserved_names <- c(latlon_reserved_names,
                          synop_reserved_names,
                          poly_reserved_names)
  
  if (max(table(all_reserved_names)) != 1) {
    stop("Repeated name in all_reserved_names!\n")
  }
  
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
              c(1400,6022,6205,6239,6201,6211,6252,6204,6212,6203,6207,
                10004,10007,10015,10124,6285,6208)),
              "SID")
  all_station_lists[["NorthSea"]] <- NorthSea
  
  # New NNS definition (NorthSea plus others)
  NNS <- stats::setNames(tibble::as_tibble(
    c(NorthSea$SID,1400,3002,3005,3008)),
    "SID")
  all_station_lists[["NNS"]] <- NNS

  # Add individual Irish airports
  EICK <- stats::setNames(tibble::as_tibble(c(3955)),"SID")
  EIDW <- stats::setNames(tibble::as_tibble(c(3969)),"SID")
  EINN <- stats::setNames(tibble::as_tibble(c(3962)),"SID")
  EIME <- stats::setNames(tibble::as_tibble(c(3967)),"SID")
  EIKN <- stats::setNames(tibble::as_tibble(c(3973)),"SID")
  all_station_lists[["EICK"]] <- EICK
  all_station_lists[["EIDW"]] <- EIDW
  all_station_lists[["EINN"]] <- EINN
  all_station_lists[["EIME"]] <- EIME
  all_station_lists[["EIKN"]] <- EIKN
  
  # Irish M buoys
  MBuoys <- stats::setNames(tibble::as_tibble(
    c(6200091,6200092,6200093,6200094,6200095)),
    "SID")
  all_station_lists[["MBuoys"]] <- MBuoys
  M2Buoy <- stats::setNames(tibble::as_tibble(c(6200091)),"SID")
  M3Buoy <- stats::setNames(tibble::as_tibble(c(6200092)),"SID")
  M4Buoy <- stats::setNames(tibble::as_tibble(c(6200093)),"SID")
  M5Buoy <- stats::setNames(tibble::as_tibble(c(6200094)),"SID")
  M6Buoy <- stats::setNames(tibble::as_tibble(c(6200095)),"SID")
  all_station_lists[["M2Buoy"]] <- M2Buoy
  all_station_lists[["M3Buoy"]] <- M3Buoy
  all_station_lists[["M4Buoy"]] <- M4Buoy
  all_station_lists[["M5Buoy"]] <- M5Buoy
  all_station_lists[["M6Buoy"]] <- M6Buoy

  # Estonia
  EstoniaSynop <- stats::setNames(tibble::as_tibble(
    c(26029,26038,26045,26046,26058,26115,26120,26124,26128,26134,26135,
      26141,26144,26145,26214,26215,26218,26226,26227,26231,26233,26242,
      26247,26249)),
    "SID")
  all_station_lists[["EstoniaSynop"]] <- EstoniaSynop
  
  # Latvia
  LatviaSynop <- stats::setNames(tibble::as_tibble(
    c(26229,26238,26313,26314,26318,26324,26326,26335,26339,26346,26348,
      26403,26406,26416,26422,26424,26425,26429,26435,26436,26446,26447,
      26503,26544)),
    "SID")
  all_station_lists[["LatviaSynop"]] <- LatviaSynop
  
  # Lithuania
  LithuaniaSynop <- stats::setNames(tibble::as_tibble(
    c(26502,26509,26515,26518,26524,26529,26531,26547,26600,26603,26620,
      26621,26629,26633,26634,26713,26728,26730,26732,26737)),
    "SID")
  all_station_lists[["LithuaniaSynop"]] <- LithuaniaSynop

  # GreenlandIcecap
  GreenlandIcecap <- stats::setNames(tibble::as_tibble(
                 c(4411,4412,4426,4427,4433,4434,4445,4446,4470,4471,4472,4480,
                4481,4490,4420,4422,4423,4425,4430,4431,4432,4436,4437,4443,
                4452,4453,4454,4455,4458,4464,4466,4467,4468,4485,4488,4489,
                4492,4204220,4204221,4204205,4204210,4204212,4204213,4204214,
                4204215, 4204216, 4204219, 4204228)
                 ),
               "SID")
  all_station_lists[["GreenlandIcecap"]] <- GreenlandIcecap

  #NEUcoast
  NEUcoast <- stats::setNames(tibble::as_tibble(
                c(01001, 01002, 01003, 01004, 01006, 01007, 01008, 01009, 01010, 01015,
      01016, 01017, 01019, 01020, 01025, 01026, 01028, 01033, 01043, 01045,
      01047, 01049, 01052, 01055, 01062, 01075, 01078, 01092, 01098, 01102,
      01105, 01107, 01108, 01114, 01115, 01121, 01132, 01143, 01144, 01147,
      01152, 01156, 01158, 01160, 01167, 01203, 01205, 01207, 01210, 01212,
      01214, 01224, 01225, 01228, 01240, 01241, 01241, 01242, 01262, 01287,
      01298, 01300, 01304, 01307, 01311, 01317, 01318, 01329, 01338, 01378,
      01385, 01386, 01400, 01403, 01406, 01411, 01412, 01416, 01417, 01422,
      01424, 01426, 01427, 01430, 01436, 01448, 01450, 01459, 01465, 01467,
      01470, 01476, 01478, 01482, 01483, 01486, 01493, 01495 ,02013 ,02019,
      02020 ,02024 ,02029 ,02119 ,02130 ,02176 ,02186 ,02188 ,02191 ,02196,
      02197 ,02217 ,02219 ,02226 ,02230 ,02263 ,02269 ,02284 ,02286 ,02287,
      02293 ,02292 ,02297 ,02308 ,02331 ,02347 ,02354 ,02355 ,02365 ,02366,
      02368 ,02378 ,02382 ,02403 ,02408 ,02417 ,02441 ,02450 ,02464 ,02482,
      02486 ,02487 ,02488 ,02489 ,02493 ,02496 ,02498 ,02500 ,02501 ,02505,
      02516 ,02518 ,02520 ,02544 ,02545 ,02548 ,02558 ,02563 ,02567 ,02575,
      02587 ,02589 ,02598 ,02605 ,02607 ,02609 ,02611 ,02615 ,02616 ,02625,
      02628 ,02632 ,02644 ,02646 ,02667 ,02670 ,02679 ,02680 ,02701 ,02704,
      02711 ,02719 ,02721 ,02723 ,02725 ,02727 ,02728 ,02730 ,02731 ,02732,
      02734 ,02736 ,02739 ,02741 ,02743 ,02746 ,02747 ,02750 ,02751 ,02752,
      02757 ,02761 ,02763 ,02765 ,02768 ,02772 ,02778 ,02780 ,02781 ,02788,
      02790 ,02793 ,02794 ,02795 ,02800 ,02801 ,02808 ,02816 ,02828 ,02831,
      02836 ,02862 ,02863 ,02872 ,02873 ,02875 ,02876 ,02889 ,02897 ,02907,
      02910 ,02913 ,02915 ,02917 ,02920 ,02939 ,02943 ,02944 ,02950 ,02964,
      02967 ,02976 ,02978 ,02979 ,02980 ,02981 ,02982 ,02984 ,02987 ,02988,
      02991 ,02992 ,02993 ,02996 ,04001 ,04003 ,04005 ,04007 ,04010 ,04013,
      04014 ,04017 ,04018 ,04025 ,04027 ,04030 ,04030 ,04034 ,04038 ,04044,
      04045 ,04048 ,04051 ,04053 ,04058 ,04059 ,04062 ,04064 ,04065 ,04071,
      04072 ,04077 ,04081 ,04082 ,04087 ,04091 ,04094 ,04097 ,04101 ,04103,
      04104 ,04109 ,04115 ,04119 ,04120 ,04124 ,04126 ,04130 ,04131 ,04140,
      04161 ,04165 ,04170 ,04180 ,04183 ,04196 ,04197 ,04208 ,04211 ,04213,
      04214 ,04217 ,04219 ,04220 ,04221 ,04224 ,04228 ,04234 ,04241 ,04242,
      04250 ,04253 ,04254 ,04260 ,04266 ,04272 ,04273 ,04280 ,04282 ,04283,
      04285 ,04301 ,04312 ,04313 ,04320 ,04330 ,04339 ,04341 ,04351 ,04360,
      04361 ,04373 ,04382 ,04390 ,06021 ,06022 ,06033 ,06041 ,06042 ,06051,
      06052 ,06055 ,06058 ,06063 ,06073 ,06079 ,06081 ,06088 ,06093 ,06096,
      06099 ,06119 ,06123 ,06124 ,06132 ,06138 ,06147 ,06149 ,06151 ,06159,
      06165 ,06168 ,06169 ,06180 ,06183 ,06190 ,06193 ,22028 ,22165 ,22212,
      22217 ,22271 ,22282 ,22312 ,22349 ,22365 ,22422 ,22438 ,22446 ,22452,
      22471 ,22518 ,22520 ,22522 ,22525 ,22529 ,22541 ,22550 ,22550 ,22551,
      22559 ,22602 ,22619 ,22621 ,22641 ,22727 ,22802 ,22892 ,22917 ,22798,
      22981 ,22996 ,26029 ,26058 ,26063 ,26115 ,26120 ,26128 ,26157 ,26214,
      26215 ,26218 ,26226 ,26227 ,26229 ,26313 ,26314 ,26324 ,26403 ,26406,
      26509 ,26603
                )
                ),
               "SID")
  all_station_lists[["NEUcoast"]] <- NEUcoast

  #EWGLAM
  EWGLAM <- stats::setNames(tibble::as_tibble(
      c(01001,01025,01049,01098,01102,01152,01205,01271,01317,01384,01415,01452,01494,
  02020,02096,02120,02186,02222,02226,02286,02316,02366,02376,02418,02444,02464,
  02512,02562,02590,02640,02807,02836,02864,02897,02911,02917,02929,02935,02944,
  02952,02958,02970,02972,02974,03005,03026,03066,03075,03091,03100,03140,03162,
  03171,03240,03257,03302,03222,03360,03496,03502,03534,03566,03715,03772,03797,
  03808,03862,03895,03917,03953,03957,03962,03969,03970,03976,03980,06011,06030,
  06052,06110,06180,06235,06260,06290,06310,06400,06447,06476,06479,06610,06720,
  07005,07015,07027,07037,07070,07110,07130,07157,07168,07180,07190,07222,07240,
  07255,07280,07292,07315,07434,07460,07481,07510,07535,07577,07602,07630,07645,
  07650,07690,07747,07761,07790,08001,08015,08023,08027,08045,08084,08160,08181,
  08202,08261,08280,08284,08306,08314,08348,08360,08373,08391,08482,08487,08495,
  08538,08549,08562,08571,08575,08579,10035,10063,10091,10113,10131,10147,10162,
  10224,10280,10315,10338,10361,10384,10400,10439,10469,10488,10490,10510,10548,
  10637,10655,10685,10708,10729,10738,10763,10776,10803,10870,10893,11010,11035,
  11120,11150,11240,11448,11518,11541,11659,11723,11782,11858,11903,11934,11968,
  12100,12115,12135,12195,12205,12250,12295,12300,12330,12375,12400,12424,12465,
  12495,12500,12520,12550,12566,12570,12580,12772,12812,12843,12860,12882,12925,
  12935,12960,12982,15010,15020,15090,15120,15197,15200,15230,15235,15310,15346,
  15360,15410,15420,14450,15470,15480,15511,15526,15552,15614,15625,15640,15655,
  15712,16020,16045,16059,16080,16090,16105,16120,16149,16170,16181,16230,16289,
  16300,16320,16360,16405,16420,16453,16470,16490,16506,16520,16560,16597,16622,
  16627,16641,16648,16650,16667,16682,16684,16716,16723,16732,16743,16749,16754,
  17022,17050,17060,17112,17116,17124,17150,17184,17188,17240,17292,17300,22106,
  22408,22602,22802,22820,22892,26038,26063,26215,26258,26406,26422,26477,26524,
  26544,26629,26702,26730,26850,26863,33008,33036,33041,33088,33177,33317,33345,
  33393,33526,33562,33587,33631,33658,33663,33777,33815,33837)
                ),
               "SID")
  all_station_lists[["EWGLAM"]] <- EWGLAM
  #RussiaCoast
  RussiaCoast <- stats::setNames(tibble::as_tibble(
           c(20046,20069,20087,20292,20471,20476,20667,20674,20744,
             20871,20946,20967,21432,21535,21608,21636,21711,21821,
             21824,21978,21982,22003,22028,22113,22165,22193,22217,
             22271,22292,22312,22324,22334,22349,22365,22429,22438,
             22446,22471,22520,22525,22529,22541,22546,22550,22551,
             22641,23022,23032,23058,23112,23114,23242,23256,23345,
             25034,25042,25044,25051,25062,25151,25282,25399)
              ),
               "SID")
  all_station_lists[["RussiaCoast"]] <- RussiaCoast

  #RussiaInland
  RussiaInland <- stats::setNames(tibble::as_tibble(
              c(20891,20978,20982,21721,21802,21813,21908,21921,21931,
                21937,21946,22004,22101,22106,22127,22212,22213,22235,
                22249,22383,22408,22413,22456,22481,22559,22563,22573,
                22583,22602,22619,22621,22648,22656,22671,22676,22686,
                22695,22717,22721,22727,22749,22762,22768,22778,22798,
                23073,23078,23174,23179,23205,23226,23274,23305,23324,
                23330,23339,23358,23376,23383,23405,23412,23418,23426,
                23431,23443,23445,23453,23465,23472,23503,23518,23527,
                23552,23578,23589,23606,23625,23631,23635,23656,23657,
                23662,23678,23699,23701,23708,23711,23724,23734,23741,
                23748,23758,23774,23776,23788,24076,24125,24136,24143,
                24194,24219,24261,24263,24266,24322,24329,24338,24343,
                24371,24382,24449,24462,24477,24507,24525,24538,24606,
                24639,24724,24725,24726,24737,24738,25017,25121,25123,
                25129,25138,25147,25206,25248,25335,25356,25400,25428,
                25469)
              ),
               "SID")
  all_station_lists[["RussiaInland"]] <- RussiaInland

  #NorthAmericaInland
  NorthAmericaInland <- stats::setNames(tibble::as_tibble(
              c(70178,70197,70200,70207,70212,70235,70270,70272,70273,
                70274,70275,71976,71092,71926,71356,71923,71490,71963,
                71076,71160,71529,71470,71362,71934,71162,71936,71680,
                71935,71361,71087,71086,71163,71165,71199,71990,71980,
                71021,71020,71682,71497,71166,71492,71583,71503,71055,
                71491,71043,71480,71164,71683,71506,71044,71977,71978,
                71364,71957,71199,71990,71949,71045,71505,71001,71966,
                71039,71964,71773,71365,71946,71365,71946)
              ),
               "SID")
  all_station_lists[["NorthAmericaInland"]] <- NorthAmericaInland
  
  # Define a list of all coastal stations for the DINI+IG domain
  Coastal_DINI <- stats::setNames(tibble::as_tibble(
    c(1001,1002,1003,1004,1006,1007,1008,1009,1011,1016,1017,1019,1020,
      1062,1102,1105,1106,1107,1108,1112,1114,1115,1116,1117,1121,1139,
      1141,1143,1144,1152,1153,1156,1160,1161,1200,1201,1203,1205,1210,
      1212,1214,1217,1223,1224,1225,1228,1237,1240,1241,1259,1262,1282,
      1300,1302,1304,1307,1308,1309,1310,1319,1399,1400,1401,1402,1403,
      1404,1406,1407,1408,1411,1412,1414,1415,1416,1418,1426,1427,1428,
      1430,1436,1448,1460,1464,1465,1467,1476,1478,1482,1495,1503,1508,
      2091,2176,2183,2185,2186,2188,2191,2197,2269,2284,2287,2288,2297,
      2355,2368,2378,2382,2450,2487,2488,2489,2492,2493,2496,2498,2499,
      2500,2501,2503,2505,2516,2517,2518,2563,2567,2573,2575,2583,2584,
      2587,2589,2590,2591,2598,2605,2611,2615,2616,2625,2628,2632,2644,
      2646,2665,2667,2670,2679,2680,2721,2724,2743,2746,2750,2751,2752,
      2757,2761,2780,2781,2790,2794,2795,2800,2831,2851,2852,2862,2863,
      2872,2873,2874,2875,2876,2901,2907,2910,2920,2921,2931,2934,2943,
      2950,2961,2964,2967,2968,2970,2971,2976,2978,2979,2980,2981,2982,
      2984,2987,2988,2989,2991,2992,2993,2996,2998,3002,3005,3006,3007,
      3008,3010,3011,3013,3014,3017,3019,3023,3026,3027,3034,3035,3037,
      3040,3055,3062,3066,3068,3075,3077,3088,3092,3094,3100,3105,3111,
      3120,3132,3136,3138,3153,3171,3174,3204,3210,3214,3240,3275,3292,
      3302,3316,3318,3385,3392,3402,3405,3407,3469,3488,3502,3604,3605,
      3609,3693,3696,3701,3716,3717,3791,3796,3797,3803,3807,3808,3809,
      3810,3827,3837,3857,3866,3872,3874,3876,3894,3895,3896,3907,3914,
      3916,3927,3928,3951,3952,3953,3963,3976,3978,3980,4005,4013,
      4017,4018,4030,4031,4038,4045,4048,4051,4058,4059,4071,4072,4077,
      4082,4083,4097,4101,4102,4103,4104,4106,4107,4108,4110,4111,4116,
      4118,4120,4121,4122,4123,4124,4125,4128,4129,4130,4131,4144,4146,
      4151,4155,4157,4159,4164,4165,4170,4171,4175,4183,4184,4187,4188,
      4194,4196,4197,4198,4199,4202,4203,4205,4208,4211,4213,4214,4217,
      4219,4220,4221,4224,4228,4234,4241,4242,4250,4253,4254,4260,4266,
      4272,4273,4282,4283,4285,4301,4310,4312,4313,4320,4330,4339,4341,
      4351,4360,4361,4373,4382,4390,4446,4490,4491,4802,4808,4809,4812,
      4813,4818,4822,4824,4832,4833,4837,4838,4843,4848,4857,4868,4870,
      4880,4883,4884,4886,4892,4895,4899,4908,4911,4913,4923,4926,4927,
      4928,4930,4932,4934,4938,4949,4951,4953,4955,4956,4957,4959,4963,
      4965,4970,4983,4986,4990,4992,4994,6005,6008,6009,6010,6011,6012,
      6013,6016,6017,6021,6022,6024,6033,6041,6042,6043,6044,6051,6052,
      6055,6058,6073,6079,6081,6088,6093,6096,6099,6118,6119,6123,6124,
      6132,6136,6138,6143,6147,6149,6151,6158,6159,6165,6168,6169,6179,
      6180,6181,6183,6184,6186,6187,6190,6193,6197,6201,6203,6204,6205,
      6206,6207,6208,6209,6211,6212,6225,6229,6235,6239,6242,6248,6251,
      6252,6253,6254,6255,6257,6258,6267,6277,6285,6308,6310,6311,6312,
      6313,6315,6316,6320,6321,6323,6324,6330,6331,6400,6407,6408,6418,
      7002,7003,7010,7020,7024,7028,7031,7040,7046,7100,7103,7107,7117,
      7125,7200,7203,7205,7207,7217,7300,7314,7315,7316,7500,7503,7600,
      7602,7641,7643,7661,7666,7667,7670,7684,7690,7749,7753,7754,7761,
      7765,7770,7774,7775,7785,7790,8001,8011,8014,8021,8023,8027,8029,
      8046,8081,8181,8543,8551,10004,10007,10015,10018,10020,10028,10042,
      10043,10044,10055,10067,10089,10091,10093,10097,10113,10124,10127,
      10130,10131,10152,10161,10169,10170,10184,10192,10204,12001,12100,
      12115,12120,12135,12140,12145,12155,12200,13455,13457,13461,13464,
      14105,14216,14307,14314,14317,14321,14323,14428,14438,14441,14443,
      14444,14445,14447,14452,14454,14462,14472,14474,16101,16105,16110,
      16120,16121,16123,16125,16127,16129,16141,16145,16146,16149,16150,
      16153,16154,16155,16168,16190,16191,16197,16198,16210,16214,16215,
      16232,16242,16245,16249,16266,16270,16271,16280,16281,16294,16295,
      16310,16320,16325,16334,16336,16337,16342,16350,16351,16360,16362,
      16415,16416,16420,16504,16522,16523,16550,20107,22897,26029,26045,
      26058,26115,26116,26120,26128,26214,26215,26218,26226,26227,26229,
      26313,26314,26324,26326,26403,26406,26502,26509,26603,71074,71082,
      71090,71093,71094,71095,71096,71338,71355,71357,71358,71576,71872,
      71971,71975,99090,3900185,3900199,3900275,3900509,3900542,3900775,
      3900908,3901033,3901075,3901085,3901107,3901108,3901116,3901225,3901233,
      3901332,3901544,3901575,3901585,3901616,3901733,3901785,3901909,3901925,
      3901926,3901934,3901944,3902009,3902017,3902026,3902041,3902075,3902107,
      3902117,3902126,3902207,3902221,3902226,3902241,3902275,3902307,3902321,
      3902334,3902340,3902341,3902375,3902523,3902526,3902585,3902640,3902724,
      3902734,3902736,3902802,3902824,3902834,3902885,3902936,3902985,3903040,
      3903324,3903338,3903402,3903436,3903438,3903538,3903624,3903705,3903924,
      3903936,3904024,3904085,3904105,3904124,3904235,3904385,3904404,3904485,
      3904505,3904535,3904605,3904705,3904802,3904815,3905004,3905080,3905180,
      3905205,3905280,3905285,3905305,3905380,3905480,3905580,3905680,3905702,
      3905715,3905780,3905880,3905980,3906080,3906180,3906185,3906280,3906304,
      3906380,3906480,3906485,3906580,3906680,3906685,3906780,3906985,3909223,
      3909942,3962091,3962092,3962093,3962094,3962095)
    ),
    "SID")
  
  # Define a list of all coastal stations by combining the coastal lists above
  Coastal_All <- bind_rows(Coastal_DINI) %>% unique()
  all_station_lists[["Coastal_All"]] <- Coastal_All
  
  if (return_defined_lists){
    cat("Just outputting the user-specified station lists\n")
    return(all_station_lists)
  }

  #=================================================#
  # GENERALLY NO NEED TO EDIT BEYOND HERE
  #
  # GENERATE STATIONLISTS BY USING EITHER THE DYNAMIC
  # OR STATIC METHOD
  #=================================================#
  
  # Filename which contains all the stationlists
  if (dynamic_gen){
    fname_domains <- paste0("dynamic_",param,"_sid_lists.rds")
  } else {
    fname_domains <- domain_file
  }
  
  #=================================================#
  # GET STATIONS EITHER FROM THE FOREAST OBJECT OR USING
  # THE SYNOP/TEMP FILES
  #=================================================#
  
  if (dynamic_gen){
    
  qwe  <- fcst_object[[1]] %>% dplyr::select(SID,lat,lon,elev) %>% dplyr::distinct()
  # May have multiple lat lon values here if definition changed in vfld
  # Remove NAs which can occur at random leadtimes for Pcp - vfld2sql processing?
  # Use "max" for elev in case of -99 instances
  qwe2 <- qwe %>% group_by(SID) %>% summarise(lat  = min(lat,na.rm = T),
                                              lon  = min(lon,na.rm = T),
                                              elev = max(elev,na.rm = T))
  if (nrow(qwe) != nrow(qwe2)) {
    mult_ll_SIDS <- names(table(qwe$SID)[table(qwe$SID)>1])
    mult_ll      <- qwe %>% filter(SID %in% mult_ll_SIDS) %>% arrange(SID)
    cat("Multiple lat/lon values for the following SIDs in the fcst_object:\n")
    print(as.data.frame(mult_ll))
    cat("Using min lat/lon value in each case\n")
  }
  allstations <- qwe2
    
  } else {
  
  # Use Harmonie's synop.list and temp.list for SID, lat, and lon
  # This is due to the fact that the lat/lon for some stations are missing in
  # the vobs files (a known issue at METIE, bufr2vobs related)
  #
  # Note that allsynop/alltemp.list are required for generating domains. 
  # If these files are not found, then abort if generate_domains=TRUE.
  # If generate_domains=FALSE, use harp's default station list for "allstations".
  # This "allstations" is then only used for plotting SID lists which are not
  # found in "domain_file".
  allsynop    <- file.path(sl_dir,"allsynop.list") 
  alltemp     <- file.path(sl_dir,"alltemp.list")
  if ((file.exists(allsynop)) & (file.exists(alltemp))) {
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
  } else {
    if (generate_domains) {
      stop("Did not find allsynop/temp.list with domain generation turned on")
    } else {
      if (plot_domains) {
        cat("Using default harp station list for plotting in fn_station_selection\n")
      }
      allstations <- harpCore::station_list
    }
  }
  
  # Modify the elevation column for plotting purposes
  allstations$elevmap                           <- "0: Missing"
  allstations$elevmap[allstations$elev < 10]    <- "1: <10m"
  allstations$elevmap[allstations$elev >= 10]   <- "2: 10-100m"
  allstations$elevmap[allstations$elev >= 100]  <- "3: 100-250m"
  allstations$elevmap[allstations$elev >= 250]  <- "4: 250-1000m"
  allstations$elevmap[allstations$elev >= 1000] <- "5: >1000m"
  
  } # Dynamic check
  
  if ( (generate_domains) || (dynamic_gen) ){
    
    # Two QC checks are carried out to compare stations in allsynop/temp.list 
    # (or fcst_object) versus an input OBSTABLE (or obs_object) i.e.;
    # 1) If OBSTABLE is provided, filter to common stations in OBSTABLE
    #    and allsynop/temp.list (only required for STATIC method)
    # 2) If multlatlon_rmv = TRUE, compare lat/lon positions in OBSTABLE (obs_object) 
    #    to those in allsynop/temp.list (fcst_object), and discard if significantly different
    # If neither of these checks run, then the variable "allstations"
    # will be defined solely by what's in the allsynop/temp.list (or fcst_object).
    
    #=================================================#
    # GET STATIONS IN THE OBSTABLE
    #=================================================#
    
    sql_stations <- NULL
    sql_full     <- NULL
    if (!dynamic_gen) {
    if (file.exists(sql_file)) {
      
    cat("Reading",sql_file,"\n")
    
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
    # Add extra filtering of this tibble to speed up SID,lat,lon search
    sql_full       <- sql_full %>% dplyr::select(SID,lat,lon) %>% 
      dplyr::distinct()
    
    # Save SID, lat, lon info from SQL file
    sql_stations$SID <- sort(unique(sql_full$SID))
    
    # Now filter to common SIDs in OBSTABLE and allstations
    allstations      <- allstations %>%
                        dplyr::filter(SID %in% sql_stations$SID)
    
    } else {
      
    warning("Cannot find" ,sql_file," - all SIDS in allsynop.list will be used")
      
    } # Filter to common stations in OBSTABLE
    } else {
    sql_full         <- obs_object %>% dplyr::select(SID,lat,lon) %>% dplyr::distinct()
    # Here use the stations in allstations instead of obs to avoid looping over
    # SIDs that won't be used anyway
    sql_stations$SID <- sort(unique(allstations$SID))
    }
    
    #=================================================#
    # LOOP OVER SQL STATIONS AND PERFORM QC CHECKS
    #=================================================#
    
    # multlatlon_rmv is only meaningful if you provide an OBSTABLE 
    if ( (multlatlon_rmv) & (!is.null(sql_full)) ){
      
    cat("Performing QC check of OBSTABLE positions vs forecast object/allsynop.list\n")
    
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
          cat("Filtering: Will remove station",sid,"from the stationlist (different lat/lon in OBSTABLE)\n")
          lat_v <- lat_v[1]
          lon_v <- lon_v[1]
          ind_v <- 1
        }
      }
      
      # Compare lat lon in OBSTABLE to the synop/temp.list values
      if (sid %in% allstations$SID) {
        
      lat_ref <- allstations$lat[allstations$SID == sid]
      lon_ref <- allstations$lon[allstations$SID == sid]
      # Some stations in OBSTABLE have missing lat/lon
      if ((lat_v != -99) & (lon_v != -99) & 
         (length(lat_ref) > 0) & (length(lon_ref) > 0)) { 
        
        if ((abs(lat_v - lat_ref) > tol_val) || 
            (abs(lon_v - lon_ref) > tol_val)) {
          #print(paste0("Difference between lat/lon values in synop/temp.list and the OBSTABLE for SID = ",sid))
          #print(paste0("Lat: ",paste(lat_v,lat_ref,sep = ",")," and Lon:",paste(lon_v,lon_ref,sep = ",")))
          cat("Filtering: Will remove station",sid,"from the stationlist (OBSTABLE lat/lon different to forecast)\n")
          ind_v <- 2
        }
      }
      
      } else {
      cat(sid,"is in the OBSTABLE but not the forecast object/allsynop.list - this will not be included!\n")
      ind_v <- 3
      }
      
      lat_vals <- c(lat_vals,lat_v)
      lon_vals <- c(lon_vals,lon_v)
      ind_vals <- c(ind_vals,ind_v)
    }
    
    sql_stations$lat <- lat_vals
    sql_stations$lon <- lon_vals
    sql_stations$ind <- ind_vals
    sql_stations     <- tibble::as_tibble(sql_stations)
    
    # Remove stations 
    mll_rmv_obstable <- sql_stations %>% 
                        dplyr::filter(ind > 0) %>%
                        dplyr::arrange(SID)
    mll_rmv_vfldlist <- allstations %>%
                        dplyr::filter(SID %in% mll_rmv_obstable$SID)
    allstations      <- allstations %>% 
                        dplyr::filter(!(SID %in% mll_rmv_obstable$SID))
    } else {
      
    warning("Skipping the position QC check between OBSTABLE and the allsynop list")
    mll_rmv_obstable <- NULL
    mll_rmv_vfldlist <- NULL    
  
    } # multlatlon_rmv
    
    #=================================================#
    # LOOP OVER DOMAINS AND GENERATE STATION LISTS FROM
    # LAT LON BOXES
    #=================================================#
    
    # Check if this filename already exists. If it does, then load the
    # current domainlists for comparison with the newly generated lists
    if (!dynamic_gen){
    if (file.exists(here::here("verification",fname_domains))) {
      cat(fname_domains,"already exists. Saving new data to a tmp file\n")
      all_current_domains <- readRDS(file = here::here("verification",
                                                       fname_domains))
      fname_domains_save  <- "tmp_sid_lists.rds"
    } else {
      cat("Create",fname_domains,"\n")
      fname_domains_save  <- fname_domains
    }
    } else {
    fname_domains_save <- fname_domains
    }
    
    # Note: if you use domain = "All", you will generate a list corresponding
    # to allstations
    if (length(domains_to_gen) == 1) {
      # "All_Domains" would only be used for static generation
      if (domains_to_gen == "All_Domains") {
        domains_to_gen <- all_reserved_names
        cat("Generating all reserved domains\n")
      } else {
        cat("Generating domain",domains_to_gen,"\n")
      }
    } else if (length(domains_to_gen) == 0){
      stop("Why are we here?")
    } else {
      cat("Generating domains",paste0(domains_to_gen,collapse = ","),"\n")
    }
    
    # Object to hold all generated stationlists
    all_gen_domains <- NULL

    for (domain in domains_to_gen) {
      
      read_poly   <- FALSE
      skip_filter <- FALSE
      poly_file   <- file.path(poly_dir,paste0(domain,".poly"))
      
      # Check for "_ELEVA", "_ELEVB", "_COASTAL", or "_INLAND" flags in domain name
      elevl     <- grepl("_ELEV",domain,fixed = T)
      coastal_l <- grepl("_COASTAL",domain,fixed = T)
      land_l    <- grepl("_INLAND",domain,fixed = T)
      # Cannot have multiple flags in the domain name!
      sum_flags <- sum(elevl,coastal_l,land_l)
      if (sum_flags>1) {
        warning("Domain ",domain," has mutliple flags, skipping\n")
        next
      }
      if (elevl) {
        domain_o <- domain
        domain   <- strsplit(domain_o,"_ELEV")[[1]][1]
        elevab   <- strsplit(strsplit(domain_o,"_ELEV")[[1]][2],"_")[[1]][1]
        elevf    <- strsplit(strsplit(domain_o,"_ELEV")[[1]][2],"_")[[1]][2]
      }
      if (coastal_l) {
        domain_o <- domain
        domain   <- strsplit(domain_o,"_COASTAL")[[1]][1]
      }
      if (land_l) {
        domain_o <- domain
        domain   <- strsplit(domain_o,"_INLAND")[[1]][1]
      }
      
      if (domain %in% names(all_station_lists)){
        if (dynamic_gen) {
          cat("Domain",domain,"is already a user specified list, skipping dynamic gen loop\n")
          # If flag is indicated, then continue the loop but skip the filtering
          if (sum_flags > 0) {
            skip_filter <- TRUE
            fixed_list  <- all_station_lists[[domain]]
          } else {
            next
          }
        } else {
          stop("A user-specified stationlist name already exists in the pre-defined domain-generated stationlists")
        }
      } else {
        if (grepl("Synop",domain,fixed = TRUE)) { # Filter based on WMO SID range
          slat <- NULL
          wlon <- NULL
          nlat <- NULL
          elon <- NULL
        } else if (file.exists(poly_file)) { # Then check based on poly file
          if (sf_available) {
            cat("Reading",poly_file,"\n")
            read_poly <- TRUE
          } else {
            cat("Trying to use poly file",poly_file,"but package sf is not available, skipping!\n")
            next
          }
        } else if (domain == "All") {
          cat("No filtering of stations applied for domain",domain,"\n")
        } else {
          # If none of the above, check lat/lon bounding box
          lll  <- define_latlonbounds(domain)
          slat <- lll$slat
          wlon <- lll$wlon
          nlat <- lll$nlat
          elon <- lll$elon
          if (is.null(slat)) {
            warning("Domain ",domain," not recognised, skipping\n")
            next
          } else {
            cat("Found lat/lon box",domain,"\n")
          }
        }
      }
      
      # Filter once to the region of interest
      if (domain != "All") {
        if (read_poly) {
          poly_df <- poly_filter(allstations,poly_file)
          df <- poly_df$df
          if (is.null(df)) {
            cat("An error occured for poly file",poly_file,"\n")
            cat("The domain",domain,"was not generated\n")
            next
          }
        } else {
          if (!skip_filter) {
            df <- filter_stations(allstations,domain,slat,nlat,wlon,elon)
          } else {
            df <- allstations %>% filter(SID %in% fixed_list$SID)
          }
        }
      } else {
        df <- allstations
      }
      
      # Check if this domain already exists. If so, add new SIDS for that region
      # Only relevant for static generation
      if (!dynamic_gen){
      if (file.exists(here::here("verification",fname_domains))) {
        cdf <- all_current_domains[[domain]]
        if (!is.null(cdf)) {
          new_df <- df %>% dplyr::filter(!(SID %in% unique(cdf$SID)))
          df     <- dplyr::bind_rows(cdf,new_df)
        }
      }
      
      # Then lat/lon filter again to deal with changing (in particular smaller)
      # domain definitions. This may reduce the number of stations in the domain.
      # Only relevant for static generation
      if (domain != "All") {
        if (read_poly) {
          poly_df <- poly_filter(df,poly_file)
          df <- poly_df$df
        } else {
          df <- filter_stations(df,domain,slat,nlat,wlon,elon)
        }
      } else {
        df <- allstations
      }
      } # dynamic gen
      
      # Now apply elev filtering if required
      if (elevl) {
        
        domain <- domain_o

        if (!("elev" %in% names(df))) {
          
          cat("No elevation found - cannot do filtering")
          next
          
        } else {
        
          # Remove -99 stations
          df99 <- df %>% filter(elev == -99)
          df   <- df %>% filter(elev != -99)
          if (nrow(df99) > 0) {
            cat("Removed the following stations due to -99 elevation\n")
            print(df99,n=nrow(df99))
          }
          
          if (elevab == "A") {
            df <- df %>% filter(elev > as.double(elevf))
          } else if (elevab == "B") {
            df <- df %>% filter(elev <= as.double(elevf))
          } else {
            stop("Elevation filtering went wrong - check your domain!")
          }
          
          if (nrow(df) == 0) {
            cat("No stations found for",domain,"- skipping\n")
            next
          }
        
        }
        
      }
      
      # Apply coastal filtering if required
      if (coastal_l) {
        domain <- domain_o
        df <- df %>% filter(SID %in% Coastal_All$SID)
        if (nrow(df) == 0) {
          cat("No stations found for",domain,"- skipping\n")
          next
        }
      }
      
      # Apply land filtering if required
      if (land_l) {
        domain <- domain_o
        df <- df %>% filter(!(SID %in% Coastal_All$SID))
        if (nrow(df) == 0) {
          cat("No stations found for",domain,"- skipping\n")
          next
        }
      }
      
      # Plot the maps if desired
      if ((plot_domains) & (nrow(df)>0)){
        if (read_poly) {
          p1        <- station_map(df,domain,polygon = poly_df$polygon)
        } else {
          p1        <- station_map(df,domain)
          
        }
        if (dynamic_gen) {
          png_fname <- paste0("dynamic_",param,"_",domain,"_stationmap.png")
        } else {
          png_fname <- paste0("sample-",domain,"-stationmap.png")
        }
        ggplot2::ggsave(p1,
                        filename = png_fname,
                        path     = png_path,
                        width    = 10,
                        height   = 4.5,
                        units    = "in",
                        dpi      = 200)
      }
      
      # Save to generated domain list
      # Only save domain "All" if using dynamic generation
      if (domain != "All"){
        all_gen_domains[[domain]] <- df
      } else {
        if (dynamic_gen){
          all_gen_domains[[domain]] <- df
        }
      }
      
    } # End of domain loop
    
    #=================================================#
    # OUTPUT SID DATA
    #=================================================#
    
    # If the domains file already exists, output the original domains that 
    # have not been generated here
    if (!dynamic_gen){
    if (file.exists(here::here("verification",fname_domains))) {
      orig_minus_domains <- all_current_domains[!(names(all_current_domains) %in%
                                                  domains_to_gen)]
      all_gen_domains    <- c(orig_minus_domains,all_gen_domains)
      
    }
    }
    
    # Add in some additional lists for reference
    if (multlatlon_rmv) {
      if (!dynamic_gen){
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
      }
      all_gen_domains[["mll_rmv_obstable"]] <- mll_rmv_obstable
      all_gen_domains[["mll_rmv_vfldlist"]] <- mll_rmv_vfldlist
    }
    
    # Save the output list (for static generation)
    if (!dynamic_gen){
    saveRDS(all_gen_domains,file = here::here("verification",fname_domains_save))
    }
    
  } else {
    
    # Read stationlist from an existing file
    if (file.exists(here::here("verification",fname_domains))) {
      cat("Read existing SID lists in",fname_domains,"\n")
      all_gen_domains <- readRDS(file = here::here("verification",fname_domains))
    } else {
      stop("Cannot find verification/",fname_domains)
    }
    
  }
  
  # Now pick out the relevant stationlist for the chosen domains and filter
  if (any(names(all_station_lists) %in% names(all_gen_domains))) {
    warning("A user-specified stationlist name already exists in the pre-defined domain-generated stationlists")
    stop("Aborting due to fn_station_selection error!")
    
  } else {
    all_station_lists  <- c(all_station_lists,all_gen_domains)
    stationlist_out    <- NULL
    if (all(domain_choice == "All_Domains")) {
      domain_choice <- names(all_station_lists)
    }
    for (dc in domain_choice) {
      if (dc %in% names(all_station_lists)) {
        stationlist_choice <- all_station_lists[[dc]] 
        
        # Filter
        if (nrow(stationlist_choice) > 0) {
          stationlist_choice <- stationlist_choice %>%
            dplyr::filter(!(SID %in% stations_to_rmv$SID))
          
          # Add in extra filtering in the case of dynamic generation which is
          # relevant to user-specified SID lists i.e.
          # 1) Filter to available stations in fcst_object (already done for gen domains)
          # 2) Filter out any stations in "mll_rmv_obstable" such that 
          # user-defined lists are consistent with gen lists
          if (dynamic_gen){
            
            stationlist_choice <- stationlist_choice %>% 
              dplyr::filter(SID %in% allstations$SID)
            
            if (!is.null(all_station_lists[["mll_rmv_obstable"]])){
              if (nrow(all_station_lists[["mll_rmv_obstable"]]) > 0){
                stationlist_choice <- stationlist_choice %>%
                  dplyr::filter(!(SID %in% all_station_lists[["mll_rmv_obstable"]]$SID))
              }
            }
          }
        }
        
        # Plot
        if ((plot_domains) & (nrow(stationlist_choice) > 0)) {
          psc <- stationlist_choice
          # Deal with case of user-specifed SID list where lat/lon not available
          if (!("lon" %in% names(psc))) { 
            psc <- allstations %>% filter(SID %in% psc$SID)
          }
          if (nrow(psc) > 0){
            p1 <- station_map(psc,dc)
            if (dynamic_gen) {
              png_fname <- paste0("dynamic_",param,"_",dc,"_stationmap.png")
            } else {
              png_fname <- paste0("sample-",dc,"-stationmap.png")
            }
            ggplot2::ggsave(p1,
                            filename = png_fname,
                            path     = png_path,
                            width    = 10,
                            height   = 4.5,
                            units    = "in",
                            dpi      = 200)
          } else {
            cat("Warning: Cannot plot domain",dc,"as no data found\n")
          }
        }
        
        if (nrow(stationlist_choice) > 0) {
          stationlist_out[[dc]] <- stationlist_choice
        } else {
          cat("No SIDs found for domain",dc," - this domain will be skipped!\n")
        }
        
      } else {
        warning("Domain ",dc," not recognised in station selection!\n")
      }
    }
    # Add in the stations_to_rmv (used when looking at "All" stations for static generation)
    stationlist_out[["stations_to_rmv"]] <- stations_to_rmv

    # Save dynamic SIDs for reference
    if (dynamic_gen){
        stationlist_out[["mll_rmv_obstable"]] <- mll_rmv_obstable
        stationlist_out[["mll_rmv_vfldlist"]] <- mll_rmv_vfldlist
        saveRDS(stationlist_out,file = here::here("verification",fname_domains_save))
    }
  }

  return(stationlist_out)
}


#=================================================#
# DEFINE THE LAT LON BOUNDS FOR DIFFERENT DOMAINS
#=================================================#

define_latlonbounds <- function(domain){

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
    slat <- 50.5
    wlon <- 2.5
    nlat <- 55.5
    elon <- 7.5
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
    #} else if (domain == "NNS") {
    #  slat <- 55
    #  wlon <- -1.5
    #  nlat <- 61
    #  elon <- 9
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
  } else {
    slat <- NULL
    wlon <- NULL
    nlat <- NULL
    elon <- NULL
  }
  
  return(list("slat" = slat,
              "wlon" = wlon,
              "nlat" = nlat,
              "elon" = elon))
  
}

#=================================================#
# LAT LON FILTER FOR
# 1) BOUNDING BOX
# 2) FIXED SID RANGES E.G. FROM 7001-7998
#=================================================#
filter_stations <- function(df,
                            domain,
                            slat,
                            nlat,
                            wlon,
                            elon){
  
  if (!is.null(slat)) {
    df_out <- df %>% 
      dplyr::filter(between(lat,slat,nlat)) %>%
      dplyr::filter(between(lon,wlon,elon))
  } else {
    if (grepl("France",domain,fixed = "TRUE")) {
      SID_min <- 7001
      SID_max <- 7998
    } else if (grepl("Germany",domain,fixed = "TRUE")) {
      SID_min <- 10001
      SID_max <- 10998
    } else if (grepl("Ireland",domain,fixed = "TRUE")) {
      SID_min <- 3950
      SID_max <- 3990
    } else if (grepl("Norway",domain,fixed = "TRUE")) {
      SID_min <- 1001
      SID_max <- 1499
    } else if (grepl("Sweden",domain,fixed = "TRUE")) {
      SID_min <- 2001
      SID_max <- 2699
    } else if (grepl("Finland",domain,fixed = "TRUE")) {
      SID_min <- 2700
      SID_max <- 2998
    } else if (grepl("Iceland",domain,fixed = "TRUE")) {
      SID_min <- 4001
      SID_max <- 4199
    } else if (grepl("Greenland",domain,fixed = "TRUE")) {
      SID_min <- 4200
      SID_max <- 4499
    } else if (grepl("Denmark",domain,fixed = "TRUE")) {
      SID_min <- 6001
      SID_max <- 6199
    } else if (grepl("Netherlands",domain,fixed = "TRUE")) {
      SID_min <- 6200
      SID_max <- 6399
    } else if (grepl("Spain",domain,fixed = "TRUE")) {
      SID_min <- 8001
      SID_max <- 8494
    } else if (grepl("Switzerland",domain,fixed = "TRUE")) {
      SID_min <- 6600
      SID_max <- 6998
    } else if (grepl("UnitedKingdom",domain,fixed = "TRUE")) {
      SID_min <- 3001
      SID_max <- 3949
    } else {
      stop("Domain",domain,"not recognised")
    }
    df_out <- df %>% dplyr::filter(between(SID,SID_min,SID_max))
  }
    
  return(df_out)
}

#=================================================#
# FILTER TO REGION DEFINED BY POLY FILE
#=================================================#
poly_filter <- function(df,
                        poly_file,
                        SID_filter = TRUE) {
  
  coords <- read.table(poly_file,
                       header    = FALSE,
                       sep       = "",
                       fill      = TRUE,
                       col.names = paste0("V",seq_len(20))) %>% 
            dplyr::select(V2,V1) %>% tidyr::drop_na() 
  colnames(coords) <- c("lon","lat")
  
  if (nrow(coords) < 3) {
    cat("Number of points in poly file should be at least 3, exiting!\n")
    return(list("df" = NULL,
                "polygon" = NULL))
  }
  
  if (!(all(coords[1,] == coords[nrow(coords),]))) {
    cat(poly_file,"is not a closed loop - enforcing this!\n")
    coords <- dplyr::bind_rows(coords,coords[1,])
  }
  
  polygon    <- sf::st_polygon(list(data.matrix(coords)))
  polygon_sf <- sf::st_sfc(polygon, crs = 4326)
  polygon_sf <- sf::st_make_valid(polygon_sf)
  if (!sf::st_is_valid(polygon_sf)){
    cat(poly_file,"is not valid, exiting!\n")
    return(list("df" = NULL,
                "polygon" = NULL))
  }
  
  polygon_sf  <- sf::st_transform(polygon_sf, 4326)
  stations_sf <- sf::st_as_sf(df, coords = c("lon","lat"), crs = 4326)
  stations_sf <- sf::st_transform(stations_sf, 4326)
  
  stations_in_polygon <- stations_sf[sf::st_intersects(stations_sf, 
                                                       polygon_sf, 
                                                       sparse = FALSE), ]
  if (SID_filter) {
    df <- df %>% filter(SID %in% stations_in_polygon$SID)
  } else {
    cnames <- setdiff(names(stations_in_polygon),"geometry")
    df <- inner_join(df,stations_in_polygon,by=cnames) %>% select(-geometry)
  }
  
  return(list("df" = df,
              "polygon" = polygon_sf))
}

#=================================================#
# FOR PLOTTING THE DOMAINS
#=================================================#
station_map <- function(df,domain,polygon = NULL){
  
  num_stations <- length(df$SID)
  if (!is.null(polygon)) {
    bbox <- sf::st_bbox(polygon)
    min_lon <- bbox$xmin
    max_lon <- bbox$xmax
    min_lat <- bbox$ymin
    max_lat <- bbox$ymax
  } else {
    min_lon      <- min(df[["lon"]]) - 0.2
    max_lon      <- max(df[["lon"]]) + 0.2
    min_lat      <- min(df[["lat"]]) - 0.2
    max_lat      <- max(df[["lat"]]) + 0.2  
  }
  
  if ("elevmap" %in% names(df)) {
    # Handle NA for elevmap only
    df    <- df %>% mutate(elevmap = case_when(
      is.na(elevmap) ~ "0: Missing",
      .default = elevmap
    ))
    p_map <- df %>% ggplot2::ggplot(aes(lon,lat,fill = elevmap),size = 3)
  } else if ("diff" %in% names(df)) {
    p_map <- df %>% ggplot2::ggplot(aes(lon,lat,fill = diff),size = 3)
  } else {
    p_map <- df %>% ggplot2::ggplot(aes(lon,lat,fill = "red"),size = 3)
  }
  p_map <- p_map + 
    ggplot2::geom_polygon(data        = ggplot2::map_data("world"),
                          mapping     = aes(long, lat, group = group),
                          fill        = "grey100",
                          colour      = "black",
                          inherit.aes = FALSE) +  
    ggplot2::geom_point(colour = 'grey40',pch = 21) + 
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
    ggplot2::labs(title = paste0(domain,": ",num_stations," stations")) +
    ggplot2::guides(size = "none") # Remove size label from legend
  
  if ("elevmap" %in% names(df)) {
    p_map <- p_map + ggplot2::scale_fill_brewer("Elevation",palette = "YlOrBr")
  } else if ("diff" %in% names(df)) {
    p_map <- p_map + ggplot2::scale_fill_gradient2("Diff",low=muted("blue"),high=muted("red"))
  } else {
    p_map <- p_map + ggplot2::guides(fill = "none") 
  }
  
  if (!is.null(polygon)) {
    p_map <- p_map + 
      ggplot2::geom_sf(data = polygon,aes(geometry = geometry),
                       fill="red",alpha = 0.2, inherit.aes = FALSE)
  }
  if (sf_available){
    p_map <- p_map + ggplot2::coord_sf(xlim = c(min_lon,max_lon),
                                       ylim = c(min_lat,max_lat))
  } else {
    p_map <- p_map + ggplot2::coord_cartesian(xlim = c(min_lon,max_lon),
                                              ylim = c(min_lat,max_lat))
  }
  
  return(p_map)
  
}
