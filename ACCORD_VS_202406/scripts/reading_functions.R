#' Read parameter using grib message
#'
#'@param file 
#'@param grb_message
#'@return geofield
#' param <- read_param_with_grbmessg("<your_file>", <grb_message_nr>)
read_param_with_grbmessg <- function(file, grb_message){
        require(Rgrib2)
	grib_list <- Gopen(file)
        gf        <- Gdec(grib_list, grb_message)
        return (gf)
}



#' Read total precipitation from DEODE grib
#'
#'@param file
#'@return total precipitation as geofield
#' add to .bashrc export ECCODES_DEFINITION_PATH=$PERM/definitions:ECCODES_DEFINITION_PATH
#' added: $PERM/definitions/grib2/localConcepts/lfpw 
#' containging the files necessary to define the grib parameter information
#' param <- read_deode_tp("<your_file>")
read_deode_tp <- function(file){
	require(harp)
	require(Rgrib2)
        my_param_defs <- add_param_def(
                                       "tp",
                                       decription = "Total precipitation",
                                       grib = new_grib_param(
                                                             name = list(r = "tirf",
                                                                         g = "tgrp",
                                                                         s = "tsnowp"
                                                                         ),
                                                             ),
                                       func = function(r, g, s) r + g + s
                                       )
        tp_gf <- read_grid(
                           file,
                           "tp",
                           param_defs = my_param_defs,
                           file_format = "grib"
                           )
        return (tp_gf)
}



#' Read native msg and regrid using grb
#'
#' @param file_name file_name of msg
#' @param parameter channel of interest
#' @param is_forecast TRUE/FALSE decides whether satellite or model data are returned
#' @param format_opts list(python_function, python_version, file_2regridding_grb,
#' grib_message, strAreaId, invert_data, origin)
#' @return geofield
read_msg_reticulate <- function(file_name,
                                parameter,
                                format_opts = list(),
                                ...
                                ) {

        require(harp)
        require(reticulate)
        require(meteogrid)

        message("parameter: ", parameter, ", class: ", class(parameter))

        if (class(parameter) == "harp_parameter"){
                parameter <- parameter$fullname
        }

        reticulate::use_python(format_opts$python_version)
        reticulate::source_python(format_opts$python_function)

        message("############# call function sat_model_to_same_grid() with")
        message("file_name ", file_name)
        message("parameter ", parameter)
        message("file_4regridding ", format_opts$file_4regridding_grb)
        message("grib_message ", format_opts$grib_message)
        message("strAreaId ")
        print(format_opts$strAreaId)
        message(") ################")
        message("##### R --> python ##### reticulate ###")

        regridded_data <- sat_model_to_same_grid(file_name,
                                                 parameter,
                                                 format_opts$file_4regridding_grb,
                                                 format_opts$grib_message,
                                                 format_opts$strAreaId)

        message ("##### python --> R #####")
        projS <- regridded_data[[5]]
        dttm  <- regridded_data[[6]]

        gf_domain <- structure(list(
                                 projection=list(proj=projS$proj),
                                 nx=projS$nx,
                                 ny=projS$ny,
                                 dx=projS$dx,
                                 dy=projS$dy,
                                 SW=c(projS$SW_lon, projS$SW_lat),
                                 NE=c(projS$NE_lon, projS$NE_lat)),
                            class="geodomain")

        dttm <- as.POSIXct(dttm, format="%Y-%m-%dT%H:%M:%S")
        fcdate <- dttm - lead_time * 60 * 60
        t_str <- paste0(format(fcdate, format="%Y-%m-%d %H:%M:%S"), " + ", lead_time, "h")

        # select either obs or forecast via index
        if (format_opts$is_obs){
                data_index <- 1  # sat
                lead_time  <- 0
                time_str   <- format(dttm, format="%Y-%m-%d %H:%M:%S")
        } else {
                data_index <- 2  # mod
                time_str   <- t_str
        }

        if (format_opts$invert_data){
                # transpose and put data upside-down:
                data_array <- t(regridded_data[[data_index]])
                data_array <- data_array[, ncol(data_array):1]

        } else {
                data_array <- regridded_data[[data_index]]
        }

        data_gf <- meteogrid::as.geofield(data_array,
                       domain = gf_domain,
                       info = list(
                                   origin = format_opts$origin,
                                   name   = parameter,
                                   TIME   = time_str  ## not working
                                   ))

        message("valid_dttm, ", dttm)
        message("lead_time, ",  lead_time)


data <- tibble::tibble(
               valid_dttm   = dttm,
               parameter    = parameter,
               lead_time    = lead_time,
               fcdate       = fcdate,
               gridded_data = list(data_gf)
               )
print(data)
data <- as_harp_df(data)

# plot_field(data_gf)

return (data)
}



#' Read native nc grid in netcd file
#'
#' @param file_name file_name with data
#' @param parameter of interest
#' @param format_opts list(python_function, python_version, 
#' invert_data, origin)
#' @return geofield
read_nc_reticulate <- function(file_name,
                                parameter,
                                format_opts = list(),
                                ...
                                ) {

        require(harp)
        require(reticulate)
        require(meteogrid)

        message("parameter read by nc function, ", parameter, class(parameter))

        reticulate::use_python(format_opts$python_version)
        reticulate::source_python(format_opts$python_function)

        # the function being called here comes from the file: reading_functions.py
        returnList <- get_data_nc_file(file_name,
                                parameter)
       data_array <- returnList[[1]]
        projS <- returnList[[4]]
        dttm <-returnList[[5]]
        dttm <- as.POSIXct(dttm, format="%Y-%m-%dT%H:%M:%S")
        time_str   <- format(dttm, format="%Y-%m-%d %H:%M:%S")
        #print(dttm)
        #print("Creating geodomain")
        gf_domain <- structure(list(
                                 #projection=list(proj=projS$proj),
                                 projection=list(proj="latlon"), #harp does not seem to like regular_ll, which is what the grib file says
                                 nx=projS$nx,
                                 ny=projS$ny,
                                 dx=projS$dx,
                                 dy=projS$dy,
                                 SW=c(projS$SW_lon, projS$SW_lat),
                                 NE=c(projS$NE_lon, projS$NE_lat)),
                            class="geodomain")
     print("gf_domain being returned by the python function for nc")
     print(projS$proj)
     print(gf_domain)
     #print("Creating geofield")
     #print(class(data_array))
     #print(dim(t(data_array)))
     #reformat array to match meteogrid expectations as done above
     data_array <- t(data_array) 
     #NO NEED TO DO THIS, since the data is already oriented correctly
     #data_array <- data_array[, ncol(data_array):1]

     #print(dim(data_array))
     data_gf <- meteogrid::as.geofield(data_array,
                       domain = gf_domain,
                       info = list(
                                   origin = format_opts$origin,
                                   name   = parameter,
                                   TIME   = time_str  ## not working
                                   ))
     print("Creating tibble from nc file")
     #to fix: fcdate to be given as input
     fcdate <- as.POSIXct(dttm, format="%Y-%m-%dT%H:%M:%S")

     data <- tibble::tibble(
               valid_dttm = dttm,
               parameter = parameter,
               lead_time = lead_time,
               fcdate = fcdate,
               gridded_data = list(data_gf)
               )
    #print(data)
    data <- as_harp_df(data)


    return (data)
}

