#' read native msg and regrid using grb
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

	message("parameter, ", parameter, class(parameter))

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
	message("strAreaId ", format_opts$strAreaId)
	message(") ################")

	regridded_data <- sat_model_to_same_grid(file_name,
						 parameter,
						 format_opts$file_4regridding_grb,
						 format_opts$grib_message,
						 format_opts$strAreaId)

	projS <- regridded_data[[5]]
	dttm <- regridded_data[[6]]

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
	t_str <- paste0(as.character(fcdate, format="%Y-%m-%d %H:%M:%S"), " + ", lead_time, "h")

        # select either obs or forecast via index
	if (format_opts$is_obs){
		data_index <- 1  # sat
		lead_time  <- 0
		time_str   <- as.character(dttm, format="%Y-%m-%d %H:%M:%S")
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
	message("lead_time, ", lead_time)


data <- tibble::tibble(
	       valid_dttm = dttm,
	       parameter = parameter,
	       lead_time = lead_time,
	       fcdate = fcdate,
	       gridded_data = list(data_gf)
	       # units = "K",
	       )
print(data)
data <- as_harp_df(data)

# plot_field(data_gf)

return (data)
}


