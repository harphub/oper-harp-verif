#' Score into format for text box in plot
#'
#' @param verif_data[[fc_model]]
#' @return string for plotting
text_for_plot_scores <- function(data_tbl, score){
        paste0(score, " ", round(data_tbl[[score]][[score]], digits = 2), " (", data_tbl[[score]]$ranking,")")

}


#' Average rank into format for text box in plot
#'
#' @param verif_data[[fc_model]]
#' @return string for plotting
text_for_plot_avg_rank <- function(data_tbl){
	print(data_tbl)
        paste0("AVG Rank: ", round(data_tbl$avg_rank, digits = 2)," (", data_tbl$rank_avg_rank,")")

}


#' title into format for text box in plot
#'
#' @param verif_data[[fc_model]]
#' @return string for plotting
text_for_plot_title <- function(data_tbl){
        tmp <- data_tbl$FSS
	if (class(tmp$fcdate[[1]]) == "numeric"){
		fcdate <- format(as.POSIXct(tmp$fcdate, 
					      origin = "1970-01-01",
					      tz="UTC"),
				   format="%Y/%m/%d %H:%M")
	} else {
		fcdate <- tmp$fcdate
	}
        string <- paste(tmp$model %>% unique(),
			" ",
                        paste0(fcdate %>% unique(), " + ", tmp$leadtime/3600 %>% unique(), "h"),
                        " (",
                        data_tbl$rank_avg_fss_rank,
                        ")"
        )
}

#' Ranking of Values min based
#'
#' @param values to rank
#' @return ranks
rank_min <- function(...){
        rank(c(...), ties.method = "min")

}


#' Ranking of Values max based
#'
#' @param values to rank
#' @return ranks
rank_max <- function(...){
        rank(desc(c(...)), ties.method = "min")

}


