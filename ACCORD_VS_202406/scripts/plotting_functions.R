#' Plot ob panel
#'
#' @param field 
#' @param model
#' @param domain
#' @param subdomain
#' @return ggplot plot for one model fcst 
plot_panel_ob <- function(ob_field,
			  ob_name,
			  domain,
			  subdomain,
			  breaks,
			  palette,
			  with_legend = TRUE,
			  only_legend = FALSE){
	require(patchwork)
        library(grid)

	message("plot_panel_ob, ", ob_name)

        p <- plot_panel_field(
                              data        = ob_field,
                              column      = ob_name,
                              domain      = domain,
                              subdomain   = subdomain,
			      breaks      = breaks,
			      palette     = palette,
			      with_legend = with_legend,
			      only_legend = only_legend
			      )
	panel_title <- paste0(ob_name, "  ",
			     format(ob_field$valid_dttm,
				    format = "%Y/%m/%d %H:%M")) 
	text <- grobTree(rectGrob(gp=gpar(fill = "white")),
                textGrob(panel_title,
			 gp = gpar(fontsize = rel(6),
                                   col      = "black")))	
        p     <- p +
		inset_element(text,
			      left   = 0.0,
			      right  = 0.95,
			      bottom = 0.97,
			      top    = 1.03)

	return(as.ggplot(p))

}


#' Plot fcst panel
#'
#' @param data 
#' @param field 
#' @param model
#' @param domain
#' @param subdomain
#' @return ggplot plot for one model fcst 
plot_panel_fc <- function(data,
			  field,
			  model,
			  domain,
			  breaks,
			  palette,
			  subdomain,
			  ll_score_box,
			  ll_fss_box,
			  with_legend  = TRUE,
			  only_legend  = FALSE){
	require(patchwork)
        fcp1 <- plot_panel_field(
                                 data        = field,
                                 column      = model,
                                 domain      = domain,
			         breaks      = breaks,
			         palette     = palette,
                                 subdomain   = subdomain,
				 with_legend = with_legend,
				 only_legend = only_legend
	)
        # ## collect the box info ###
	message("plot_panel_fc, ", model)
        text_title    <- textbox_for_plot(data, txtbox="title")
        text_avg_rank <- textbox_for_plot(data, txtbox="avg_rank")

        fss_tbl       <- plot_tbl_fss(data, score="FSS", no_xaxis=TRUE)
        fssp_tbl      <- plot_tbl_fss(data, score="FSSp")

        txt_bias      <- textbox_for_plot(data, txtbox="score", "bias")
        txt_mae       <- textbox_for_plot(data, txtbox="score", "mae")
        txt_rmse      <- textbox_for_plot(data, txtbox="score", "rmse")
        txt_Rpearson  <- textbox_for_plot(data, txtbox="score", "Rpearson")

        # ## putting it together ###

	x0     <- ll_score_box[[1]]
	x1     <- ll_score_box[[2]]
	y0     <- ll_score_box[[3]]
	y1     <- ll_score_box[[4]]
	y_step <- (y1 - y0) / 4

 	x0_fss     <- ll_fss_box[[1]]
 	x1_fss     <- ll_fss_box[[2]]
 	y0_fss     <- ll_fss_box[[3]]
 	y1_fss     <- ll_fss_box[[4]]
 	y_step_fss <- trunc((y1_fss - y0_fss) / 4 * 100) / 100

	
	fcp <- fcp1 +
               inset_element(text_title,    # title - name, date, parameter
                      left   = 0.0,
		      right  = 0.64,
		      bottom = 0.97,
		      top    = 1.03) + 
               inset_element(text_avg_rank, # title - avg rank basic score
                      left   = 0.65,
		      right  = 0.95,
		      bottom = 0.97,
		      top    = 1.03) +
	       inset_element(fss_tbl,       # FSS
	       	      left   = x0_fss,
		      bottom = y0_fss + 0.17,
		      right  = x1_fss,
		      top    = y0_fss + 0.38) + 
               inset_element(fssp_tbl,      # FSS percentile
	       	      left   = x0_fss + 0.005,
		      bottom = y0_fss,
		      right  = x1_fss,
		      top    = y0_fss + 0.17) +
               inset_element(txt_bias,      # basic score - bias
		      left   = x0,
		      right  = x1,
		      bottom = y0 + 3 * y_step,
		      top    = y0 + 4 * y_step) +
               inset_element(txt_mae,       # basic score - mae
                      left   = x0,
		      right  = x1,
		      bottom = y0 + 2 * y_step,
		      top    = y0 + 3 * y_step) +
               inset_element(txt_rmse,      # basic score - rmse
                      left   = x0,
		      right  = x1,
		      bottom = y0 + y_step,
		      top    = y0 + 2 * y_step) +
               inset_element(txt_Rpearson,  # basic score - Rpearson
                      left   = x0,
		      right  = x1,
		      bottom = y0,
		      top    = y0 + y_step)

	return(as.ggplot(fcp))

}



#' Plotting fields for panels with ggplot
#'
#' @param geofield as tibble
#' @param column which is the column of the tibble containing geolist with geofield
#' @return "ggplot" plots fields
plot_panel_field <- function(data,
			     column,
			     domain      = get_domain(data[[column]]),
			     subdomain   = get_domain(data[[column]]),
			     breaks      = c(0, 0.1, 0.2, 0.5, 1., 5., 10., 15.,
					20., 25., 30., 35., 40., 45., 50., 100),
			     palette     = c("#FFFFFE","#00FE96","#00FEC8","#00FEFE",
					 "#00C8FE","#0096FE","#0032FE","#3200FE",
					 "#6400FE","#9600FE","#C800FE","#FA00FE",
					 "#C800C8", "#960096","#FF0000"),
			     limits      = c(min(breaks), max(breaks)),
			     NAcolour    = "white",
			     with_legend = TRUE,
			     only_legend = FALSE
			     ){
        
	require(ggpubr)
	require(ggplotify)

        message("data:")
	print(data)
	message("column: ", column)

	countries <- get_map(dom = domain, poly = FALSE)
	verif_box <- meteogrid:::DomainExtent(subdomain)
	boxdim    <- data.frame(xmin = verif_box$x0,
		   		xmax = verif_box$x1,
		   		ymin = verif_box$y0,
		   		ymax = verif_box$y1)

        plt_units <- data$units

        p <- ggplot() +
        geom_georaster(aes(geofield = !!as.name(column)), data) +
        geom_path(aes(x, y), countries) +
	geom_rect(data=boxdim, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), colour="red", alpha=0) + 
        scale_fill_gradientn(plt_units,
                             limits   = limits,
                             colours  = palette,
                             values   = scales::rescale(breaks, to=c(0, 1)),
                             breaks   = breaks,
			     na.value = NAcolour,
			     guide    = "legend") +
        guides(fill = guide_legend(override.aes = list(size = 0.5)),
                                     reverse = TRUE) +
        coord_equal(expand = FALSE) +
        theme_harp_map() + 
	theme(plot.margin=unit(c(0,0,0,0), "cm"))

        if (!with_legend) {
		p <- p + theme(legend.position = "none")
        }

        if (only_legend) {
		p <- as_ggplot(get_legend(p))
        }

	return(as.ggplot(p))

}


#' Plotting FSS ranking in table according to panelification tool
#'
#' @param fss tibble containing fss scores of one verification
#' @return ggplot
plot_tbl_fss <- function(data_tbl,
			 score     = "FSS",
			 no_xaxis  = FALSE,
			 rank_clrs = c("0"="black",
				       "1"="red",
				       "2"="limegreen",
				       "3"="#f7ce16",
				       "4"="#b3bac9",
				       "5"="#FF9933",
				       "6"="ffffff")
			 ) {

	data_fss  	    <- data_tbl[[score]]
	rank_avg_fss_rank   <- data_tbl$rank_avg_fss_rank
	rank_avg_rank_4crls <- data_tbl$rank_avg_fss_rank %>% 
		ifelse(. > 3, 4, .)
	model 		    <- unique(data_fss$model)
	fcdate 	            <- unique(data_fss$fcdate)

	data_fss <- data_fss %>% 
		mutate(ranking = replace(ranking, ranking > 5, 5)) %>%
		mutate(ranking = as.character(ranking))

        thres <- factor(data_fss$threshold, levels = rev(unique(data_fss$threshold)))
        
        if (no_xaxis) {
		x_text <- element_blank()
	} else {
		x_text <- element_text(angle = 90, size = rel(0.6))
	}
        
	ggplot(data_fss, aes(x = as.factor(scale), y = thres, fill = ranking)) +
                geom_tile(color = "white", linewidth = 0.2) +
                scale_x_discrete(name = NULL) +
                scale_y_discrete(name = NULL) +
                scale_fill_manual(values = rank_clrs) +
                theme(axis.text.x 	= x_text,
                      axis.text.y 	= element_text(size=rel(0.6)),
                      legend.position 	= "none",
                      plot.margin	= unit(c(0,0,0,0), "null"),
                      panel.spacing 	= unit(0, "lines"))
		
}


#' text into coloured box for plot
#'
#' @param verif_data[[fc_model]]
#' @return string for plotting
textbox_for_plot <- function(data_tbl, txtbox, string=""){
	message("txtbox, string", txtbox, string, collapse=" ")
	if (txtbox == "score"){
		text <- text_for_plot_scores(data_tbl, string)
		box  <- colour_for_text(text, data_tbl[[string]]$ranking)
	}
	else if (txtbox == "title"){
		text <- text_for_plot_title(data_tbl)
		box  <- colour_for_text(text, data_tbl$rank_avg_fss_rank)
	}
	else if (txtbox == "avg_rank"){
		text <- text_for_plot_avg_rank(data_tbl)
		box  <- colour_for_text(text, data_tbl$rank_avg_rank)
	} else {
		error("which_box is none of the three known: score, title, avg_rank")
	}

	return(box)
	
}



#' text into coloured box for plot
#'
#' @param verif_data[[fc_model]]
#' @return string for plotting
colour_for_text <- function(text, rank){
	library(grid)
	top3_clrs = c("1"="#f7ce16", "2"="#b3bac9",
		      "3"="#FF9933", "4"="white",
	              "0"="white")
	rank_4_clr <- rank %>% ifelse(. > 4, 4, .) %>%
		as.character()
        grobTree(rectGrob(gp=gpar(fill = top3_clrs[[rank_4_clr]])),
		 textGrob(text, gp = gpar(fontsize = rel(6),
					  col = "black")))
}


#' set window plot size 
#'
#' @param fc_models list of fcst models
#' @return height, width, dpi, units for plot window
set_plot_size <- function(fcst_models) {
        plot_size <- c("height" = 70,
		       "width"  = 100,
                       "dpi"    = 360,
                       "units"  = "mm")

        if (length(fcst_models) + 1 <= 4) {
                plot_window <- c("height" = as.numeric(plot_size["height"]),
				 "width"  = as.numeric(plot_size["width"]) *
					 (length(fcst_models) + 2),
                                 "dpi"    = 360,
                                 "units"  = "mm")
        } else {
                plot_window <- c("height" = as.numeric(plot_size["height"]) *
				 ceiling((length(fcst_models) + 2)/4),
				 "width"  = as.numeric(plot_size["width"]) * 4,
                                 "dpi"    = 360,
                                 "units"  = "mm")
        }
}



#' main plotting function which calls the subfunctions
main_plotting <- function(verif_data, verif_fields, ob_name, param, plt_definitions, plot_path="", plot_name="panel.png"){
        source(plt_definitions)
        
	fcst_models <- names(verif_data)
        if (!is_null(verif_fields[[ob_name]]$obfield[[ob_name]][[1]])){
	   domain <- verif_fields[[ob_name]]$obfield[[ob_name]][[1]] %>% get_domain()
	} else {
	   warning("ERROR: geolist in verif_fields[[", ob_name, "]] is empty!")
	}

	if (!is_null(verif_fields$verif_domain)){
        verif_subdomain <- verif_fields$verif_domain
	} else {
           warning("ERROR: verif_fields$verif_domain is empty!")
	}

        source(paste0(here(),"/ACCORD_VS_202406/scripts/utils.R"))

        # ## obfield
        obpanel  <- plot_panel_ob(ob_field = verif_fields[[ob_name]]$obfield,
                              ob_name      = ob_name,
                              domain       = domain,
			      breaks       = breaks,
			      palette      = palette,
                              subdomain    = verif_subdomain,
                              with_legend  = FALSE
        )

        # ## fcfields
        fcpanels        <- vector("list", length(fcst_models))
	names(fcpanels) <- names(verif_data)
        for (model in names(verif_data)) {
                fcpanels[[model]] <-
			plot_panel_fc(
		             data         = verif_data[[model]],
                             field        = verif_fields[[model]]$fcfield,
                             model        = model,
			     breaks       = breaks,  # plt_definitions
			     palette      = palette, # plt_definitions
                             domain       = domain,
                             subdomain    = verif_subdomain,
                             ll_score_box = ll_score_box, # plt_definitons
                             ll_fss_box   = ll_fss_box,   # plt_definitions
                             with_legend  = FALSE
			     )
        }

        # ## legend
        legend <- plot_panel_field(
				   data        = verif_fields[[ob_name]]$obfield,
				   column      = ob_name,
				   domain      = domain,
				   breaks      = breaks,
				   palette     = palette,
				   subdomain   = verif_subdomain,
				   only_legend = TRUE
				   )

        # ## put panels together
        plt <- obpanel
        for (i in 1:(length(fcst_models))) {
                plt <- plt + fcpanels[i]
        }
        plt <- as.ggplot(plt +
			 plot_layout(ncol = 4, byrow = TRUE)) +
                         legend +
                         plot_layout(width = c(10, 1))

        # ## set size of the plot dependent on number of fields
        plot_window_all <- set_plot_size(fcst_models)

        # ## save plot to png
        if (plot_path == "") {
                plot_path <- paste0(here::here(), "/ACCORD_VS_202406/PLOTS/")
        }
        if (plot_name == "") {
                plot_name <- paste(veri_time, parameter, sep="_")
        }
        ggsave(paste0(plot_path, plot_name), plot = plt,
               width  = as.numeric(plot_window_all["width"])+10,
               height = as.numeric(plot_window_all["height"]),
               units  = plot_window_all["units"],
               dpi    = as.numeric(plot_window_all["dpi"]))
        message("Saved panel to: ", paste0(plot_path, plot_name))

}


