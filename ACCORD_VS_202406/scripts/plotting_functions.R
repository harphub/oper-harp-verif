#' Plot ob panel
#'
#' @param field 
#' @param model
#' @param domain
#' @param subdomain
#' @return ggplot plot for one model fcst 
plot_panel_ob <- function(field, obs_name, domain, subdomain, breaks, palette, with_legend = TRUE, only_legend = FALSE){
	require(patchwork)
        library(grid)

	message("plot_panel_ob, ", obs_name)

        p <- plot_panel_field(
                                 data = field,
                                 column = obs_name,
                                 domain = domain,
                                 subdomain = subdomain,
			         breaks = breaks,
			         palette = palette,
				 with_legend = with_legend,
				 only_legend = only_legend
	)
	text <- grobTree(rectGrob(gp=gpar(fill = "white")),
                 textGrob(obs_name, gp = gpar(fontsize = rel(6),
                                          col = "black")))	
        p     <- p +
		inset_element(text,
			      left = 0.0, right = 0.65, bottom = 0.97, top = 1.03)

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
plot_panel_fc <- function(data, field, model, domain, breaks, palette, subdomain, with_legend = TRUE, only_legend = FALSE){
	require(patchwork)
        fcp1 <- plot_panel_field(
                                 data = field,
                                 column = model,
                                 domain = domain,
			         breaks = breaks,
			         palette = palette,
                                 subdomain = subdomain,
				 with_legend = with_legend,
				 only_legend = only_legend
	)
        # ## fss table
	message("plot_panel_fc, ", model)

        fss_tbl <- plot_tbl_fss(data, score="FSS", no_xaxis=TRUE)
        fssp_tbl <- plot_tbl_fss(data, score="FSSp")

        txt_bias    <- textbox_for_plot(data, txtbox="score", "bias")
        txt_mae     <- textbox_for_plot(data, txtbox="score", "mae")
        txt_rmse    <- textbox_for_plot(data, txtbox="score", "rmse")
        txt_Rpearson <- textbox_for_plot(data, txtbox="score", "Rpearson")

        text_avg_rank <- textbox_for_plot(data, txtbox="avg_rank")

        text_title <- textbox_for_plot(data, txtbox="title")

        # ## puting it together
#         fcp     <- fcp1 +
#                 inset_element(fss_tbl,
#                               left = 0.0, bottom = 0.76, right = 0.24, top = 0.97) +
#                 inset_element(fssp_tbl,
#                               left = 0.012, bottom = 0.59, right = 0.24, top = 0.77) +
# inset_element(txt_bias,
#                       left = 0.25, right = 0.47, bottom = 0.9, top = 0.95) +
#         inset_element(txt_mae,
#                       left = 0.25, right = 0.47, bottom = 0.85, top = 0.9) +
#         inset_element(txt_rmse,
#                       left = 0.25, right = 0.47, bottom = 0.8, top = 0.85) +
#         inset_element(txt_Rpearson,
#                       left = 0.25, right = 0.47, bottom = 0.75, top = 0.8) +
#         inset_element(text_avg_rank,
#                       left = 0.7, right = 1, bottom = 0.97, top = 1.03) +
#         inset_element(text_title,
#                       left = 0.0, right = 0.65, bottom = 0.97, top = 1.03)
# 
        fcp     <- fcp1 +
                inset_element(fss_tbl,
                              left = 0.0, bottom = 0.74, right = 0.24, top = 0.95) +
                inset_element(fssp_tbl,
                              left = 0.0, bottom = 0.57, right = 0.24, top = 0.75) +
inset_element(txt_bias,
                      left = 0.0, right = 0.24, bottom = 0.45, top = 0.5) +
        inset_element(txt_mae,
                      left = 0.0, right = 0.24, bottom = 0.4, top = 0.45) +
        inset_element(txt_rmse,
                      left = 0.0, right = 0.24, bottom = 0.35, top = 0.4) +
        inset_element(txt_Rpearson,
                      left = 0.0, right = 0.24, bottom = 0.3, top = 0.35) +
        inset_element(text_avg_rank,
                      left = 0.6, right = 0.95, bottom = 0.97, top = 1.03) +
        inset_element(text_title,
                      left = 0.0, right = 0.59, bottom = 0.97, top = 1.03)


	return(as.ggplot(fcp))

}




#' Plotting fields for panels with ggplot
#'
#' @param geofield as tibble
#' @param column which is the column of the tibble containing geolist with geofield
#' @return "ggplot" plots fields
plot_panel_field <- function(data,
			     column,
			     domain = get_domain(data[[column]]),
			     subdomain = get_domain(data[[column]]),
			     breaks = c(0, 0.1, 0.2, 0.5, 1., 5., 10., 15.,
					20., 25., 30., 35., 40., 45., 50., 100),
			     palette = c("#FFFFFE","#00FE96","#00FEC8","#00FEFE",
					 "#00C8FE","#0096FE","#0032FE","#3200FE",
					 "#6400FE","#9600FE","#C800FE","#FA00FE",
					 "#C800C8", "#960096","#FF0000"),
			     limits = c(min(breaks), max(breaks)),
			     NAcolour = "white",
			     with_legend = TRUE,
			     only_legend = FALSE
			     ){
        
	require(ggpubr)
	require(ggplotify)

	countries <- get_map(dom = domain, poly = FALSE)
	verif_box <- meteogrid:::DomainExtent(subdomain)
	boxdim <- data.frame(xmin=verif_box$x0, xmax=verif_box$x1,
			     ymin=verif_box$y0, ymax=verif_box$y1)

        p <- ggplot() +
        geom_georaster(aes(geofield = !!as.name(column)), data) +
        geom_path(aes(x, y), countries) +
	geom_rect(data=boxdim, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), colour="red", alpha=0) + 
        scale_fill_gradientn("mm",
                             limits  = limits,
                             colours = palette,
                             values  = scales::rescale(breaks, to=c(0, 1)),
                             breaks  = breaks,
			     na.value= NAcolour,
			     guide   = "legend") +
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
			 score = "FSS",
			 no_xaxis = FALSE,
			 rank_clrs = c("0"="black", "1"="red",
				       "2"="limegreen","3"="#f7ce16",
				       "4"="#b3bac9", "5"="#FF9933", "6"="ffffff")
			 ) {

	data_fss  	    <- data_tbl[[score]]
	rank_avg_fss_rank   <- data_tbl$rank_avg_fss_rank
	rank_avg_rank_4crls <- data_tbl$rank_avg_fss_rank %>% ifelse(. > 3, 4, .)
	model 		    <- unique(data_fss$model)
	fcdate 	            <- unique(data_fss$fcdate)

	data_fss <- data_fss %>% mutate(ranking = replace(ranking, ranking > 5, 5)) %>%
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
		box <- colour_for_text(text, data_tbl[[string]]$ranking)
	}
	else if (txtbox == "title"){
		text <- text_for_plot_title(data_tbl)
		box <- colour_for_text(text, data_tbl$rank_avg_fss_rank)
	}
	else if (txtbox == "avg_rank"){
		text <- text_for_plot_avg_rank(data_tbl)
		box <- colour_for_text(text, data_tbl$rank_avg_rank)
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
        obpanel  <- plot_panel_ob(field = verif_fields[[ob_name]]$obfield,
                              obs_name = ob_name,
                              domain = domain,
			      breaks = breaks,
			      palette = palette,
                              subdomain = verif_subdomain,
                              with_legend = FALSE
        )

        # ## fcfields
        fcpanels        <- vector("list", length(fcst_models))
	names(fcpanels) <- names(verif_data)
        for (model in names(verif_data)) {
                fcpanels[[model]] <- plot_panel_fc(data = verif_data[[model]],
                             field = verif_fields[[model]]$fcfield,
                             model = model,
			     breaks = breaks,
			     palette = palette,
                             domain = domain,
                             subdomain = verif_subdomain,
                             with_legend = FALSE
                )
        }

        # ## legend
        legend <- plot_panel_field(data = verif_fields[[ob_name]]$obfield,
                                 column = ob_name,
                                 domain = domain,
			         breaks = breaks,
			         palette = palette,
                                 subdomain = verif_subdomain,
                                 only_legend = TRUE
        )

        # ## put panels together
        plt <- obpanel
        for (i in 1:(length(fcst_models))) {
                plt <- plt + fcpanels[i]
        }
        plt <- as.ggplot(plt + plot_layout(ncol = 4, byrow = TRUE)) +
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
               width = as.numeric(plot_window_all["width"])+10,
               height = as.numeric(plot_window_all["height"]),
               units = plot_window_all["units"],
               dpi = as.numeric(plot_window_all["dpi"]))
        message("Saved panel to: ", paste0(plot_path, plot_name))

}


