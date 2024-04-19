
#================================================#
# PLOT THE DIFF BETWEEN TWO MODELS AND THE
# CONFIDENCE OF THAT DIFF (USING BOOTSTRAP DATA)
#
# TAKEN DIRECTLY FROM THE 2022 harp COURSE
# WRITTEN BY ANDREW SINGLETON
#================================================#

fn_plot_signif_diff <- function(x,
                                verif_type,
                                plot_score,
                                fcst_model,
                                ref_model,
                                significance = 0.95,
                                conf = c("ribbon", "errorbar"),
                                alpha = 0.5,
                                domain = "NA",
                                num_cycles = "NA"){
  
  conf <- match.arg(conf)
  
  sig_text <- c(
    paste0(
      fcst_model, " better than ", ref_model, 
      " with ", significance * 100, "% confidence"
    ),
    paste0(
      fcst_model, " worse than ", ref_model,
      " with ", significance * 100, "% confidence"
    ),
    paste0(
      "No difference between ", fcst_model, " and ", ref_model, 
      " with ", significance * 100, "% confidence"
    )
  )
  
  shapes        <- c(24, 25, 1)
  names(shapes) <- sig_text
  
  colours        <- c("#5555DD", "#DD5555", "#ACACAC")
  names(colours) <- sig_text
  
  if (verif_type == "ens") {
    d2f <- x$ens_summary_scores
  } else {
    d2f <- x$det_summary_scores
  }
  
  plot_data <- dplyr::filter(
    d2f,
    score %in% plot_score,
    fcst_model == .env$fcst_model,
    ref_model == .env$ref_model
  ) %>% 
    dplyr::mutate(
      sig = dplyr::case_when(
        percent_better >= significance       ~ sig_text[1],
        percent_better <= (1 - significance) ~ sig_text[2],
        TRUE                                 ~ sig_text[3]
      )
    )
  
  dttm_avail <- attributes(x)$dttm
  if (is.list(dttm_avail)) {
    dttm_avail <- dttm_avail %>% unlist() %>% unique()
  }
  dttm_avail <- dttm_avail[order(dttm_avail)]
  title_str <- paste0(paste(harpVis:::totitle(gsub("_", " ", plot_score)),
                            collapse = "; "), " : ",
    stringr::str_to_title(attr(x, "parameter")), " : ",
    suppressMessages(format(harpIO::str_datetime_to_datetime(dttm_avail[1]),"%Y-%m-%d-%H")),
    " - ",
    suppressMessages(format(harpIO::str_datetime_to_datetime(tail(dttm_avail,1)),"%Y-%m-%d-%H")))
  
  if (num_cycles != "NA") {
    title_str <- paste0(title_str," (",num_cycles," cycles)")
  }
  
  if (is.list(attr(x, "stations"))) {
    num_stations <- length(attr(x, "stations") %>% unlist() %>% unique())
  } else{
    num_stations <- length(attr(x, "stations"))
  }
  
  if (domain != "NA") {
    subtitle_str <- paste0(fcst_model,"-",ref_model," over ",
                           domain," stations (",num_stations,")")
  } else {
    subtitle_str <- paste0(fcst_model,"-",ref_model," at ",
                           num_stations," stations")
  }
  
  p <- ggplot2::ggplot(
    plot_data, 
    aes(
      x        = lead_time,
      y        = difference_mean, 
      ymin     = difference_lower, 
      ymax     = difference_upper, 
      linetype = score
    )
  ) + 
    ggplot2::labs(
      x        = "Lead Time [h]", 
      y        = paste(harpVis:::totitle(gsub("_", " ", plot_score)),
                       collapse = "; "),
      colour   = NULL, 
      linetype = NULL,
      shape    = NULL,
      title    = title_str,
      subtitle = subtitle_str
    ) 
  
  if (length(plot_score) < 2) {
    p  <- p + ggplot2::guides(linetype = "none") 
    lt <- 8
  } else {
    p  <- p + ggplot2::guides(
      linetype = ggplot2::guide_legend(NULL,
                                       override.aes = list(fill = NA),
                                       nrow = 2))
    lt <- 7
  }
  
  if (conf == "errorbar") {
    p <- p + ggplot2::geom_errorbar(colour = "#555555")
  }
  
  if (conf == "ribbon") {
    p <- p + ggplot2::geom_ribbon(alpha = alpha, fill = "#AAAAAA") +
      ggplot2::guides(fill = "none")
  }
  
  p <- p +
    ggplot2::geom_line(colour = "#555555",linewidth = 1) +
    ggplot2::geom_point(aes(shape = sig, colour = sig, fill = sig)) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_shape_manual(values = shapes) +
    ggplot2::scale_x_continuous(breaks = seq(0, 48, 6)) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(nrow = 3),
      fill   = ggplot2::guide_legend(NULL, nrow = 3),
      shape  = ggplot2::guide_legend(nrow = 3)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 10),
      plot.subtitle   = ggplot2::element_text(size = 10),
      axis.text       = ggplot2::element_text(size = 10),
      axis.title      = ggplot2::element_text(size = 10),
      legend.text     = ggplot2::element_text(size = lt),
      legend.position = "bottom"
    )
  
  return(p)
}