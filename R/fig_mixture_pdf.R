#' Plot the monthly Gaussian mixture density divided by its components
#' @param model solarModel
#' @param nmonth number of month
#' @param bins number of bins
#' @examples
#' fig_mixture_monthly_pdf(model)
#' @rdname fig_mixture_pdf
#' @name fig_mixture_pdf
#' @aliases  fig_mixture_pdf
#' @aliases  fig_mixture_pdf_monthh
#' @export
fig_mixture_pdf <- function(model, bins = 30){
  # Plot for each month
  plot_1 <- fig_mixture_pdf_month(model, nmonth = 1, bins = bins)
  plot_2 <- fig_mixture_pdf_month(model, nmonth = 2, bins = bins)
  plot_3 <- fig_mixture_pdf_month(model, nmonth = 3, bins = bins)
  plot_4 <- fig_mixture_pdf_month(model, nmonth = 4, bins = bins)
  plot_5 <- fig_mixture_pdf_month(model, nmonth = 5, bins = bins)
  plot_6 <- fig_mixture_pdf_month(model, nmonth = 6, bins = bins)
  plot_7 <- fig_mixture_pdf_month(model, nmonth = 7, bins = bins)
  plot_8 <- fig_mixture_pdf_month(model, nmonth = 8, bins = bins)
  plot_9 <- fig_mixture_pdf_month(model, nmonth = 9, bins = bins)
  plot_10 <- fig_mixture_pdf_month(model, nmonth = 10, bins = bins)
  plot_11 <- fig_mixture_pdf_month(model, nmonth = 11, bins = bins)
  plot_12 <- fig_mixture_pdf_month(model, nmonth = 12, bins = bins)
  # y-label
  yleft <- gridtext::richtext_grob("Density", rot = 90, vjust = 1, gp = grid::gpar(fontsize = 20))
  # x-label
  xbottom <- gridtext::richtext_grob(text = 'Residuals', vjust = 0.25, gp = grid::gpar(fontsize = 20))
  gridExtra::grid.arrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6,
                          plot_7, plot_8, plot_9, plot_10, plot_11, plot_12,
                          ncol = 4, nrow = 3, left = yleft, bottom = xbottom)
}


#' Plot a monthly Gaussian mixture density divided by its components
#'
#' @examples
#' library(solarr)
#' model <- Bologna
#' fig_mixture_pdf_month(model)
#' @rdname fig_mixture_pdf
#' @name fig_mixture_pdf
#' @export
fig_mixture_pdf_month <- function(model, nmonth = 1, bins = 30){

  # Fitted residuals
  ut <- dplyr::filter(model$data, Month == nmonth)$u_tilde
  # Empiric density
  pdf_emp <- density(ut)
  # Normal mixture parameters
  params <- unlist(model$NM_model[nmonth,-1])
  # Normal mixture probabilities
  p <- solarr::dmixnorm(pdf_emp$x, means = params[1:2], sd = params[3:4], p = c(params[5], 1-params[5]))
  # Normal mixture component 1
  pdf_1 <- dnorm(pdf_emp$x, mean = params[1], sd = params[3])*params[5]
  # Normal mixture component 2
  pdf_2 <- dnorm(pdf_emp$x, mean = params[2], sd = params[4])*(1-params[5])
  # Normal mixture mean probability component 1
  pdf_1_mu <- dnorm(params[1], mean = params[1], sd = params[3])*params[5]
  # Normal mixture mean probability component 2
  pdf_2_mu <- dnorm(params[2], mean = params[2], sd = params[4])*(1-params[5])

  # Color depending on mean parameter
  color_1 <- ifelse(params[[1]] > params[[2]], "darkorange", "black")
  color_2 <- ifelse(params[[2]] > params[[1]], "darkorange", "black")

  ggplot()+
    geom_histogram(aes(ut, after_stat(density), color = "emp"), fill = "lightgray", bins = bins)+
    geom_line(aes(pdf_emp$x, pdf_1, color = "pdf_1"))+
    geom_line(aes(pdf_emp$x, pdf_2, color = "pdf_2"))+
    geom_line(aes(pdf_emp$x, p, color = "nm"), linetype = "dashed", size = 1.2)+
    geom_segment(aes(x = params[1], xend = params[1], y = 0, yend = pdf_1_mu),
                 color = color_1, linetype = "dashed", size = 0.5)+
    geom_segment(aes(x = params[2], xend = params[2], y = 0, yend = pdf_2_mu),
                 color = color_2, linetype = "dashed", size = 0.5)+
    scale_color_manual(values = c(emp = "black", nm = "red", pdf_1 = color_1, pdf_2 = color_2),
                       labels = c(emp = "Empiric", nm = "Normal-Mixture"))+
    scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, length.out = 5))+
    scale_y_continuous(breaks = round(seq.default(min(p), max(p), length.out = 4), 3))+
    labs(color = NULL, x = NULL, y = NULL, subtitle = lubridate::month(nmonth, label = T, abbr = F))+
    theme_bw()+
    theme(legend.position = "none",
          plot.subtitle = element_text(size = 20),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "black", linetype = "dashed", linewidth = 0.1),
          panel.grid.major.x = element_blank()
    )
}

