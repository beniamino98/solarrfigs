#' Plot a probabilistic forecast
#'
#' @param model par
#' @param date par
#' @param ci par
#' @param type par
#' @param unconditional par
#' @examples
#' library(solarr)
#' model <- Bologna
#' fig_probabilistic_forecast(model)
#'
#' @rdname fig_probabilistic_forecast
#' @name fig_probabilistic_forecast
#' @export
fig_probabilistic_forecast <- function(model, date = "2013-01-09", ci = 0.1, type = "mix", unconditional = FALSE){

  object <- solarModel_forecaster(model, date = date, ci = ci, unconditional = unconditional)

  grid <- object$grid
  emp <- object$emp
  e_mix <- object$e_mix
  e_up <- object$e_up
  e_dw <- object$e_dw
  type <- match.arg(type, choices = c("mix", "up", "dw"))
  vals_colors <- c(realized = "black", expected_value = "orange",seasonal = "red", bounds = "purple", fitted = "magenta")
  vals_labels <- c(realized = "Realized", expected_value = "Expectation", seasonal = "Seasonal",
                   bounds = "Interval",
                   fitted = "Point pred")

  pdf_plot <-  ggplot()+
    # Mixture density
    geom_line(data = grid, aes(x, pdf_GHI))+
    # Components densities (weighted by prior probs)
    geom_line(data = grid, aes(x, pdf_GHI_up), color = "green")+
    geom_line(data = grid, aes(x, pdf_GHI_dw), color = "red")+
    geom_point(data = emp, aes(GHI, pdf_GHI, color = "realized"), size = 3)

  if (type == "mix") {
    pdf_plot <- pdf_plot+
      geom_point(data = emp, aes(GHI_hat, pdf_GHI_hat, color = "fitted"), shape = 17, size = 2)+
      geom_point(data = e_mix, aes(e_GHI, pdf_e_GHI, color = "expected_value"), size = 3)+
      geom_segment(data = e_mix, aes(x = ci_lo, xend = ci_lo, y = 0, yend = pdf_ci_lo, color = "bounds"), size = 0.5)+
      geom_segment(data = e_mix, aes(x = ci_hi, xend = ci_hi, y = 0, yend = pdf_ci_hi, color = "bounds"), size = 0.5)+
      geom_point(data = e_mix, aes(ci_lo, 0, color = "bounds"), size = 3)+
      geom_point(data = e_mix, aes(ci_hi, 0, color = "bounds"), size = 3)+
      geom_segment(data = e_mix, aes(x = ci_lo, xend = ci_hi, y = 0, yend = 0, color = "bounds"))+
      geom_point(data = emp, aes(GHI_hat, pdf_GHI_hat, color = "fitted"), shape = 17, size = 2)
  } else if (type == "up") {
    pdf_plot <- pdf_plot+
      geom_point(data = emp, aes(GHI_hat_up, pdf_GHI_hat_up, color = "fitted"), shape = 17, size = 2)+
      geom_point(data = e_up, aes(e_GHI, pdf_e_GHI, color = "expected_value"), size = 3)+
      geom_point(data = e_up, aes(ci_lo, 0, color = "bounds"), size = 3)+
      geom_segment(data = e_up, aes(x = ci_lo, xend = ci_lo, y = 0, yend = pdf_ci_lo, color = "bounds"), linewidth = 0.5)+
      geom_segment(data = e_up, aes(x = ci_hi, xend = ci_hi, y = 0, yend = pdf_ci_hi, color = "bounds"), linewidth = 0.5)+
      geom_point(data = e_up, aes(ci_hi, 0, color = "bounds"), size = 3)+
      geom_segment(data = e_up, aes(x = ci_lo, xend = ci_hi, y = 0, yend = 0, color = "bounds"))+
      geom_point(data = emp, aes(GHI_hat_up, pdf_GHI_hat_up, color = "fitted"), shape = 17, size = 2)
  } else if (type == "dw") {
    pdf_plot <- pdf_plot+
      geom_point(data = emp, aes(GHI_hat_dw, pdf_GHI_hat_dw, color = "fitted"), shape = 17, size = 2)+
      geom_point(data = e_dw, aes(e_GHI, pdf_e_GHI, color = "expected_value"), size = 3)+
      geom_segment(data = e_dw, aes(x = ci_lo, xend = ci_lo, y = 0, yend = pdf_ci_lo, color = "bounds"), linewidth = 0.5)+
      geom_segment(data = e_dw, aes(x = ci_hi, xend = ci_hi, y = 0, yend = pdf_ci_hi, color = "bounds"), linewidth = 0.5)+
      geom_point(data = e_dw, aes(ci_lo, 0, color = "bounds"), size = 3)+
      geom_point(data = e_dw, aes(ci_hi, 0, color = "bounds"), size = 3)+
      geom_point(data = emp, aes(GHI_hat_dw, pdf_GHI_hat_dw, color = "fitted"), shape = 17, size = 2)+
      geom_segment(data = e_dw, aes(x = ci_lo, xend = ci_hi, y = 0, yend = 0, color = "bounds"))
  }
  type <- ifelse(type == "mix", "Mixture", ifelse(type == "up", "Sunny", "Cloudy"))

  # Add extra elements
  pdf_plot+
    geom_point(data = emp, aes(GHI_bar, 0, color = "seasonal"), size = 3)+
    theme_bw()+
    labs(color = NULL, y = NULL, x = NULL, subtitle = paste0("Forecast: ", date, " (", type, ")"))+
    scale_color_manual(values = vals_colors, labels = vals_labels)+
    # theme(legend.position = "top")+
    theme(legend.position = "top",
          # legend.justification = c("right", "top"),
          #legend.box.just = "right",
          legend.text.align = 1,
          legend.text = element_text(size = 15),
          legend.margin = margin(6, 6, 6),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.subtitle = element_text(size = 24),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.ticks = element_line(linewidth = 0.2),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "black", linetype = "dashed", linewidth = 0.1),
          panel.grid.major.x = element_blank())
}
