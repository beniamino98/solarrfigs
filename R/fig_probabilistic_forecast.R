#' Plot a probabilistic forecast
#'
#' @param model par
#' @param date par
#' @param ci par
#' @param type par
#' @param unconditional par
#'
#' @examples
#' model <- Bologna$model
#' fig_probabilistic_forecast(model)
#'
#' @rdname fig_probabilistic_forecast
#' @name fig_probabilistic_forecast
#' @export
fig_probabilistic_forecast <- function(model, date = "2021-05-29", ci = 0.1, type = "mix", unconditional = FALSE){

  object <- solarModel_forecaster(model, date = date, ci = ci, unconditional = unconditional)

  grid <- object$grid
  emp <- object$emp
  type <- match.arg(type, choices = c("mix", "up", "dw"))
  vals_colors <- c(realized = "black", expected_value = "orange",seasonal = "red", bounds = "purple", fitted = "magenta")
  vals_labels <- c(realized = "Realized", expected_value = "Expectation", seasonal = "Seasonal", bounds = "Conf int", fitted = "Fitted")

  pdf_plot <-  ggplot()+
    # Mixture density
    geom_line(data = grid, aes(x, pdf_GHI))+
    # Components densities (weighted by prior probs)
    geom_line(data = grid, aes(x, pdf_GHI_up), color = "red")+
    geom_line(data = grid, aes(x, pdf_GHI_dw), color = "green")

  if (type == "mix") {
    pdf_plot <- pdf_plot +
      geom_segment(data = emp, aes(x = ci_mix_lo, xend = ci_mix_lo, y = 0, yend = pdf_ci_mix_lo, color = "bounds"), linewidth = 0.5)+
      geom_segment(data = emp, aes(x = ci_mix_hi, xend = ci_mix_hi, y = 0, yend = pdf_ci_mix_hi, color = "bounds"), linewidth = 0.5)+
      geom_segment(data = emp, aes(x = ci_mix_lo, xend = ci_mix_hi, y = 0, yend = 0, color = "bounds"))+
      geom_point(data = emp, aes(ci_mix_lo, 0, color = "bounds"), size = 3)+
      geom_point(data = emp, aes(ci_mix_hi, 0, color = "bounds"), size = 3)+
      geom_point(data = emp, aes(e_GHI, 0, color = "fitted"), shape = 17, size = 2)+
      geom_point(data = emp, aes(e_Rt, 0, color = "expected_value"), size = 3)


  } else if (type == "up") {
    pdf_plot <- pdf_plot +
      geom_segment(data = emp, aes(x = ci_up_lo, xend = ci_up_lo, y = 0, yend = pdf_ci_up_lo, color = "bounds"), linewidth = 0.5)+
      geom_segment(data = emp, aes(x = ci_up_hi, xend = ci_up_hi, y = 0, yend = pdf_ci_up_hi, color = "bounds"), linewidth = 0.5)+
      geom_segment(data = emp, aes(x = ci_up_lo, xend = ci_up_hi, y = 0, yend = 0, color = "bounds"))+
      geom_point(data = emp, aes(ci_up_lo, 0, color = "bounds"), size = 3)+
      geom_point(data = emp, aes(ci_up_hi, 0, color = "bounds"), size = 3)+
      geom_point(data = emp, aes(e_GHI_up, 0, color = "fitted"), shape = 17, size = 2)+
      geom_point(data = emp, aes(e_Rt_up, 0, color = "expected_value"), size = 3)

  } else if (type == "dw") {
    pdf_plot <- pdf_plot +
      geom_segment(data = emp, aes(x = ci_dw_lo, xend = ci_dw_lo, y = 0, yend = pdf_ci_dw_lo, color = "bounds"), linewidth = 0.5)+
      geom_segment(data = emp, aes(x = ci_dw_hi, xend = ci_dw_hi, y = 0, yend = pdf_ci_dw_hi, color = "bounds"), linewidth = 0.5)+
      geom_segment(data = emp, aes(x = ci_dw_lo, xend = ci_dw_hi, y = 0, yend = 0, color = "bounds"))+
      geom_point(data = emp, aes(ci_dw_lo, 0, color = "bounds"), size = 3)+
      geom_point(data = emp, aes(ci_dw_hi, 0, color = "bounds"), size = 3)+
      geom_point(data = emp, aes(e_GHI_dw, 0, color = "fitted"), shape = 17, size = 2)+
      geom_point(data = emp, aes(e_Rt_dw, 0, color = "expected_value"), size = 3)
  }
  type <- ifelse(type == "mix", "Mixture", ifelse(type == "up", "Sunny", "Cloudy"))
  plot_title <- ifelse(unconditional, "Unconditional", "Conditional")
  plot_title <- paste0(plot_title, " forecast: ", date, " (", type, ")")
  pdf_plot+
    geom_point(data = emp, aes(GHI, 0, color = "realized"), size = 3)+
    theme_bw()+
    theme(legend.position = "top")+
    labs(color = NULL, y = NULL, x = NULL, title = plot_title)+
    scale_color_manual(values = vals_colors, labels = vals_labels)
}
