#' Visually compare the Gaussian mixture parameters between two models
#'
#' @param model_A first model
#' @param model_B second model
#'
#' @rdname fig_compare_mixture_parameters
#' @name fig_compare_mixture_parameters
#' @export
fig_compare_mixture_parameters <- function(model_A, model_B){

  plot_mu <- ggplot()+
    geom_line(data = model_A$NM_model, aes(Month, mu_up, color = "A"))+
    geom_line(data = model_A$NM_model, aes(Month, mu_dw, color = "A"), linetype = "dashed")+
    theme_bw()+
    theme(legend.position = "top")+
    labs(color = "Model: ", x = NULL, y = "Mean")

  plot_sd <- ggplot()+
    geom_line(data = model_A$NM_model, aes(Month, sd_up, color = "A"))+
    geom_line(data = model_A$NM_model, aes(Month, sd_dw, color = "A"), linetype = "dashed")+
    theme_bw()+
    theme(legend.position = "none")+
    labs(x = NULL,  y = "Std. deviation")

  plot_p_up <- ggplot()+
    geom_line(data = model_A$NM_model, aes(Month, p_up, color = "A"))+
    theme_bw()+
    theme(legend.position = "none")+
    labs(x = "Month", y = "Probability (up)")

  if (!missing(model_B)) {
    plot_mu <- plot_mu +
      geom_line(data = model_B$NM_model, aes(Month, mu_up, color = "B"))+
      geom_line(data = model_B$NM_model, aes(Month, mu_dw, color = "B"), linetype = "dashed")
    plot_sd <- plot_sd +
      geom_line(data = model_B$NM_model, aes(Month, sd_up, color = "B"))+
      geom_line(data = model_B$NM_model, aes(Month, sd_dw, color = "B"), linetype = "dashed")
    plot_p_up <- plot_p_up+
      geom_line(data = model_B$NM_model, aes(Month, p_up, color = "B"))
  }
  gridExtra::grid.arrange(plot_mu, plot_sd, plot_p_up)
}
