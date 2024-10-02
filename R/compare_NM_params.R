compare_NM_params <- function(model_A, model_B){

  plot_mu <- ggplot()+
    geom_line(data = model_A$NM_model, aes(Month, mu_up, color = "A"))+
    geom_line(data = model_A$NM_model, aes(Month, mu_dw, color = "A"), linetype = "dashed")
  plot_sd <- ggplot()+
    geom_line(data = model_A$NM_model, aes(Month, sd_up, color = "A"))+
    geom_line(data = model_A$NM_model, aes(Month, sd_dw, color = "A"), linetype = "dashed")
  plot_p_up <- ggplot()+
    geom_line(data = model_A$NM_model, aes(Month, p_up, color = "A"))

  if (!missing(model_B)){
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
