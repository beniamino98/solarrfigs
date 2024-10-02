#' @rdname fig_option_premium_month
#' @examples
#' fig_option_premium_month(Bologna$model, type = "sim")
#' fig_option_premium_month(Bologna$model, type = "model")
#' fig_option_premium_month(Oslo$model, type = "sim")
#' fig_option_premium_month(Oslo$model, type = "model")
#' fig_option_premium_month(Palermo$model, type = "sim")
#' fig_option_premium_month(Palermo$model, type = "model")

fig_option_premium_month <- function(model, type = "sim", tick = 1, subtitle = NULL){
  
  type <- match.arg(type, choices = c(simulation = "sim", model = "model"))
  
  model$payoffs[[type]]$structured$payoff_month %>%
    ggplot()+
    geom_line(aes(Month, tick*premium), size = 1.2)+
    geom_line(aes(Month, tick*premium_Qdw), color = "red", linetype = "dashed")+
    geom_line(aes(Month, tick*premium_Qup), color = "green", linetype = "dashed")+
    geom_line(aes(Month, tick*premium_P), color = "blue")+
    geom_line(aes(Month, tick*premium_Q), color = "orange")+
    geom_line(aes(Month, tick*premium_Qr), color = "magenta")+
    theme_bw()+
    scale_y_continuous(breaks = seq(5, 100, 5), labels = paste0(round(seq(5, 100, 5), 2), " €"))+
    scale_x_continuous(breaks = seq(2, 12, 2), labels = lubridate::month(seq(2, 12, 2), label = TRUE))+
    labs(x = "Month", y = "Option price", subtitle = subtitle)+
    theme(legend.position = "none",
          plot.subtitle = element_text(size = 24),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15, angle = 0),
          axis.ticks = element_line(linewidth = 0.4),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "black", linetype = "dashed", linewidth = 0.1),
          panel.grid.major.x = element_blank())
}