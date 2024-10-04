#' @rdname fig_option_exercise_month
#' @examples
#' fig_option_exercise_month(Oslo$model)
#' fig_option_exercise_month(Oslo$model, type = "model")
#' fig_option_exercise_month(Bologna$model)
#' fig_option_exercise_month(Bologna$model, type = "model")
#' fig_option_exercise_month(Palermo$model)
#' fig_option_exercise_month(Palermo$model, type = "model")
fig_option_exercise_month <- function(payoff, type = "sim", limits = c(0.27, 0.58), subtitle = NULL){

  type <- match.arg(type, choices = c(simulation = "sim", model = "mod"))

  payoff[[type]]$structured$payoff_month %>%
    ggplot()+
    geom_line(aes(Month, exercise), size = 1.2)+
    geom_line(aes(Month, exercise_P), color = "blue")+
    geom_line(aes(Month, exercise_Q), color = "orange")+
    # geom_line(aes(Month, exercise_Qr), color = "magenta")+
    geom_line(aes(Month, exercise_Qdw), color = "red", linetype = "dashed")+
    geom_line(aes(Month, exercise_Qup), color = "green", linetype = "dashed")+
    theme_bw()+
    scale_x_continuous(breaks = seq(2, 12, 2), labels = lubridate::month(seq(2, 12, 2), label = TRUE))+
    scale_y_continuous(breaks = seq(0.1, 0.9, 0.05), limits = limits,
                       labels = paste0(round(seq(0.1, 0.9, 0.05)*100, 2), "%"))+
    labs(x = "Month", y = "Probability of exercise", subtitle = subtitle)+
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
