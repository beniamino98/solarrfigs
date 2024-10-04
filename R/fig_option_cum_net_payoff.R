#' @rdname fig_option_exercise_month
#' @examples
#' fig_option_cum_net_payoff(Berlino$model, type = "sim")
#' fig_option_cum_net_payoff(Berlino$model, type = "model")
#' fig_option_cum_net_payoff(Oslo$model, type = "model")
#' fig_option_cum_net_payoff(Oslo$model, type = "sim")
#' fig_option_cum_net_payoff(Amsterdam$model, type = "sim")
#' fig_option_cum_net_payoff(Amsterdam$model, type = "model")
#' fig_option_cum_net_payoff(Palermo$model, type = "sim")
#' fig_option_cum_net_payoff(Palermo$model, type = "model")
#' fig_option_cum_net_payoff(Bologna$model, type = "model")
#' fig_option_cum_net_payoff(Bologna$model, type = "sim")
#' fig_option_cum_net_payoff(Roma$model, type = "sim")
#' fig_option_cum_net_payoff(Roma$model, type = "model")

fig_option_cum_net_payoff <- function(payoff, type = "scenarios", put = TRUE, tick = 1, exact_daily_premium = FALSE, subtitle = NULL){

  type <- match.arg(type, choices = c(scenarios = "scenarios", model = "model"))
  option_type = ifelse(put, "put", "call")
  payoff <- solarOption_structure(payoff, type = type, put = put, exact_daily_premium = exact_daily_premium)

  df_cum <- na.omit(payoff[[option_type]][[type]]$structured$payoff_cum)
  df_cum_year <- dplyr::filter(df_cum, Year == 2011)
  df_year <- payoff[[option_type]][[type]]$structured$payoff_year
  benchmark_price <- payoff[[option_type]]$historical$payoff_year$premium

  # Y-axis
  min_y <- min(df_cum$cum_net_payoff, na.rm = TRUE)
  max_y <- max(df_cum$cum_net_payoff, na.rm = TRUE)
  y_breaks <- c(min_y, df_year$premium_Qup, df_year$premium_Q, df_year$premium*0.97, df_year$premium_Qdw, max_y)
  y_breaks <- y_breaks[order(y_breaks)]
  y_labels <- paste0(round(y_breaks, digits = 0), " €")
  # Secondary Axis (rates)
  y_breaks_sec.axis <- (y_breaks/benchmark_price - 1)
  y_labels_sec.axis <- paste0(format(-y_breaks_sec.axis*100, digits = 1, scientific = FALSE), "%")

  # Cumulated payoff
  ggplot()+
    geom_line(data = df_cum, aes(n, tick*cum_net_payoff, group = Year), alpha = 0.3, size = 0.2)+
    geom_line(data = df_cum_year, aes(n, tick*e_cum_net_payoff_P, group = Year), alpha = 0.8, color = "blue")+
    geom_line(data = df_cum_year, aes(n, tick*e_cum_net_payoff_Q, group = Year), alpha = 0.8, color = "orange")+
    geom_line(data = df_cum_year, aes(n, tick*e_cum_net_payoff_Qup, group = Year), alpha = 1, color = "red")+
    geom_line(data = df_cum_year, aes(n, tick*e_cum_net_payoff_Qdw, group = Year), alpha = 1, color = "green")+
    geom_ribbon(data = df_cum, aes(n, ymin = e_cum_net_payoff, ymax = Inf), alpha = 0.05, fill = "red") +
    geom_ribbon(data = df_cum, aes(n, ymax = e_cum_net_payoff, ymin = -Inf), alpha = 0.05, fill = "green") +
    #geom_line(data = df_cum_year, aes(n, tick*e_cum_net_payoff, group = Year), alpha =  0.8, color = "purple", linetype = "dashed")+
    #geom_line(data = df_cum_year, aes(n, tick*e_cum_net_payoff_disc, group = Year), alpha = 1, color = "blue", linetype = "dashed")+
    theme_bw()+
    scale_x_continuous(breaks = c(1, 92, 183, 274, 365))+
    scale_y_continuous(breaks = y_breaks, labels = y_labels,
                       sec.axis = sec_axis(~./benchmark_price - 1,
                                           breaks = y_breaks_sec.axis,
                                           labels = y_labels_sec.axis,
                                           name = "Expected Return"))+
    labs(x = "Day of the year", y = "Option price", subtitle = subtitle)+
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

