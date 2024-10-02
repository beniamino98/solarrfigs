#' @rdname fig_option_cum_premium
#' @examples
#' fig_option_cum_premium(Bologna$model)
#' fig_option_cum_premium(Oslo$model, nmonths = 10:12)
#' fig_option_cum_premium(Oslo$model, type = "model")
#' fig_option_cum_premium(Amsterdam$model)

fig_option_cum_premium <- function(model, nmonths = 1:12, type = "sim", tick = 1, exact_daily_premium = FALSE, subtitle = NULL){

  type <- match.arg(type, choices = c(simulation = "sim", model = "model"))
  model <- solar_option_payoff_structure(model, type = type, exact_daily_premium = exact_daily_premium)
  filter(model$payoffs[[type]]$structured$payoff_cum, Year == 2011)
  df_month_day <- na.omit(model$payoffs[[type]]$structured$payoff_month_day)
  df_month_day <- filter(model$payoffs[[type]]$structured$payoff_cum, Year == 2011)
  r <- (1 + 0.03)^(nrow(df_month_day)/365) - 1
  df_month_day <- df_month_day %>%
    filter(Month %in% nmonths)

  # Y-axis
  min_y <- min(cumsum(df_month_day$premium), na.rm = TRUE)
  max_y <- max(cumsum(df_month_day$premium), na.rm = TRUE)
  y_breaks <- seq(min_y, max_y, length.out = 5)
  y_labels <- paste0(round(y_breaks, digits = 0), " €")

  df_month_day %>%
    ggplot()+
    geom_line(aes(n, cumsum(premium)), size = 1.1)+
    geom_line(aes(n, cumsum(premium_P)), color = "blue")+
    geom_line(aes(n, cumsum(premium_Qr)), color = "magenta")+
    geom_line(aes(n, cumsum(premium)*model$payoffs$control$B(nrow(df_month_day)/365)), color = "blue", linetype = "dashed")+
    geom_line(aes(n, cumsum(premium_Q)), color = "orange")+
    geom_line(aes(n, cumsum(premium_Qup)), color = "red")+
    geom_line(aes(n, cumsum(premium_Qdw)), color = "red")+
    theme_bw()+
    scale_x_continuous(breaks = c(1, 92, 183, 274, 365))+
    scale_y_continuous(breaks = y_breaks, labels = y_labels)+
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