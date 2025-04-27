#' @rdname fig_option_cum_premium
#' @examples
#' payoffs <- SampleData$payoffs
#' fig_option_cum_premium(payoffs, put = FALSE, type = "scenarios")
#' @export
fig_option_cum_premium <- function(payoffs, nmonths = 1:12, put = TRUE, type = "scenarios", tick = 1, exact_daily_premium = FALSE, subtitle = NULL){

  type <- match.arg(type, choices = c(scenarios = "scenarios", model = "model"))
  option_type = ifelse(put, "put", "call")
  payoffs <- solarOption_structure(payoffs, type, put, exact_daily_premium = exact_daily_premium)
  df_month_day <- dplyr::filter(payoffs[[option_type]][[type]]$structured$payoff_cum, Year == 2011, Month %in% nmonths)
  r <- (1 + 0.03)^(nrow(df_month_day)/365) - 1

  # Y-axis
  min_y <- min(cumsum(df_month_day$premium), na.rm = TRUE)
  max_y <- max(cumsum(df_month_day$premium), na.rm = TRUE)
  y_breaks <- seq(min_y, max_y, length.out = 5)
  y_labels <- paste0(round(y_breaks, digits = 0), " €")

  df_month_day %>%
    ggplot()+
    geom_line(aes(n, cumsum(premium)), size = 1.1)+
    geom_line(aes(n, cumsum(premium_P)), color = "blue")+
    #geom_line(aes(n, cumsum(premium_Qr)), color = "magenta")+
    #geom_line(aes(n, cumsum(premium)*model$payoffs$control$B(nrow(df_month_day)/365)), color = "blue", linetype = "dashed")+
    geom_line(aes(n, cumsum(premium_Q)), color = "orange")+
    geom_line(aes(n, cumsum(premium_Qup)), color = "red")+
    geom_line(aes(n, cumsum(premium_Qdw)), color = "red")+
    theme_bw()+
    scale_x_continuous(breaks = c(1, 92, 183, 274, 365))+
    scale_y_continuous(breaks = y_breaks, labels = y_labels)+
    labs(x = "Day of the year", y = "Option price", subtitle = subtitle)+
    theme(legend.position = "none")
}
