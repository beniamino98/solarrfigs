fig_options_hedged_payoff <- function(model, params, exact_daily_premium = FALSE, subtitle = NULL){

  tick = params$tick
  efficiency = params$efficiency
  n_panels = params$n_panels
  pun <- params$pun
  n_contracts = params$n_contracts
  nyear <- params$nyear

  if (exact_daily_premium){
    df_month_day <- select(model$payoffs$sim$Qr$payoff_month_day, Month, Day, premium)
    df_hedged_year <- model$payoffs$hist$payoff %>%
      left_join(df_month_day, by = c("Month", "Day")) %>%
      left_join(select(model$seasonal_data, Month, Day, GHI_bar), by = c("Month", "Day"))
  } else {
    df_month_day <- select(model$payoffs$sim$Qr$payoff_month, Month, premium = "daily_premium")
    df_hedged_year <- model$payoffs$hist$payoff %>%
      left_join(df_month_day, by = c("Month")) %>%
      left_join(select(model$seasonal_data, Month, Day, GHI_bar), by = c("Month", "Day"))
  }

  df_hedged_year <- df_hedged_year %>%
    filter(Year == nyear) %>%
    mutate(unhedged = pun*n_panels*efficiency*GHI,
           hedged = unhedged + tick*n_contracts*(payoff - premium),
           mean_prod = pun*n_panels*efficiency*GHI_bar,
           col = ifelse(hedged > unhedged, "up", "dw"))

  ggplot(df_hedged_year)+
    geom_segment(aes(x = n, xend = n, y = unhedged, yend = hedged, color = col), size = 1, alpha = 0.5)+
    geom_point(aes(n, hedged), color = "black", size = 1.5)+
    geom_point(aes(n, hedged, color = col), size = 1)+
    geom_point(aes(n, unhedged), color = "black", size = 1)+
    geom_line(aes(n, mean_prod), color = "blue")+
    scale_color_manual(values = c(up = "green", dw = "red"))+
    scale_y_continuous(breaks = seq(min(df_hedged_year$unhedged), max(df_hedged_year$unhedged), length.out = 5),
                       labels = format(seq(min(df_hedged_year$unhedged), max(df_hedged_year$unhedged), length.out = 5), digits = 2))+
    labs(x = NULL, y = NULL, subtitle = subtitle)+
    theme_bw()+
    scale_x_continuous(breaks = c(1, 92, 183, 274, 365))+
    theme(legend.position = "none",
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          plot.subtitle = element_text(size = 24),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.ticks = element_line(linewidth = 0.2),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "black", linetype = "dashed", linewidth = 0.1),
          panel.grid.major.x = element_blank())
}