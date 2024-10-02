create_df_hedged_year <- function(model, params, exact_daily_premium = FALSE){
  
  tick = params$tick
  efficiency = params$efficiency
  n_panels = params$n_panels
  pun <- params$pun
  n_contracts = params$n_contracts
  nyear <- params$nyear
  
  # Extract payoffs
  payoffs <- model$payoffs
  # Extract seasonal data
  df_seasonal <- dplyr::select(model$seasonal_data, Month, Day, GHI_bar, Ct)
  
  # Use exactly daily premium 
  premium <- payoffs$sim$Qr
  if (exact_daily_premium) {
    df_daily_premium <- dplyr::select(premium$payoff_month_day, Month, Day, premium)
    df_hedged_year <- dplyr::left_join(payoffs$hist$payoff, df_daily_premium, by = c("Month", "Day")) 
  } else {
    df_daily_premium <- dplyr::select(premium$payoff_month, Month, premium = "daily_premium")
    df_hedged_year <- dplyr::left_join(payoffs$hist$payoff, df_daily_premium, by = c("Month")) 
  }
  # Create the dataset with premium and seasonal data 
  df_hedged_year <- df_hedged_year %>%
    dplyr::left_join(df_seasonal, by = c("Month", "Day")) %>%
    dplyr::filter(Year == nyear) %>%
    dplyr::mutate(unhedged = pun*n_panels*efficiency*GHI,
                  hedged = unhedged + tick*n_contracts*(payoff - premium),
                  mean_prod = pun*n_panels*efficiency*GHI_bar,
                  Ct = pun*n_panels*efficiency*Ct,
                  col = ifelse(unhedged > mean_prod, "up", "dw"))
  return(df_hedged_year)
}


fig_cashflows_hedged_vs_unhedged <- function(df_hedged_year){
  
  y_limits <- c(min(min(df_hedged_year$unhedged), min(df_hedged_year$hedged)), max(df_hedged_year$Ct))
  fig_unhedged <- df_hedged_year %>%
    mutate(type = "Unhedged") %>%
    ggplot() +
    geom_line(aes(date, unhedged), color = "orange") +
    geom_line(aes(date, Ct), color = "blue") +
    geom_line(aes(date, mean_prod), color = "black") +
    geom_point(aes(date, unhedged), color = "black", size = 1.5) +
    geom_point(aes(date, unhedged, color = col), size = 1) +
    facet_wrap(~type) +
    scale_color_manual(values = c(up = "green", dw = "red")) +
    scale_y_continuous(limits = y_limits)+
    scale_x_date(date_labels = "%b")+
    labs(x = NULL, y = "Cash Flows (Eur)")+
    theme_bw()+
    theme(legend.position = "none",
          strip.background = element_rect(colour = "black", fill = "white"),
          plot.background = element_blank())
  fig_hedged <- df_hedged_year %>%
    mutate(type = "Hedged") %>%
    ggplot() +
    geom_line(aes(date, hedged), color = "orange") +
    geom_line(aes(date, Ct), color = "blue") +
    geom_line(aes(date, mean_prod), color = "black") +
    geom_point(aes(date, hedged), color = "black", size = 1.5) +
    geom_point(aes(date, hedged, color = col), size = 1) +
    facet_wrap(~type) +
    scale_color_manual(values = c(up = "green", dw = "red"),
                       labels = c(up = "GHI above seasonal", dw = "GHI below seasonal")) +
    scale_y_continuous(limits = y_limits)+
    scale_x_date(date_labels = "%b")+
    labs(x = NULL, y = NULL, color = NULL)+
    theme_bw()+
    theme(legend.position = "none", # c(.999, .99),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.key.size = unit(0.1, "cm"),
          legend.margin = margin(rep(4, 4)),
          strip.background = element_rect(colour = "black", fill = "white"),
          plot.background = element_blank())
  gridExtra::grid.arrange(fig_unhedged, fig_hedged, ncol = 2)
}

fig_cashflows_compare_moments <- function(df_hedged_year){
  
  df_month <- df_hedged_year %>%
    group_by(Month) %>%
    mutate(Month = factor(Month, levels = 1:12, labels = lubridate::month(1:12, label = TRUE), ordered = TRUE)) %>%
    summarise(e_uh = mean(unhedged), e_h = mean(hedged),
              sd_uh = sd(unhedged), sd_h = sd(hedged))
  
  mean(df_month$sd_h/df_month$e_h)
  mean(df_month$sd_uh/df_month$e_uh)
  
  plot_expectation <- ggplot(df_month)+
    geom_line(aes(Month, e_uh, group = 1, color = "unhedged"))+
    geom_line(aes(Month, e_h, group = 1, color = "hedged"))+ 
    geom_line(aes(Month, e_h + sd_uh, group = 1, color = "hedged"), linetype = "dashed")+
    geom_line(aes(Month, e_h - sd_uh, group = 1, color = "hedged"), linetype = "dashed")+
    labs(x = NULL, y = "Expectation (Eur)")+
    scale_color_manual(values = c(unhedged = "black", hedged = "red")) +
    theme_bw()+
    theme(legend.position = "none")
  
  plot_std_dev <- ggplot(df_month)+
    geom_line(aes(Month, sd_uh, group = 1, color = "unhedged"))+
    geom_line(aes(Month, sd_h, group = 1, color = "hedged"))+ 
    labs(x = NULL, y = "Std. deviation (Eur)", color = NULL)+
    scale_color_manual(values = c(unhedged = "black", hedged = "red")) +
    theme_bw()+
    theme(legend.position = "none")
  
  gridExtra::grid.arrange(plot_expectation, plot_std_dev, ncol = 2, widths = c(0.4, 0.6))
}
