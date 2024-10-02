fig_options_hedged_payoff_year <- function(model, params, exact_daily_premium = FALSE, subtitle = NULL){

  tick = params$tick
  efficiency = params$efficiency
  n_panels = params$n_panels
  pun <- params$pun
  n_contracts = params$n_contracts
  nyear <- params$nyear
  type <- params$type
  premium <- params$premium

  if (exact_daily_premium){
    df_month_day <- select(model$payoffs$sim$Qr$payoff_month_day, Month, Day, premium)
    df_hedged <- model$payoffs$hist$payoff %>%
      left_join(df_month_day, by = c("Month", "Day")) %>%
      left_join(select(model$seasonal_data, Month, Day, GHI_bar), by = c("Month", "Day"))
  } else {
    df_month_day <- select(model$payoffs$sim$Qr$payoff_month, Month, premium = "daily_premium")
    df_hedged <- model$payoffs$hist$payoff %>%
      left_join(df_month_day, by = c("Month")) %>%
      left_join(select(model$seasonal_data, Month, Day, GHI_bar), by = c("Month", "Day"))
  }

  df_hedged <- df_hedged %>%
    filter(Year < nyear) %>%
    mutate(unhedged = pun*n_panels*efficiency*GHI,
           hedged = unhedged + tick*n_contracts*(payoff - premium),
           mean_prod = pun*n_panels*efficiency*GHI_bar,
           col = ifelse(hedged > unhedged, "up", "dw")) %>%
    group_by(Year) %>%
    summarise(hedged = sum(hedged), unhedged = sum(unhedged))

  df <- df_hedged
  min_x <- min(c(min(df$hedged), min(df$unhedged)))
  min_x <- min_x - min_x*0.08
  max_x <- max(c(max(df$hedged), max(df$unhedged)))
  max_x <- max_x + max_x*0.08
  ker_h <- density(df$hedged, from = min_x, to = max_x)
  #ker_h$y <- ker_h$y/sum(ker_h$y)
  p_h <- dnorm(ker_h$x, mean = mean(df$hedged), sd = sd(df$hedged))
  #p_h <- p_h/sum(p_h)
  ker_uh <- density(df$unhedged, from = min_x, to = max_x)
  #ker_uh$y <- ker_uh$y/sum(ker_uh$y)
  p_uh <- dnorm(ker_uh$x, mean = mean(df$unhedged), sd = sd(df$unhedged))
  #p_uh <- p_uh/sum(p_uh)

  ggplot()+
    #geom_line(aes(ker_h$x, p_h), color = "red", linetype = "dotted")+
    geom_line(aes(ker_uh$x, ker_uh$y/sum(ker_uh$y)), color = "black") +
    #geom_histogram(aes(df_hedged$unhedged, after_stat(density)), bins = 30, color = "black", fill = "gray")+
    geom_line(aes(ker_h$x, ker_h$y/sum(ker_h$y)), color = "red", linetype = "solid") +
    #geom_line(aes(ker_uh$x, p_uh), color = "black", linetype = "dotted")+
    theme_bw()+
    labs(x = NULL, y = NULL, subtitle = subtitle)+
    scale_x_continuous(breaks = seq(min_x, max_x, length.out = 3))+
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

