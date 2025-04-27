#' Plot the cumulated net payoff
#'
#' @rdname fig_option_cum_net_payoff
#' @name fig_option_cum_net_payoff
#' @examples
#' payoffs <- SampleData$payoffs
#' fig_option_cum_net_payoff(payoffs, type = "model", put = TRUE)
#' fig_option_cum_net_payoff(payoffs, type = "scenarios", put = TRUE)
#' fig_option_cum_net_payoff(payoffs, type = "model", put = FALSE)
#' fig_option_cum_net_payoff(payoffs, type = "scenarios", put = FALSE)
#' @export
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


solarOption_structure_return <- function(payoffs, type = "model", put = TRUE, exact_daily_premium = TRUE){

  option_type <- ifelse(put, "put", "call")
  type <- match.arg(type, choices = c("scenarios", "model"))
  payoff <- payoffs[[option_type]][c("historical", type)]

  # Yearly premiums and avg. exercise probabilities under different prob. measures
  df_year <- dplyr::tibble(
    side =  payoff$historical$payoff_year$side,
    ndays =  payoff$historical$payoff_year$ndays,
    # Premiums for the option
    premium = payoff$historical$payoff_year$premium,
    premium_P = payoff[[type]]$P$payoff_year$premium,
    premium_Q = payoff[[type]]$Q$payoff_year$premium,
    premium_Qdw = payoff[[type]]$Qdw$payoff_year$premium,
    premium_Qup = payoff[[type]]$Qup$payoff_year$premium,
    premium_Qr = payoff[[type]]$Qr$payoff_year$premium,
    # Probabilities of exercise the option
    exercise = payoff$historical$payoff_year$exercise,
    exercise_P = payoff[[type]]$P$payoff_year$exercise,
    exercise_Q = payoff[[type]]$Q$payoff_year$exercise,
    exercise_Qup = payoff[[type]]$Qup$payoff_year$exercise,
    exercise_Qdw = payoff[[type]]$Qdw$payoff_year$exercise,
    exercise_Qr = payoff[[type]]$Qr$payoff_year$exercise,
  )

  # Monthly premiums and avg. exercise probabilities under different prob. measures
  df_month <- dplyr::tibble(
    Month = payoff$historical$payoff_month$Month,
    side = payoff$historical$payoff_month$side,
    n = payoff$historical$payoff_month$ndays,
    # Premiums for the option
    premium = payoff$historical$payoff_month$premium,
    premium_P = payoff[[type]]$P$payoff_month$premium,
    premium_Q = payoff[[type]]$Q$payoff_month$premium,
    premium_Qup = payoff[[type]]$Qup$payoff_month$premium,
    premium_Qdw = payoff[[type]]$Qdw$payoff_month$premium,
    premium_Qr = payoff[[type]]$Qdw$payoff_month$premium,
    # Probabilities of exercise the option
    exercise = payoff$historical$payoff_month$exercise,
    exercise_P = payoff[[type]]$P$payoff_month$exercise,
    exercise_Q = payoff[[type]]$Q$payoff_month$exercise,
    exercise_Qup = payoff[[type]]$Qup$payoff_month$exercise,
    exercise_Qdw = payoff[[type]]$Qdw$payoff_month$exercise,
    exercise_Qr = payoff[[type]]$Qr$payoff_month$exercise,
  )

  # Avg. daily premiums and avg. exercise probabilities under different prob. measures
  df_month_day_mean <- dplyr::tibble(
    Month = payoff$historical$payoff_month$Month,
    side = payoff$historical$payoff_month$side,
    n = payoff$historical$payoff_month$ndays,
    # Premiums for the option
    premium = payoff$historical$payoff_month$daily_premium,
    premium_P = payoff[[type]]$P$payoff_month$daily_premium,
    premium_Q = payoff[[type]]$Q$payoff_month$daily_premium,
    premium_Qup = payoff[[type]]$Qup$payoff_month$daily_premium,
    premium_Qdw = payoff[[type]]$Qdw$payoff_month$daily_premium,
    premium_Qr = payoff[[type]]$Qr$payoff_month$daily_premium,
  )

  # Exact daily premiums and exact exercise probabilities under different prob. measures
  df_month_day <- dplyr::tibble(
    Month = payoff$historical$payoff_month_day$Month,
    Day = payoff$historical$payoff_month_day$Day,
    side = payoff$historical$payoff_month_day$side,
    # Premiums for the option
    premium = payoff$historical$payoff_month_day$premium,
    premium_P = payoff[[type]]$P$payoff_month_day$premium,
    premium_Q = payoff[[type]]$Q$payoff_month_day$premium,
    premium_Qup = payoff[[type]]$Qup$payoff_month_day$premium,
    premium_Qdw = payoff[[type]]$Qdw$payoff_month_day$premium,
    premium_Qr = payoff[[type]]$Qr$payoff_month_day$premium,
    # Probabilities of exercise the option
    exercise = payoff$historical$payoff_month_day$exercise,
    exercise_P = payoff[[type]]$P$payoff_month_day$exercise,
    exercise_Q = payoff[[type]]$Q$payoff_month_day$exercise,
    exercise_Qup = payoff[[type]]$Qup$payoff_month_day$exercise,
    exercise_Qdw = payoff[[type]]$Qdw$payoff_month_day$exercise,
    exercise_Qr = payoff[[type]]$Qr$payoff_month_day$exercise,
    n = payoff$hist$payoff_month_day$n
  )


  # Historical Daily Premiums
  df_payoff <- dplyr::select(payoff$historical$payoff, -exercise, -GHI, -strike)
  if (!exact_daily_premium) {
    df_payoff <- dplyr::left_join(df_payoff, dplyr::select(df_month_day_mean, Month, premium:premium_Qr), by = c("Month"))
  } else {
    df_payoff <- dplyr::left_join(df_payoff, dplyr::select(df_month_day, Month, Day, premium:premium_Qr), by = c("Month", "Day"))
  }
  # Compute the price of a contract for each day of the year as:
  # - Vt = sum(Remaining premiums till T) + sum(realized payoff till t - premium paid till t)
  j <- 1
  cumulated_payoff <- list()
  seq_years <- seq(min(df_payoff$Year), max(df_payoff$Year), 1)
  for (j in 1:length(seq_years)){
    df_cum <- dplyr::filter(df_payoff, Year == seq_years[j])
    # Remove the 29-02 for graphic purposes
    df_cum <- df_cum[paste0(df_cum$Month, "-", df_cum$Day) != "2-29",]
    df_cum <- dplyr::mutate(df_cum,
                            cum_payoff = NA,
                            cum_net_payoff_hist = NA,
                            cum_net_payoff_Qdw = NA,
                            cum_net_payoff_P = NA,
                            cum_net_payoff_Q = NA,
                            cum_net_payoff_Qr = NA,
                            cum_net_payoff_Qup = NA)
    ndays <- nrow(df_cum)
    for(i in 1:nrow(df_cum)){
      df_cum$cum_payoff[i] <- sum(df_cum$payoff[1:i])
      df_cum$cum_net_payoff_hist[i] <- df_cum$cum_payoff[i] - sum(df_cum$premium[1:i])
      df_cum$cum_net_payoff_P[i] <- df_cum$cum_payoff[i] - sum(df_cum$premium_P[1:i])
      df_cum$cum_net_payoff_Q[i] <- df_cum$cum_payoff[i] - sum(df_cum$premium_Q[1:i])
      df_cum$cum_net_payoff_Qr[i] <- df_cum$cum_payoff[i] - sum(df_cum$premium_Qr[1:i])
      df_cum$cum_net_payoff_Qdw[i] <- df_cum$cum_payoff[i] - sum(df_cum$premium_Qdw[1:i])
      df_cum$cum_net_payoff_Qup[i] <- df_cum$cum_payoff[i] - sum(df_cum$premium_Qup[1:i])
    }
    cumulated_payoff[[j]] <- df_cum
  }

  # Compute the expected trajectory for each day
  df_cum <- dplyr::bind_rows(cumulated_payoff) %>%
    mutate(cum_net_payoff = cum_net_payoff_Qr/(df_year$premium_Qr)) %>%
    dplyr::group_by(Month, Day) %>%
    dplyr::mutate(
      e_cum_net_payoff_hist = mean(cum_net_payoff_hist),
      e_cum_net_payoff_P = mean(cum_net_payoff_P/df_year$premium_P),
      e_cum_net_payoff_Qr = mean((cum_net_payoff_Qr)/(df_year$premium_Qr)),
      e_cum_net_payoff_Q = mean((cum_net_payoff_Q)/(df_year$premium_Q)),
      e_cum_net_payoff_Qdw = mean(cum_net_payoff_Qdw/(df_year$premium_Qdw)),
      e_cum_net_payoff_Qup = mean(cum_net_payoff_Qup/(df_year$premium_Qup)),
    ) %>%
    dplyr::ungroup()

  payoffs[[option_type]][[type]]$structured <- list(payoff = df_payoff,
                                                    payoff_year = df_year,
                                                    payoff_month = df_month,
                                                    payoff_month_day = df_month_day,
                                                    payoff_cum = df_cum)

  return(payoffs)
}


fig_option_cum_return <- function(payoffs, type = "scenarios", put = TRUE, tick = 1, exact_daily_premium = FALSE, subtitle = NULL){

  type <- match.arg(type, choices = c(scenarios = "scenarios", model = "model"))
  option_type = ifelse(put, "put", "call")
  payoff <- solarOption_structure_return(payoffs, type = type, put = put, exact_daily_premium = exact_daily_premium)

  df_cum <- na.omit(payoff[[option_type]][[type]]$structured$payoff_cum)
  df_cum_year <- dplyr::filter(df_cum, Year == 2011)
  df_year <- payoff[[option_type]][[type]]$structured$payoff_year
  benchmark_price <- payoff[[option_type]]$historical$payoff_year$premium

  # Y-axis
  min_y <- min(df_cum$cum_net_payoff, na.rm = TRUE)
  max_y <- max(df_cum$cum_net_payoff, na.rm = TRUE)
  # Build Y-breaks
  y_breaks <- c(min_y, tail(df_cum_year$e_cum_net_payoff_Qup, 1),
                tail(df_cum_year$e_cum_net_payoff_P, 1),
                tail(df_cum_year$e_cum_net_payoff_Q, 1),
                tail(df_cum_year$e_cum_net_payoff_Qr, 1),
                tail(df_cum_year$e_cum_net_payoff_Qdw, 1), max_y)
  y_breaks <- y_breaks[order(y_breaks)]
  y_labels <- paste0(round(y_breaks*100, digits = 2), " %")

  # Cumulated payoff
  ggplot()+
    geom_line(data = df_cum, aes(n, cum_net_payoff, group = Year), alpha = 0.3, size = 0.2)+
    geom_line(data = df_cum_year, aes(n, e_cum_net_payoff_P, group = Year), alpha = 0.8, color = "blue")+
    geom_line(data = df_cum_year, aes(n, e_cum_net_payoff_Q, group = Year), alpha = 0.8, color = "magenta")+
    geom_line(data = df_cum_year, aes(n, e_cum_net_payoff_Qr, group = Year), alpha = 0.8, color = "darkorange")+
    geom_line(data = df_cum_year, aes(n, e_cum_net_payoff_Qup, group = Year), alpha = 1, color = "red")+
    geom_line(data = df_cum_year, aes(n, e_cum_net_payoff_Qdw, group = Year), alpha = 1, color = "green")+
    geom_ribbon(data = df_cum, aes(n, ymin = e_cum_net_payoff_Qr, ymax = Inf), alpha = 0.05, fill = "red") +
    geom_ribbon(data = df_cum, aes(n, ymax = e_cum_net_payoff_Qr, ymin = -Inf), alpha = 0.05, fill = "green") +
    theme_bw()+
    scale_x_continuous(breaks = c(1, 92, 183, 274, 365))+
    scale_y_continuous(breaks = y_breaks, labels = y_labels)+
    labs(x = "Day of the year", y = "Return at maturity", subtitle = subtitle)+
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

