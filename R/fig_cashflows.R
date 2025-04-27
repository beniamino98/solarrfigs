#' Create a dataset that can be used to plot hedged vs unhedged cashflows
#'
#' @examples
#' payoff <- SampleData$payoffs
#' model <- SampleData$model
#' params <- solarOption_contracts(payoff)
#' df_hedged <- data_hedged_payoff(payoff, model, params)
#' @export
data_hedged_payoff <- function(payoff, model, params, put = TRUE, type = "model", premium = "P", exact_daily_premium = FALSE){

  # hedging parameters
  tick = params$tick
  efficiency = params$efficiency
  n_panels = params$n_panels
  pun <- params$pun
  n_contracts = params$n_contracts
  nyear <- params$nyear
  # Match option type
  option_type <- ifelse(put, "put", "call")
  # Match computation type
  type = match.arg(type, choices = c("model", "scenarios"))

  # Historical payoff
  payoff_hist <- payoff[[option_type]]$historical$payoff
  df_payoff <- payoff[[option_type]][[type]]
  # Match premium type
  premium <- match.arg(premium, choices = names(df_payoff))
  if (exact_daily_premium) {
    df_month_day <- dplyr::select(df_payoff[[premium]]$payoff_month_day, Month, Day, premium)
    df_hedged_year <- dplyr::left_join(payoff_hist, df_month_day, by = c("Month", "Day"))
  } else {
    df_month_day <- dplyr::select(df_payoff[[premium]]$payoff_month, Month, premium = "daily_premium")
    df_hedged_year <- dplyr::left_join(payoff_hist, df_month_day, by = c("Month"))
  }

  # Add seasonal mean
  df_hedged <- dplyr::left_join(df_hedged_year,
                   dplyr::select(model$seasonal_data, Month, Day, n, GHI_bar, Ct),
                   by = c("Month", "Day", "n")) %>%
    dplyr::mutate(unhedged = pun*n_panels*efficiency*GHI,
                  hedged = unhedged + tick*n_contracts*(payoff - premium),
                  mean_prod = pun*n_panels*efficiency*GHI_bar,
                  Ct = pun*n_panels*efficiency*Ct,
                  col = ifelse(hedged > unhedged, "up", "dw"))

  attr(df_hedged, "nyear") <- nyear
  return(df_hedged)
}


#' Create a dataset that can be used to plot hedged vs unhedged cashflows
#'
#' @examples
#' payoff <- SampleData$payoffs
#' model <- SampleData$model
#' params <- solarOption_contracts(payoff)
#' df_hedged <- data_hedged_payoff(payoff, model, params)
#'
#' # Plot hedged cash flows
#' fig_cashflows_hedged_year(df_hedged)
#' fig_cashflows_hedged_year(df_hedged, 2018)
#' fig_cashflows_hedged_year(df_hedged, 2021)
#'
#' # Plot unhedged cash flows
#' fig_cashflows_unhedged_year(df_hedged)
#' fig_cashflows_unhedged_year(df_hedged, 2018)
#' fig_cashflows_unhedged_year(df_hedged, 2021)
#'
#' # Plot monthly mean and std. deviation of cash flows
#' fig_cashflows_monthly_moments(df_hedged)
#' fig_cashflows_monthly_moments(df_hedged, 2018)
#' fig_cashflows_monthly_moments(df_hedged, 2021)
#'
#' # Compare unhedged and hedged cash flows
#' fig_cashflows_unhedged_vs_hedged(df_hedged)
#' fig_cashflows_unhedged_vs_hedged(df_hedged, 2018)
#' fig_cashflows_unhedged_vs_hedged(df_hedged, 2021)
#'
#' # Compare unhedged and hedged cash flows (old version)
#' fig_cashflows_unhedged_vs_hedged_old(df_hedged)
#' fig_cashflows_unhedged_vs_hedged_old(df_hedged, 2018)
#' fig_cashflows_unhedged_vs_hedged_old(df_hedged, 2021)
#'
#' # Compare yearly density of unhedged and hedged cash flows
#' fig_cashflows_unhedged_vs_hedged_density_year(df_hedged)
#' fig_cashflows_unhedged_vs_hedged_density_year(df_hedged, 2018)
#' fig_cashflows_unhedged_vs_hedged_density_year(df_hedged, 2021)
#'
#' # Compare monthly density of unhedged and hedged cash flows
#' fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, 2020, 4)
#' fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, 2018, 1)
#' fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, 2021, 2)
#'
#' # Plot all monthly density of unhedged and hedged cash flows
#' fig_cashflows_unhedged_vs_hedged_density_year_months(df_hedged, 2018)
#'
#' @rdname fig_cashflows
#' @name fig_cashflows
#' @aliases fig_cashflows_hedged_year
#' @aliases fig_cashflows_unhedged_year
#' @export
fig_cashflows_hedged_year <- function(df_hedged, nyear){

  if (missing(nyear)){
    nyear <- attr(df_hedged, "nyear")
  }
  df_hedged_year <- dplyr::filter(df_hedged, Year == nyear)

  y_limits <- c(min(min(df_hedged_year$unhedged), min(df_hedged_year$hedged)), max(df_hedged_year$Ct))

  ggplot(df_hedged_year) +
    geom_line(aes(date, hedged), color = "orange") +
    geom_line(aes(date, Ct), color = "blue") +
    geom_line(aes(date, mean_prod), color = "black") +
    geom_point(aes(date, hedged), color = "black", size = 1.5) +
    geom_point(aes(date, hedged, color = col), size = 1) +
    scale_color_manual(values = c(up = "green", dw = "red"),
                       labels = c(up = "GHI above seasonal", dw = "GHI below seasonal")) +
    scale_y_continuous(limits = y_limits)+
    scale_x_date(date_labels = "%b")+
    labs(x = NULL, y = "Cash Flows (Eur)", color = NULL)+
    theme_bw()+
    theme(legend.position = "none")
}

#' @rdname fig_cashflows
#' @export
fig_cashflows_unhedged_year <- function(df_hedged, nyear){

  if (missing(nyear)){
    nyear <- attr(df_hedged, "nyear")
  }
  df_hedged_year <- dplyr::filter(df_hedged, Year == nyear)

  y_limits <- c(min(min(df_hedged_year$unhedged), min(df_hedged_year$hedged)), max(df_hedged_year$Ct))

  ggplot(df_hedged_year) +
    geom_line(aes(date, unhedged), color = "orange") +
    geom_line(aes(date, Ct), color = "blue") +
    geom_line(aes(date, mean_prod), color = "black") +
    geom_point(aes(date, unhedged), color = "black", size = 1.5) +
    geom_point(aes(date, unhedged, color = col), size = 1) +
    scale_color_manual(values = c(up = "green", dw = "red")) +
    scale_y_continuous(limits = y_limits)+
    scale_x_date(date_labels = "%b")+
    labs(x = NULL, y = "Cash Flows (Eur)")+
    theme_bw()+
    theme(legend.position = "none")
}

#' @rdname fig_cashflows
#' @export
fig_cashflows_monthly_moments <- function(df_hedged, nyear){

  if (missing(nyear)){
    nyear <- attr(df_hedged, "nyear")
  }
  df_hedged_month <- df_hedged %>%
    dplyr::filter(Year == nyear)%>%
    dplyr::group_by(Month) %>%
    dplyr::summarise(e_uh = mean(unhedged),
                     e_h = mean(hedged),
                     sd_uh = sd(unhedged),
                     sd_h = sd(hedged))

  plot_expectation <- df_hedged_month %>%
    mutate(type = "Monthly mean") %>%
    ggplot()+
    geom_line(aes(Month, e_uh, group = 1, color = "unhedged"))+
    geom_line(aes(Month, e_h, group = 1, color = "hedged"))+
    facet_wrap(~type)+
    labs(x = NULL, y = NULL)+
    scale_color_manual(values = c(unhedged = "black", hedged = "red")) +
    scale_x_continuous(breaks = c(1, 3, 6, 9, 12),
                       labels = lubridate::month(c(1, 3, 6, 9, 12), label = TRUE))+
    theme_bw()+
    theme(legend.position = "none")

  plot_std_dev <- df_hedged_month %>%
    mutate(type = "Monthly Std. Deviation") %>%
    ggplot()+
    geom_line(aes(Month, sd_uh, group = 1, color = "unhedged"))+
    geom_line(aes(Month, sd_h, group = 1, color = "hedged"))+
    facet_wrap(~type)+
    labs(x = NULL, y = NULL, color = NULL)+
    scale_color_manual(values = c(unhedged = "black", hedged = "red")) +
    scale_x_continuous(breaks = c(1, 3, 6, 9, 12),
                       labels = lubridate::month(c(1, 3, 6, 9, 12), label = TRUE))+
    theme_bw()+
    theme(legend.position = "none")

  gridExtra::grid.arrange(plot_expectation, plot_std_dev, ncol = 2, widths = c(0.5, 0.5))
}

#' @rdname fig_cashflows
#' @export
fig_cashflows_unhedged_vs_hedged <- function(df_hedged, nyear){

  fig_unhedged <- fig_cashflows_unhedged_year(df_hedged, nyear)
  fig_hedged <- fig_cashflows_hedged_year(df_hedged, nyear)+
    labs(x = NULL, y = NULL, color = NULL)

  gridExtra::grid.arrange(fig_unhedged, fig_hedged, ncol = 2)
}

#' @rdname fig_cashflows
#' @export
fig_cashflows_unhedged_vs_hedged_old <- function(df_hedged, nyear, subtitle = NULL){

  if (missing(nyear)){
    nyear <- attr(df_hedged, "nyear")
  }
  df_hedged_year <- dplyr::filter(df_hedged, Year == nyear)

  ggplot(df_hedged_year)+
    geom_segment(aes(x = n, xend = n, y = unhedged, yend = hedged, color = col), linewidth = 1, alpha = 0.5)+
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
    theme(legend.position = "none")
}

#' @rdname fig_cashflows
#' @export
fig_cashflows_unhedged_vs_hedged_density_year <- function(df_hedged, nyear, subtitle = NULL){

  if (missing(nyear)){
    nyear <- attr(df_hedged, "nyear")
  }

  df_hedged_year <- dplyr::filter(df_hedged, Year < nyear) %>%
    group_by(Year) %>%
    summarise(hedged = sum(hedged), unhedged = sum(unhedged))

  df <- df_hedged_year
  min_x <- min(c(min(df$hedged), min(df$unhedged)))
  min_x <- min_x - min_x*0.08
  max_x <- max(c(max(df$hedged), max(df$unhedged)))
  max_x <- max_x + max_x*0.08

  ker_h <- density(df$hedged, from = min_x, to = max_x, n = 200)
  ker_h$y <- ker_h$y/sum(ker_h$y)

  ker_uh <- density(df$unhedged, from = min_x, to = max_x, n = 200)
  ker_uh$y <- ker_uh$y/sum(ker_uh$y)

  ggplot()+
    geom_line(aes(ker_h$x, ker_h$y), color = "red", linetype = "solid") +
    geom_area(aes(ker_uh$x, ker_h$y), fill = "red", alpha = 0.3) +

    geom_line(aes(ker_uh$x, ker_uh$y), color = "black") +
    geom_area(aes(ker_uh$x, ker_uh$y), fill = "black", alpha = 0.3) +

    theme_bw()+
    labs(x = NULL, y = NULL, subtitle = subtitle)+
    scale_x_continuous(breaks = seq(min_x, max_x, length.out = 3))+
    theme(legend.position = "none")
}

#' @rdname fig_cashflows
#' @export
fig_cashflows_unhedged_vs_hedged_density_year_month <- function(df_hedged, nyear, nmonth = 1){

  if (missing(nyear)){
    nyear <- attr(df_hedged, "nyear")
  }

  df_hedged_year <- dplyr::filter(df_hedged, Year < nyear & Month == nmonth) %>%
    group_by(Year) %>%
    summarise(hedged = sum(hedged), unhedged = sum(unhedged))

  df <- df_hedged_year
  min_x <- min(c(min(df$hedged), min(df$unhedged)))
  min_x <- min_x - min_x*0.08
  max_x <- max(c(max(df$hedged), max(df$unhedged)))
  max_x <- max_x + max_x*0.08

  ker_h <- density(df$hedged, from = min_x, to = max_x, n = 200)
  ker_h$y <- ker_h$y/sum(ker_h$y)

  ker_uh <- density(df$unhedged, from = min_x, to = max_x, n = 200)
  ker_uh$y <- ker_uh$y/sum(ker_uh$y)

  ggplot()+
    geom_line(aes(ker_h$x, ker_h$y), color = "red", linetype = "solid") +
    geom_area(aes(ker_uh$x, ker_h$y), fill = "red", alpha = 0.3) +

    geom_line(aes(ker_uh$x, ker_uh$y), color = "black") +
    geom_area(aes(ker_uh$x, ker_uh$y), fill = "black", alpha = 0.3) +

    theme_bw()+
    labs(x = NULL, y = NULL, subtitle = subtitle)+
    scale_x_continuous(breaks = seq(min_x, max_x, length.out = 3))+
    theme(legend.position = "none")
}

#' @rdname fig_cashflows
#' @export
fig_cashflows_unhedged_vs_hedged_density_year_months <- function(df_hedged, nyear, nmonth = 1){

  fig_1 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 1)+labs(subtitle = lubridate::month(1, label = TRUE))
  fig_2 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 2)+labs(subtitle = lubridate::month(2, label = TRUE))
  fig_3 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 3)+labs(subtitle = lubridate::month(3, label = TRUE))
  fig_4 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 4)+labs(subtitle = lubridate::month(4, label = TRUE))
  fig_5 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 5)+labs(subtitle = lubridate::month(5, label = TRUE))
  fig_6 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 6)+labs(subtitle = lubridate::month(6, label = TRUE))
  fig_7 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 7)+labs(subtitle = lubridate::month(7, label = TRUE))
  fig_8 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 8)+labs(subtitle = lubridate::month(8, label = TRUE))
  fig_9 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 9)+labs(subtitle = lubridate::month(9, label = TRUE))
  fig_10 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 10)+labs(subtitle = lubridate::month(10, label = TRUE))
  fig_11 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 11)+labs(subtitle = lubridate::month(11, label = TRUE))
  fig_12 <- fig_cashflows_unhedged_vs_hedged_density_year_month(df_hedged, nyear, nmonth = 12)+labs(subtitle = lubridate::month(12, label = TRUE))

  gridExtra::grid.arrange(fig_1, fig_2, fig_3, fig_4, fig_5, fig_6,
                          fig_7, fig_8, fig_9, fig_10, fig_11, fig_12)


}

