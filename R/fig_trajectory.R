#' @examples
#' nyear = 2022
#' nsim = 100
#' pVaR = 0.05
#' alpha = 0.3
#' plot_nsim = 1
#' model <- location$model
#' scenario <- location$scenarios$P
#' fig_trajectory(model, scenario, type="emp", pVaR = 0.05, nyear = 2021, nsim = 50, plot_nsim = 1, alpha = 1)
#' fig_trajectory(model, scenario, type="sim", pVaR = 0.05, nyear = 2021, nsim = 50, plot_nsim = 1, alpha = 1)
#'
#' @name fig_trajectory
#' @rdname fig_trajectory
#' @export
fig_trajectory <- function(model, scenario, type = c("emp", "sim"), nyear = NULL, pVaR = 0.05, nsim = 20, plot_nsim = 1, alpha = 0.3){

  # Color settings
  col_pVaR <- "red"
  col_expected_value_sim <- "magenta"
  col_expected_value_emp <- "black"
  # Match type of trajectory
  type = match.arg(type, choices = c("emp", "sim"))

  # Extract empiric data
  df_emp <- dplyr::select(scenario$emp, place, date, Year, Month, Day, n, GHI)
  df_emp <- dplyr::left_join(df_emp,
                             dplyr::select(model$seasonal_data, Month, Day, Ct, GHI_bar),
                             by = c("Month", "Day"))

  # Extract simulated data
  df_sim <- scenario$sim
  # Maximum number of scenarios available
  nsim_max <- nrow(df_sim$data[[1]])
  # Number of scenarios used for computations
  nsim <- min(c(nsim, nsim_max))
  # Filter for years
  if (!is.null(nyear)) {
    df_sim <- dplyr::filter(df_sim, Year %in% nyear)
  }
  # Extract scenarios
  df_sim$data <- purrr::map(df_sim$data, ~.x[1:nsim,])
  df_sim <- tidyr::unnest(df_sim, cols = c("data"))
  df_sim <-  dplyr::select(df_sim, place,  date, Year, Month, Day, n, seed, GHI, GHI_bar)

  # Compute simulated Production at Risk
  df_PaR_sim <- df_sim %>%
    dplyr::group_by(Month, Day) %>%
    dplyr::reframe(
      e_GHI_sim = mean(GHI),
      q_var_sim = quantile(GHI, pVaR)
    )

  # Compute simulated Production at Risk
  df_PaR_emp <- df_emp %>%
    dplyr::group_by(Month, Day) %>%
    dplyr::reframe(
      e_GHI_emp = mean(GHI),
      q_var_emp = quantile(GHI, pVaR)
    )

  # Filter for years
  if (!is.null(nyear)) {
    df_emp <- dplyr::filter(df_emp, Year %in% nyear)
  }

  # Check that P@R is consistent with the specified `pVaR`.
  df_PaR <- dplyr::right_join(df_PaR_sim, df_emp, by = c("Month", "Day"))
  paR <- mean(df_PaR$GHI <= df_PaR$q_var_sim, na.rm = TRUE)*100
  print(paste0("Empiric production@Risk pVaR (", pVaR*100, "%): ", format(paste0(round(paR,2), "%"))))

  # Plot dataset
  if (type == "sim") {
    df_plot <- df_sim %>%
      dplyr::left_join(dplyr::select(df_PaR_sim, Month,Day, e_GHI_sim, q_var_sim), by = c("Month", "Day")) %>%
      dplyr::group_by(place, Year, Month, Day, n) %>%
      tidyr::nest()
    # Maximum number of scenarios for plot
    plot_nsim_max <- nrow(df_plot$data[[1]])
    plot_nsim <- min(c(plot_nsim, plot_nsim_max))
    df_plot <-  df_plot  %>%
      dplyr::mutate(data = purrr::map(data, ~.x[1:plot_nsim,])) %>%
      tidyr::unnest(cols = c("data"))
  } else {
    df_plot <- df_emp %>%
      dplyr::left_join(dplyr::select(df_PaR_sim, Month, Day, e_GHI_sim, q_var_sim), by = c("Month", "Day")) %>%
      dplyr::left_join(dplyr::select(df_PaR_emp, Month, Day, e_GHI_emp, q_var_emp), by = c("Month", "Day"))
  }

  plt <- ggplot(df_plot)
  if (type == "sim") {
    plt <- plt +
      # Empirical GHI values
      geom_line(aes(n, GHI, group = seed), alpha = alpha, linewidth = 0.2, color = "orange")+
      geom_point(aes(n, GHI, group = seed), alpha = alpha, size = 1.4, color = "black")+
      geom_point(aes(n, GHI, group = seed), alpha = alpha, size = 0.7, color = "orange")+
      # pVaR (simulated)
      geom_line(aes(n, q_var_sim), color = col_pVaR, linewidth = 0.3, linetype = "solid")+
      geom_point(aes(n, q_var_sim), color = col_pVaR, size = 0.6)+
      geom_line(aes(n, e_GHI_sim, group = Year), color = col_expected_value_sim, linewidth = 0.5)
  } else {
    plt <- plt +
      # Empirical GHI values
      geom_line(aes(n, GHI, group = Year), alpha = alpha, linewidth = 0.2, color = "orange")+
      geom_point(aes(n, GHI, group = Year), alpha = alpha, size = 1.4, color = "black")+
      geom_point(aes(n, GHI, group = Year), alpha = alpha, size = 0.7, color = "orange")+
      # pVaR (simulated)
      geom_line(aes(n, q_var_emp), color = col_pVaR, linewidth = 0.3, linetype = "solid")+
      geom_point(aes(n, q_var_emp), color = col_pVaR, size = 0.6)+
      geom_line(aes(n, e_GHI_emp, group = Year), color = col_expected_value_sim, linewidth = 0.5)
  }

  plt +
    # Seasonal mean
    geom_line(aes(n, GHI_bar), color = "black", linewidth = 0.5)+
    # Clearsky radiation
    geom_line(data = df_emp[1:365,], aes(n, Ct, group = Year), size = 0.7, color = "blue")+
    labs(subtitle = NULL, caption = NULL, x = "Number of the day (n)", y = "GHI")+
    scale_x_continuous(breaks = c(1, 92, 183, 274, 365))+
    scale_y_continuous(breaks = seq(0, 10, length.out = 6),
                       labels = format(seq(0, 10, length.out = 6), 2))+
    theme_bw()+
    theme(legend.position = "none",
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.ticks = element_line(size = 0.2),
          plot.subtitle = element_text(size = 24),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "black", linetype = "dashed", linewidth = 0.1),
          panel.grid.major.x = element_blank())
}

