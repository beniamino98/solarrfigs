#' @rdname fig_traj_sim
#' @examples
#' fig_traj_sim(Amsterdam$model$scenarios$P, pVaR = 0.05, nyear = 2021, nsim = 50, plot_nsim = 1, alpha = 1)
#' fig_traj_sim(Oslo$model$scenarios$P, pVaR = 0.02, nyear = 2010, nsim = 100, plot_nsim = 10, alpha = 0.05)
#' fig_traj_sim(Berlino$model$scenarios$P, pVaR = 0.02, nyear = 2010, nsim = 100, plot_nsim = 10, alpha = 0.05)
#' fig_traj_sim(Bologna$model$scenarios$P, pVaR = 0.02, nyear = 2010, nsim = 50, plot_nsim = 10, alpha = 0.5)
fig_traj_sim <- function(sim, nyear = NULL, pVaR = 0.05, nsim = 20, plot_nsim = 1, alpha = 0.3){

  #' @examples
  #' sim = Amsterdam$model$scenarios$P
  #' nyear = 2022
  #' nsim = 100
  #' pVaR = 0.05
  #' alpha = 0.3
  #' plot_nsim = 1
  df_emp <- sim$emp
  df_sim <- sim$sim
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

  # Compute Production at Risk
  df_PaR_emp <- df_emp %>%
    group_by(Month, Day) %>%
    reframe(
      e_GHI_emp = mean(GHI),
      q_var_emp = quantile(GHI, pVaR)
    )
  # Compute Production at Risk
  df_PaR_sim <- df_sim %>%
    group_by(Month, Day) %>%
    reframe(
      e_GHI_sim = mean(GHI),
      q_var_sim = quantile(GHI, pVaR)
    )

  df_PaR <- df_PaR_sim %>%
    right_join(df_emp, by = c("Month", "Day"))
  paR <- mean(df_PaR$GHI <= df_PaR$q_var_sim, na.rm = TRUE)*100
  print(paste0("Empiric production@Risk pVaR (", pVaR*100, "%): ", format(paste0(round(paR,2), "%"))))

  # Plot dataset
  df_plot_sim <- df_sim %>%
    left_join(select(df_PaR_sim, Month, Day, e_GHI_sim, q_var_sim), by = c("Month", "Day")) %>%
    left_join(select(df_PaR_emp, Month, Day, e_GHI_emp, q_var_emp), by = c("Month", "Day")) %>%
    group_by(date, place, Year, Month, Day, n) %>%
    nest()
  # Maximum number of scenarios for plot
  plot_nsim_max <- nrow(df_plot_sim$data[[1]])
  plot_nsim <- min(c(plot_nsim, plot_nsim_max))

  col_pVaR <- "red"
  col_expected_value_sim <- "magenta"
  col_expected_value_emp <- "black"

  df_plot_sim %>%
    mutate(data = map(data, ~.x[1:plot_nsim,])) %>%
    unnest(cols = c("data"))%>%
    ggplot()+
    # Empirical GHI values
    geom_line(aes(n, GHI, group = seed), alpha = alpha, size = 0.2, color = "orange")+
    geom_point(aes(n, GHI, group = seed), alpha = alpha, size = 1.3, color = "black")+
    geom_point(aes(n, GHI, group = seed), alpha = alpha, size = 0.8, color = "orange")+
    # pVaR (simulated)
    geom_line(aes(n, q_var_sim), color = col_pVaR, size = 0.5, linetype = "solid")+
    # pVaR (empiric)
    # geom_line(aes(n, q_var_emp, group = Year), color = col_pVaR, size = 0.5, linetype = "solid")+
    # Expected value (empiric)
    #geom_line(aes(n, e_GHI_emp, group = Year), color = col_expected_value_emp, size = 0.2)+
    # Expected value (simulated)
    geom_line(aes(n, e_GHI_sim, group = Year), color = col_expected_value_sim, size = 0.5)+
    # Seasonal mean
    geom_line(aes(n, GHI_bar), color = "black", size = 0.5)+
    # Clearsky radiation
    geom_line(aes(n, Ct, group = Year), size = 1, color = "blue")+
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
