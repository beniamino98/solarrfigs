#' Plot a trajectory for solar radiation
#'
#' @param model An object of the class `solarModel`.
#' @param scenario An object of the class `solarScenario`.
#' @param type Character. Type of plot `sim` for scenarios and `emp` for empirical values.
#' @param VaR Numeric. Quantile for computing the Value at Risk.
#' @param nyear description
#' @param line Logical. When `TRUE` the radiation points will be connected with a line.
#' @param seasonal Logical. When `TRUE` the seasonal radiation will be plotted.
#' @param clearsky Logical. When `TRUE` the clearsky radiation will be plotted.
#' @param alpha Numeric. Transparency of the plot.
#' @param nsim Numeric. Number of simulations used to computed expectation and VaR.
#' @param plot_nsim Numeric. Number of simulations to plot.
#'
#' @examples
#' nyear = 2022
#' nsim = 100
#' VaR = 0.05
#' alpha = 0.3
#' plot_nsim = 1
#' line = FALSE
#' seasonal = TRUE
#' clearsky = TRUE
#' model <- solarModel$new(spec)
#' model$fit()
#' scenario <- solarScenario(model, from = "2005-01-01", to = "2023-01-01", nsim = 10)
#' fig_trajectory(model, scenario, type="emp", VaR = 0.1, nsim = 50, plot_nsim = 1, alpha = 1)
#' fig_trajectory(model, scenario, type="sim", VaR = 0.05, nsim = 50, plot_nsim = 1, alpha = 1)
#'
#' @name fig_trajectory
#' @rdname fig_trajectory
#' @export
fig_trajectory <- function(model, scenario, type = c("emp", "sim"), VaR = 0.05, nyear = NULL,
                           line = FALSE, seasonal = TRUE, clearsky = TRUE,
                           alpha = 0.05, nsim = 200, plot_nsim = 1){

  # Color settings
  col_pVaR <- "red"
  col_expected_value_sim <- "magenta"
  col_expected_value_emp <- "black"
  # Match type of trajectory
  type = match.arg(type, choices = c("emp", "sim"))
  # Extract simulated data
  if (!missing(nyear)){
    df_sim <- dplyr::filter(scenario$sim, Year %in% nyear)
    scenario$sim <- dplyr::filter(scenario$sim, Year %in% nyear)
    #scenario$emp <- dplyr::filter(scenario$emp, Year %in% nyear)
  } else {
    df_sim <- scenario$sim
  }
  # Maximum number of scenarios available
  nsim_max <- nrow(df_sim$data[[1]])
  # Number of scenarios used for computations
  nsim <- min(c(nsim, nsim_max))
  # Extract scenarios
  df_sim$data <- purrr::map(df_sim$data, ~.x[1:nsim,])
  scenario$sim$data <- df_sim$data
  df_sim <- tidyr::unnest(df_sim, cols = c("data"))
  df_sim <- dplyr::select(df_sim, date, Year, Month, Day, n, seed, GHI, GHI_bar)
  # Simulated VaR
  data_VaR_sim <- solarScenario_VaR(scenario, alpha = VaR)
  # Compute the average
  df_sim <- df_sim %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
      e_GHI_sim = mean(GHI)
    ) %>%
    dplyr::left_join(
      dplyr::select(data_VaR_sim$data, date, VaR_sim = "VaR_alpha"),
      by = "date"
    )

  # Extract empiric data
  df_emp <- dplyr::select(scenario$emp, date, Year, Month, Day, n, GHI)
  df_emp <- dplyr::left_join(df_emp, dplyr::select(model$data, date, Ct, GHI_bar), by = c("date"))
  # Compute empiric Value at Risk
  df_emp <- df_emp %>%
    dplyr::group_by(Month, Day) %>%
    dplyr::mutate(
      e_GHI_emp = mean(GHI),
      VaR_emp = quantile(GHI, probs = VaR)
    )

  # Check that P@R is consistent with the specified `VaR`.
  check_VaR <- dplyr::right_join(data_VaR_sim$data, df_emp, by = c("date"))
  VaR_sim <- mean(check_VaR$GHI < check_VaR$VaR_alpha, na.rm = TRUE)*100
  print(paste0("Simulated VaR with alpha ", VaR*100, "%: ", format(paste0(round(VaR_sim,2), "%"))))
  VaR_emp <- mean(df_emp$GHI < df_emp$VaR_emp, na.rm = TRUE)*100
  print(paste0("Empirical VaR with alpha ", VaR*100, "%: ", format(paste0(round(VaR_emp,2), "%"))))
  # Filter for years
  if (!is.null(nyear)) {
    df_emp <- dplyr::filter(df_emp, Year %in% nyear)
    df_sim <- dplyr::filter(df_sim, Year %in% nyear)
  }
  # Plot dataset
  if (type == "sim") {
    df_plot <- df_sim %>%
      dplyr::group_by(Year, Month, Day, n) %>%
      tidyr::nest()
    # Maximum number of scenarios for plot
    plot_nsim_max <- nrow(df_plot$data[[1]])
    plot_nsim <- min(c(plot_nsim, plot_nsim_max))
    df_plot <-  df_plot  %>%
      dplyr::mutate(data = purrr::map(data, ~.x[1:plot_nsim,])) %>%
      tidyr::unnest(cols = c("data"))
  } else {
    df_plot <- df_emp
  }

  plt <- ggplot(df_plot)
  if (line){
    if (type == "sim") {
      plt <- plt +
        # Empirical GHI values
        geom_line(aes(n, GHI, group = seed), alpha = alpha, linewidth = 0.2, color = "orange")
    } else {
      plt <- plt +
        # Empirical GHI values
        geom_line(aes(n, GHI, group = Year), alpha = alpha, linewidth = 0.2, color = "orange")
    }
  }

  if (type == "sim") {
    plt <- plt +
      # Empirical GHI values
      geom_point(aes(n, GHI, group = seed), alpha = alpha, size = 1.4, color = "black")+
      geom_point(aes(n, GHI, group = seed), alpha = alpha, size = 0.7, color = "orange")+
      # pVaR (simulated)
      geom_line(aes(n, VaR_sim), color = col_pVaR, linewidth = 0.3, linetype = "solid")+
      geom_point(aes(n, VaR_sim), color = col_pVaR, size = 0.6)+
      geom_line(aes(n, e_GHI_sim, group = Year), color = col_expected_value_sim, linewidth = 0.5)
  } else {
    plt <- plt +
      # Empirical GHI values
      geom_point(aes(n, GHI, group = Year), alpha = alpha, size = 1.4, color = "black")+
      geom_point(aes(n, GHI, group = Year), alpha = alpha, size = 0.7, color = "orange")+
      # pVaR (simulated)
      geom_line(aes(n, VaR_emp), color = col_pVaR, linewidth = 0.3, linetype = "solid")+
      geom_point(aes(n, VaR_emp), color = col_pVaR, size = 0.6)+
      geom_line(aes(n, e_GHI_emp, group = Year), color = col_expected_value_sim, linewidth = 0.5)
  }
  # Seasonal mean
  if (seasonal) {
    plt <- plt +
      geom_line(aes(n, GHI_bar), color = "black", linewidth = 0.5)
  }

  # Clearsky radiation
  if (clearsky) {
    plt <- plt +
      geom_line(data = df_emp[1:365,], aes(n, Ct, group = Year), linewidth = 0.7, color = "blue")
  }

  plt +
    labs(subtitle = NULL, caption = NULL, x = "Number of the day", y = latex2exp::TeX("$R_t$"))+
    scale_x_continuous(breaks = c(1, 92, 183, 274, 365))+
    scale_y_continuous(breaks = seq(0, 10, length.out = 6),
                       labels = format(seq(0, 10, length.out = 6), 2))+
    theme_bw()+
    theme(legend.position = "none")
}
