#' Compare simulated and empirical densities
#'
#' @examples
#' scenario <- SampleData$scenarios$P
#' # Compare Monte Carlo pdf vs empirical pdf
#' fig_compare_sim_vs_emp_pdf(scenario, bins = 25)
#'
#' # Compare Monte Carlo pdf vs empirical pdf for every month
#' fig_compare_sim_vs_emp_pdf_monthly(scenario)
#'
#' # Compare Monte Carlo cdf vs empirical cdf
#' fig_compare_sim_vs_emp_cdf(scenario)
#'
#' @rdname fig_compare_sim_vs_emp
#' @name fig_compare_sim_vs_emp
#' @aliases fig_compare_sim_vs_emp_pdf
#' @aliases fig_compare_sim_vs_emp_cdf
#' @aliases fig_compare_sim_vs_emp_pdf_monthly
#' @export
fig_compare_sim_vs_emp_pdf <- function(scenario, nmonth = 1:12, nsim = 100, bins = 20){

  # Filter for the selected months
  df_sim <- dplyr::filter(scenario$sim, Month %in% nmonth)
  # Filter for the maximum number of simulation to use
  nsim_max <- nrow(scenario$sim$data[[1]])
  nsim <- min(c(nsim, nsim_max))
  df_sim$data <- purrr::map(df_sim$data, ~.x[1:nsim,])

  # Extract simulated values
  df_sim <- tidyr::unnest(df_sim, cols = "data")
  # Extract empiric values
  df_emp <- dplyr::filter(scenario$emp, Month %in% nmonth)

  # Minimum and maximum on empiric values
  min_x <- min(c(min(df_sim$GHI)))
  max_x <- max(c(max(df_sim$GHI)))
  breaks_x <- seq(min_x, max_x, length.out = bins)
  # Number of empiric years used
  nyears <- length(unique(df_emp$Year))
  # Kernel estimate on simulated values
  ker_sim <- density(df_sim$GHI, from = min_x, to = max_x)
  ker_sim$y <- ker_sim$y

  # Labels
  linetype_values <- c(sim = "solid", emp = "solid")
  linetype_labels <- c(sim = paste0(nsim, " simulations"), emp = paste0(nyears, " years (empiric)"))

  ggplot()+
    geom_histogram(aes(df_emp$GHI, after_stat(density)), breaks = breaks_x, bins = bins,
                   color = "black", size = 0.2, fill = "lightgray", alpha = 0.8)+
    geom_line(aes(ker_sim$x, ker_sim$y, linetype = "sim"), size = 1.1, color = "red", alpha = 1)+
    labs(subtitle = NULL, caption = NULL, x = NULL, y = NULL, linetype = NULL)+
    scale_linetype_manual(values = linetype_values, labels = linetype_labels)+
    theme_bw()+
    theme(legend.position = "none")
}


#' @rdname fig_compare_sim_vs_emp
#' @name fig_compare_sim_vs_emp
#' @export
fig_compare_sim_vs_emp_cdf <- function(scenario, nmonth = 1:12, nsim = 100){
  # Filter for the selected months
  df_sim <- dplyr::filter(scenario$sim, Month %in% nmonth)
  # Filter for the maximum number of simulation to use
  nsim_max <- nrow(scenario$sim$data[[1]])
  nsim <- min(c(nsim, nsim_max))
  df_sim$data <- purrr::map(df_sim$data, ~.x[1:nsim,])

  # Extract simulated values
  df_sim <- tidyr::unnest(df_sim, cols = "data")
  # Extract empiric values
  df_emp <- dplyr::filter(scenario$emp, Month %in% nmonth)

  # Minimum and maximum on empiric values
  min_x <- min(c(min(df_sim$GHI), min(df_emp$GHI)))
  max_x <- max(c(max(df_sim$GHI), max(df_emp$GHI)))
  # Number of empiric years used
  nyears <- length(unique(df_emp$Year))
  # Kernel estimate on simulated values
  ker_sim <- density(df_sim$GHI, from = min_x, to = max_x)
  ker_sim$y <- cumsum(ker_sim$y/sum(ker_sim$y))
  # Kernel estimate on empiric values
  ker_emp <- density(df_emp$GHI, from = min_x, to = max_x)
  ker_emp$y <- cumsum(ker_emp$y/sum(ker_emp$y))

  # Labels
  linetype_values <- c(sim = "solid", emp = "solid")
  linetype_labels <- c(sim = paste0(nsim, " simulations"), emp = paste0(nyears, " years (empiric)"))
  # x-axis
  x_breaks <- seq(min_x, max_x, length.out = 5)
  x_labels <- format(x_breaks, digits = 1)
  # y-axis
  y_breaks <- seq(0, 1, length.out = 5)
  y_labels <- paste0(format(y_breaks*100, digits = 2), " %")

  ggplot()+
    geom_line(aes(ker_emp$x, ker_emp$y, linetype = "emp"), color = "black", alpha = 1)+
    geom_line(aes(ker_sim$x, ker_sim$y, linetype = "sim"), size = 1.1, color = "red", alpha = 1)+
    labs(subtitle = NULL, caption = NULL, x = NULL, y = NULL, linetype = NULL)+
    scale_linetype_manual(values = linetype_values, labels = linetype_labels)+
    scale_x_continuous(breaks = x_breaks, labels = x_labels)+
    scale_y_continuous(breaks = y_breaks, labels = y_labels)+
    theme_bw()+
    theme(legend.position = "none")
}


#' @rdname fig_compare_sim_vs_emp
#' @name fig_compare_sim_vs_emp
#' @export
fig_compare_sim_vs_emp_pdf_monthly <- function(scenario, nsim = 100, bins = 17){

  plot_1 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 1, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(1, label = TRUE, abbr = FALSE))
  plot_2 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 2, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(2, label = TRUE, abbr = FALSE))
  plot_3 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 3, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(3, label = TRUE, abbr = FALSE))
  plot_4 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 4, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(4, label = TRUE, abbr = FALSE))
  plot_5 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 5, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(5, label = TRUE, abbr = FALSE))
  plot_6 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 6, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(6, label = TRUE, abbr = FALSE))
  plot_7 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 7, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(7, label = TRUE, abbr = FALSE))
  plot_8 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 8, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(8, label = TRUE, abbr = FALSE))
  plot_9 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 9, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(9, label = TRUE, abbr = FALSE))
  plot_10 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 10, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(10, label = TRUE, abbr = FALSE))
  plot_11 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 11, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(11, label = TRUE, abbr = FALSE))
  plot_12 <- fig_compare_sim_vs_emp_pdf(scenario, nmonth = 12, nsim = nsim, bins = bins)+labs(subtitle = lubridate::month(12, label = TRUE, abbr = FALSE))

  yleft = gridtext::richtext_grob("Density", rot = 90, vjust = 1, gp = grid::gpar(fontsize = 20))
  bottom = gridtext::richtext_grob(text = 'GHI', vjust = 0.25, gp = grid::gpar(fontsize = 20))
  gridExtra::grid.arrange(plot_1, plot_2, plot_3, plot_4,
                          plot_5, plot_6, plot_7, plot_8,
                          plot_9, plot_10, plot_11, plot_12, ncol = 4, nrow = 3, left = yleft, bottom = bottom)

}
