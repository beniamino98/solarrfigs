#' Plot the time series of solar radiation
#'
#' Plot the time series of solar radiation with clear sky and seasonal radiation.
#'
#' @param model `solarModel` object
#' @param nyear reference year to plot.
#' @param line logical, when TRUE the GHI points will be connected with a line.
#' @param interval character vector, optional vector with start and end date for plotting.
#' @param size size of the GHI points.
#' @param alpha transparency for the GHI line and points.
#' @param labels logical, when `TRUE` will be added labels on x and y axis.
#' @param subtitle plot subtitle
#' @param seasonal_mean logical, when `TRUE` the seasonal mean will be plotted.
#' @param clearsky logical, when `TRUE` the seasonal clearsky will be plotted.
#' @param legend.position position of the legend
#' @param legend.justification justification of the legend.
#'
#' @examples
#' model <- SampleData$model
#' fig_ghi_year(model, nyear = 2019)
#' fig_ghi_year(model, line=FALSE, legend.position="none")
#' fig_ghi_year(model, nyear = 2019, seasonal_mean = FALSE)
#' fig_ghi_year(model, nyear = 2019, seasonal_mean = FALSE, clearsky = FALSE)
#' fig_ghi_year(model, interval = c("2020-04-01", "2020-09-22"))
#' fig_ghi_year(model, interval = c("2020-04-01", "2020-09-22"),
#' legend.position = "none", legend.justification = NULL, labels = TRUE)
#' @rdname fig_ghi_year
#' @name fig_ghi_year
#' @export
fig_ghi_year <- function(model, nyear = 2022, line = TRUE, interval = NA, size = 0.3, alpha = 0.5, labels = FALSE, subtitle = NULL,
                         seasonal_mean = FALSE, clearsky = TRUE, legend.position = c(.98, .98), legend.justification = c("right", "top")){

  require(backports)
  # Dataset uxwsed for plots
  data <- dplyr::select(model$data, date, Year, n, GHI, Ct, GHI_bar)
  # Filter for a custom period
  if (!is.na(interval[1])) {
    from_date <- as.Date(interval[1])
    to_date <- as.Date(interval[2])
    df_plot <- dplyr::filter(data, date >= from_date & date <= to_date)
  } else {
    df_plot <- dplyr::filter(data, Year %in% nyear)
  }

  plt <- ggplot(df_plot)
  # Optionally add clearsky radiation
  if (clearsky) {
    plt <- plt +
      geom_line(aes(n, Ct, color = "clearsky"), linewidth = 0.7)
  }
  # Connect GHI observations with a line
  if (line) {
    plt <- plt +
      geom_line(aes(n, GHI, color = "GHI"), linewidth = size, alpha = alpha)
  }
  # Base plot
  plt <- plt +
    geom_point(aes(n, GHI), size = size*3, color = "black", alpha = alpha)+
    geom_point(aes(n, GHI), size = size*2, color = "orange", alpha = alpha)

  # Optionally add seasonal mean radiation
  if (seasonal_mean) {
    plt <- plt +
      geom_line(aes(n, GHI_bar, color = "GHI_bar"), linewidth = 1)
  }

  # Output figure
  plt <- plt +
    scale_color_manual(values = c(GHI = "orange", clearsky = "blue", GHI_bar = "black"),
                       labels = c(GHI =  latex2exp::TeX("$GHI_t$"), clearsky = latex2exp::TeX("$C_t$"),
                                  GHI_bar = latex2exp::TeX("$\\bar{GHI}_t$"))) +
    scale_x_continuous(breaks = c(1, 92, 183, 274, 365))+
    scale_y_continuous(breaks = seq(0, 10, length.out = 6),
                       labels = format(seq(0, 10, length.out = 6), 2))+
    theme_bw()+
    theme(legend.position = legend.position,
          legend.justification = legend.justification,
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6))+
    labs(title = NULL, subtitle = subtitle, color = NULL, caption = NULL, x = NULL, y = NULL)

  # Optionally add labels on the x and y axis
  if (labels) {
    plt <-  plt+labs(x = "Day of the year (n)", y = latex2exp::TeX("$GHI\\;(kWh/m^2)$"))
  }
  plt
}


