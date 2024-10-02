#' Plot the time series of solar radiation
#'
#' Plot the time series of solar radiation with clear sky and seasonal radiation.
#'
#' @param model `solarModel` object
#' @param nyear
#' @param size
#' @param alpha
#' @param labels
#' @param subtitle
#'
#' @examples
#' library(solarr)
#' model <- Bologna
#' fig_ghi_year(model, nyear = 2019)
#' fig_ghi_year(model, nyear = 2019, seasonal_mean = FALSE)
#' fig_ghi_year(model, nyear = 2019, seasonal_mean = FALSE, clearsky = FALSE)
#' fig_ghi_year(model, interval = c("2020-04-01", "2020-09-22"))
#' fig_ghi_year(model, interval = c("2020-04-01", "2020-09-22"),
#' legend.position = "none", legend.justification = NULL)
#' @rdname fig_ghi_year
#' @name fig_ghi_year
#' @export
fig_ghi_year <- function(model, nyear = 2022, interval = NA, size = 0.3, alpha = 0.5, labels = FALSE, subtitle = NULL,
                         legend.position = c(.98, .98), seasonal_mean = TRUE, clearsky = TRUE,
                         legend.justification = c("right", "top")){

  data <- dplyr::left_join(model$data, model$seasonal_data, by = c("Month", "Day"))
  if (!is.na(interval[1])) {
    from_date <- as.Date(interval[1])
    to_date <- as.Date(interval[2])
    df_plot <- dplyr::filter(data, date >= from_date & date <= to_date)
  } else {
    df_plot <- dplyr::filter(data, Year %in% nyear)
  }

  plt <- ggplot(df_plot)+
    # GHI
    geom_line(aes(n, GHI, color = "GHI"), linewidth = size, alpha = alpha)+
    geom_point(aes(n, GHI), size = size*3, color = "black", alpha = alpha)+
    geom_point(aes(n, GHI), size = size*2, color = "orange", alpha = alpha)

  # Seasonal mean radiation
  if (seasonal_mean) {
    plt <- plt +
      geom_line(aes(n, GHI_bar, color = "GHI_bar"), linewidth = 1)
  }
  # Clearsky radiation
  if (clearsky) {
    plt <- plt +
      geom_line(aes(n, Ct, color = "clearsky"), linewidth = 1)
  }

  plt <- plt +
    scale_color_manual(values = c(GHI = "orange", clearsky = "blue", GHI_bar = "black"),
                       labels = c(GHI = TeX("$GHI_t$"), clearsky = TeX("$C_t$"),
                                  GHI_bar = TeX("$\\bar{GHI}_t$"))) +
    scale_x_continuous(breaks = c(1, 92, 183, 274, 365))+
    scale_y_continuous(breaks = seq(0, 10, length.out = 6),
                       labels = format(seq(0, 10, length.out = 6), 2))+
    theme_bw()+
    theme(legend.position = legend.position,
          legend.justification = legend.justification,
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          plot.subtitle = element_text(size = 24),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.ticks = element_line(linewidth = 0.2),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = "black", linetype = "dashed", linewidth = 0.1),
          panel.grid.major.x = element_blank())+
    labs(title = NULL, subtitle = subtitle, color = NULL, caption = NULL)

  if (labels) {
    plt <-  plt+labs(x = "Day of the year (n)", y = "GHI")
  }
  return(plt)
}


