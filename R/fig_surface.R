#' Plot surface
#'
#' Plot a bi or tri dimensional grid where each square (or altitude) is filled
#' accordingly to the magnitude of a variable of interest.
#'
#' @param data dataset with variables `lat`, `lon` and the variable of interest specified in `target`.
#' @param solarModels a list of `solarModel` objects.
#' @param target character, column name of the target variable to plot.
#' @rdname fig_surface
#' @name fig_suface
#' @aliases fig_surface2D
#' @aliases fig_surface3D
#' @examples
#' fig_surface2D(sample_data$lat_lon_GHI)
#' fig_surface3D(sample_data$lat_lon_GHI)
#' @export
fig_surface2D <- function(data, target = "GHI", by = 0.1){

  df_plot <- data
  # Standard name for plotting
  df_plot[["target"]] <- df_plot[[target]]
  # Extract the variables of interest
  df_plot <- dplyr::select(df_plot, xmin = "lon", ymin = "lat", target)
  df_plot <- dplyr::mutate(df_plot, xmax = xmin + by, ymax = ymin + by)

  ggplot(df_plot)+
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = target))+
    scale_fill_gradient(low = "blue", high = "orange")+
    theme_bw()+
    labs(x = "Longitude", y = "Latitude", fill = target)+
    theme(legend.position = "top")
}

#' @rdname fig_surface
#' @export
fig_surface3D <- function(data, target = "GHI"){

  df_plot <- data
  # Standard name for plotting
  df_plot[["target"]] <- df_plot[[target]]
  # Extract the variables of interest
  df_plot <- dplyr::select(df_plot, lat, lon, target)
  # Compute Z
  df_plot_z <- df_plot %>%
    tidyr::spread(lat, target)
  # Coordinates
  x_coord <- as.numeric(colnames(df_plot_z)[-1])
  y_coord <- df_plot_z$lon
  z_coord <- as.matrix(dplyr::select(df_plot_z, -lon))
  # 3D Surface
  plotly::plot_ly(x=x_coord, y=y_coord, z=z_coord, type = "surface")
}


#' @rdname fig_surface
#' @export
fig_surface2D_solarModels <- function(solarModels, target = "GHI", nmonth = 1, date){
  locations <- purrr::map_df(solarModels, ~.x$coords)
  if (missing(date)){
    locations[[target]] <- purrr::map_dbl(solarModels, ~mean(dplyr::filter(.x$data, Month %in% nmonth)[[target]]))
  } else {
    day_date <- as.Date(date)
    locations[[target]] <- purrr::map_dbl(solarModels, ~dplyr::filter(.x$data, date == day_date)[[target]])
  }
  fig_surface2D(locations, target = target)
}

#' @rdname fig_surface
#' @export
fig_surface3D_solarModels <- function(solarModels, target = "GHI", nmonth = 1, date){

  locations <- purrr::map_df(solarModels, ~.x$coords)
  if (missing(date)){
    locations[[target]] <- purrr::map_dbl(solarModels, ~mean(dplyr::filter(.x$data, Month %in% nmonth)[[target]]))
  } else {
    day_date <- as.Date(date)
    locations[[target]] <- purrr::map_dbl(solarModels, ~dplyr::filter(.x$data, date == day_date)[[target]])
  }
  fig_surface3D(locations, target = target)
}
