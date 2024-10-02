#' Grid neighborhoods
#' @param location locations dataset
#' @param best_locations neighborhoods dataset.
#' @rdname fig_grid_neighborhoods
#' @export
fig_grid_neighborhoods <- function(location, best_locations){
  ggplot(locations)+
    geom_point(aes(lon, lat))+
    geom_segment(data = best_locations, aes(x = lon, xend = lon_B, y = lat, yend = lat_B, color = "dist"))+
    geom_point(data = best_locations, aes(lon_B, lat_B, color = "interp"))+
    geom_point(data = best_locations, aes(lon, lat, color = "nb"))+
    scale_color_manual(values = c(nb = "magenta", interp = "red", dist = "blue"),
                       labels = c(nb = "Neighborhoods", dist = "Distance", interp = "Interpolated location"))+
    theme_bw()+
    labs(x = "Longitude", y = "Latitude", color = NULL)+
    theme(legend.position = "top")
}
