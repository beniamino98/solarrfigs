#' Plot the autocorrelation function
#'
#' @param x time series on which it is computed the autocorrelation,
#' @param lag_max numeric, maximum number for the autocorrelation function.
#' @param label character, optional label for y-axis.
#' @param caption character, optional caption.
#' @param ci probability for confidence intervals.
#' @param ci_color character, color of the confidences intervals.
#' @examples
#' fig_acf(rnorm(5000), label = "\\tilde{Y}_t", ci_color = "red", lag_max = 100, limits = c(-0.1, 0.1))
#'
#' @rdname fig_acf
#' @name fig_acf
#' @export
fig_acf <- function(x, lag_max = 400, label = "x", caption = NULL, ci = 0.05, ci_color = "blue", limits){

  # residuals
  lag.index <- seq.default(1, lag_max, 1)
  x_breaks <- seq.default(1, lag_max, length.out = 5)

  # Autocorrelation ut
  acf_x <- acf(x, lag.max = lag_max, plot = FALSE)
  acf_x_bounds <- qnorm(1 - ci/2)/sqrt(acf_x$n.used)
  plot <- ggplot() +
    geom_segment(aes(x = lag.index, xend = lag.index,
                     y = acf_x$acf[,,1][-1],
                     yend = 0)) +
    geom_point(aes(x = lag.index, y = acf_x$acf[,,1][-1]), size = 0.2) +
    geom_line(aes(lag.index, 0)) +
    geom_line(aes(lag.index, acf_x_bounds), color = ci_color,
              linetype = "dashed") +
    geom_line(aes(lag.index, -acf_x_bounds), color = ci_color,
              linetype = "dashed") +
    scale_x_continuous(breaks = x_breaks,
                       labels = round(x_breaks)) +
    labs(x = NULL, y = latex2exp::TeX(paste0("$acf(", label, ")$"))) +
    theme_bw()

  if(!missing(limits)){
    plot <- plot +
      scale_y_continuous(limits = limits)
  }
  plot
}



